(* This file is part of the grep_cmt package *)
(* See the attached LICENSE file.            *)
(* Copyright (C) 2000-2024 LexiFi            *)

open Asttypes
open Parsetree
open Typedtree
open Longident

let initial_cwd = Sys.getcwd ()

let drop_prefix ~prefix s =
  if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let read_lines fn =
  String.split_on_char '\n' (In_channel.with_open_text fn In_channel.input_all)

let memoize h f k =
  match Hashtbl.find_opt h k with
  | None -> let r = f k in Hashtbl.add h k r; r
  | Some r -> r

let build_root, build_prefix =
  let rec loop prefix dir =
    if Sys.file_exists (Filename.concat dir "_build") then
      let absdir =
        Fun.protect ~finally:(fun () -> Sys.chdir initial_cwd)
          (fun () -> Sys.chdir dir; Sys.getcwd ())
      in
      absdir, prefix
    else
      let dir' = Filename.dirname dir in
      if dir' = dir then failwith "Could not detect _build";
      loop (Filename.concat (Filename.basename dir) prefix) dir'
  in
  loop "" initial_cwd

let in_build_dir path =
  Filename.concat build_root (Filename.concat "_build" (Filename.concat "default" path))

type color =
  | Yellow
  | Red
  | Green

let color c fmt =
  Printf.sprintf ("\027[1;%dm" ^^ fmt ^^ "\027[0m") (match c with Yellow -> 33 | Red -> 31 | Green -> 32)

let print_results_with_color_range i c1 c2 s file_color =
  let i_color = color Yellow "%d" i in
  let s_color =
    let len = String.length s in
    if c2 > len || c1 > len then
      Printf.sprintf
        " Skipping this line with wrong indexes -- Maybe you should think about recompiling this file."
    else
      String.sub s 0 c1 ^
      color Red "%s" (String.sub s c1 (c2-c1)) ^
      String.sub s c2 (String.length s - c2)
  in
  Printf.printf "%s:%s:%s\n%!" file_color i_color s_color

(*** Structured search ***)

exception DontMatch

let wildcards = ref []

(* wildcards are in the form __123 ie.
   the two first characters are underscores;
   the following characters are digits.
*)
let is_wildcard str =
  String.length str > 2 && str.[0] = '_' && str.[1] = '_' &&
  let r = ref true in
  for i = 2 to String.length str - 1 do
    match str.[i] with
    | '0'..'9' -> ()
    | _ -> r := false;
  done;
  !r

let check_wildcard id e =
  try
    let e' = List.assoc id !wildcards in
    if e <> e' then raise DontMatch
  with Not_found -> wildcards := (id, e) :: !wildcards

let check_wildcard_lid id lid =
  let e = Ast_helper.Exp.ident (mknoloc lid) in
  check_wildcard id e

let try_match f x =
  let w = !wildcards in
  try f x; true
  with DontMatch -> wildcards := w; false

let one_of f l =
  if not (List.exists (fun x -> try_match f x) l) then raise DontMatch

let match_set f ts ps =
  let ok = Hashtbl.create 8 in
  let f t p = f t p; Hashtbl.add ok p () in
  List.iter (fun t -> one_of (f t) ps) ts;
  List.iter (fun p -> if not (Hashtbl.mem ok p) then raise DontMatch) ps

let rec path_matches_lident p l =
  match p, l with
  | _, Lident "__" ->
      true
  | Path.Pdot (p0, s1), Ldot (l0, s2) when s1 = s2 || s2 = "__" ->
      path_matches_lident p0 l0
  | Path.Pdot (_, s1), Lident s2 when s1 = s2 ->
      true  (* the longident can be a suffix of the path *)
  | Path.Pident id, Lident s ->
      Ident.name id = s
  | _ ->
      false

let rec constructor_match t p =
  match t, p with
  | _, Lident "__" -> ()
  | _, Lident s when is_wildcard s -> check_wildcard_lid s t
  | Lident s1, Lident s2 when s1 = s2 -> ()
  | Ldot (_, s1), Lident s2 when s1 = s2 -> () (* the ident can be a suffix *)
  | Lident s1, Ldot (_, s2) when s1 = s2 -> ()
  | Ldot (t, s1), Ldot (p, s2) when s1 = s2 -> constructor_match t p
  | _ -> raise DontMatch

let remove_loc =
  let super = Ast_mapper.default_mapper in
  {super with location = (fun _ _ -> Location.none);
              attributes = (fun _ _ -> [])}

let match_opt f t p =
  match t, p with
  | None, None -> ()
  | Some _, None | None, Some _ -> raise DontMatch
  | Some t, Some p -> f t p

let match_list f t p =
  if List.compare_lengths t p = 0 then List.iter2 f t p
  else raise DontMatch

exception Cannot_parse_type of exn

let initial_env = lazy (Compmisc.initial_env ())

let parse_type t =
  let env = Lazy.force initial_env in
  try (Typetexp.transl_type_scheme env t).ctyp_type
  with e -> raise (Cannot_parse_type e)

let parse_type = memoize (Hashtbl.create 10) parse_type

let tconstant_equal_pconst tconst pconst =
  match Typecore.constant pconst with
  | Error _ -> false
  | Ok pconst -> Parmatch.const_compare tconst pconst = 0

let rec match_expr texpr pexpr =
  if texpr.exp_loc.loc_ghost && not pexpr.pexp_loc.loc_ghost
  then raise DontMatch;

  match texpr.exp_desc, pexpr.pexp_desc with
  (* __ matches any expression *)
  | _, Pexp_ident {txt=Lident "__"; _} ->
      ()

  (* __1234 matches any expression, and checks equality *)
  | _, Pexp_ident {txt=Lident id; _} when is_wildcard id ->
      let e = remove_loc.expr remove_loc (Untypeast.(default_mapper.expr default_mapper texpr)) in
      check_wildcard id e

  | Texp_ident (path, _, _), Pexp_ident {txt=lid; _}
    when path_matches_lident path lid ->
      ()

  | Texp_tuple texprs, Pexp_tuple pexprs
  | Texp_array texprs, Pexp_array pexprs ->
      match_exprs texprs pexprs

  | Texp_constant tconst, Pexp_constant pconst when tconstant_equal_pconst tconst pconst ->
      ()

  | Texp_apply (tapply_expr, targs), Pexp_apply (pexpr, pargs) ->
      match_expr tapply_expr pexpr;
      let rec check_all targs = function
        | [] -> () (* ok if more arguments in the typed expression *)
        | (Asttypes.Optional _ as lab, {pexp_desc=Pexp_construct({txt=Lident ("MISSING"|"PRESENT" as cstr); _}, None); _}) :: pargs ->
            let pr = cstr = "PRESENT" in
            let rec loop = function
              | [] -> raise DontMatch
              | (l, Some targ) :: targs when l = lab ->
                  if pr = targ.exp_loc.loc_ghost then raise DontMatch;
                  targs
              | x :: targs -> x :: loop targs
            in
            check_all (loop targs) pargs

        | (lab, parg) :: pargs ->
            let rec loop = function
              | [] -> raise DontMatch
              | (l, Some targ) :: targs when l = lab ->
                  match_expr targ parg; targs
              | (Asttypes.Optional _ as l, Some {exp_desc=Texp_construct({txt=Lident "Some"; _}, _, [targ]); _}) :: targs when l = lab ->
                  match_expr targ parg; targs
              | x :: targs -> x :: loop targs
            in
            check_all (loop targs) pargs
      in
      check_all targs pargs
  | Texp_function ([{fp_arg_label = Nolabel; _}], Tfunction_cases {cases = tcases; _}), Pexp_function ([{pparam_desc = Pparam_val (Nolabel, None, _); _}], _, Pfunction_cases (pcases, _, _)) ->
      match_cases tcases pcases

  | Texp_construct (tcstr, _tconstr_desc, texprs), Pexp_construct (pcstr, pexpr_opt) ->
      constructor_match tcstr.txt pcstr.txt;
      begin match pexpr_opt, texprs with
      | Some {pexp_desc = Pexp_ident {txt = Lident "__"; _}; _}, _ -> ()
      | None, [] -> ()
      | Some {pexp_desc = Pexp_tuple pexprs; _}, _ :: _ :: _ ->
          match_exprs texprs pexprs
      | Some pexpr, [ texpr ] -> match_expr texpr pexpr
      | _ -> raise DontMatch
      end

  | Texp_variant (tl, te), Pexp_variant (pl, pe) when tl = pl ->
      match_opt match_expr te pe

  | Texp_match (te, tcases, _), Pexp_match (pe, pcases) ->
      match_expr te pe;
      match_cases tcases pcases

  | Texp_try (te, tcases), Pexp_try (pe, pcases) ->
      match_expr te pe;
      match_cases tcases pcases

  | Texp_let (trf, tvb, te), Pexp_let (prf, pvb, pe) when trf = prf ->
      match_expr te pe;
      match_value_bindings tvb pvb

  | Texp_ifthenelse (te1, te2, te3), Pexp_ifthenelse (pe1, pe2, pe3) ->
      match_expr te1 pe1;
      match_expr te2 pe2;
      match_opt match_expr te3 pe3

  | Texp_sequence (te1, te2), Pexp_sequence (pe1, pe2)
  | Texp_while (te1, te2), Pexp_while (pe1, pe2) ->
      match_expr te1 pe1;
      match_expr te2 pe2

  | Texp_assert (te, _), Pexp_assert pe
  | Texp_lazy te, Pexp_lazy pe ->
      match_expr te pe

  | Texp_field (texpr, tid, _), Pexp_field (pexpr, pid)  ->
      constructor_match tid.txt pid.txt;
      match_expr texpr pexpr

  | Texp_setfield (te1, tid, _, te2), Pexp_setfield (pe1, pid, pe2) ->
      constructor_match tid.txt pid.txt;
      match_expr te1 pe1;
      match_expr te2 pe2

  | Texp_setfield (te1, tid, _, _), Pexp_field (pexpr, pid) ->
      constructor_match tid.txt pid.txt;
      match_expr te1 pexpr

  | _, Pexp_constraint (pe, pt) ->
      match_expr texpr pe;
      if not (match_typ texpr.exp_type pt) then raise DontMatch

  | Texp_record {fields = tfields; extended_expression = tdef; _}, Pexp_record (pfields, pdef) ->
      match_opt match_expr tdef pdef;
      let f (tid, _, te) (pid, pe) =
        constructor_match tid.txt pid.txt;
        match_expr te pe
      in
      let tfields = List.filter_map (function
          | (_, Kept _) -> None
          | (lbl, Overridden (id, e)) -> Some (id, lbl, e)
        ) (Array.to_list tfields)
      in
      match_set f tfields pfields

  | Texp_send (te, Tmeth_name ts), Pexp_send (pe, {txt = ps; _}) when ts = ps ->
      match_expr te pe
  | Texp_send (te, Tmeth_val id), Pexp_send (pe, {txt = ps; _}) when Ident.name id = ps ->
      match_expr te pe

  | Texp_new (path, _, _), Pexp_new lid when path_matches_lident path lid.txt ->
      ()

  | Texp_for (tident, patident, texpr1, texpr2, tdir_flag, texpr), Pexp_for (pident, pexpr1, pexpr2, pdir_flag, pexpr)  when tdir_flag = pdir_flag ->
      begin match patident.ppat_desc, pident.ppat_desc with
      | Ppat_any, Ppat_any -> ()
      | Ppat_any, Ppat_var {txt = "__"; loc = _} -> ()
      | Ppat_var {txt; loc = _}, Ppat_any when String.starts_with ~prefix:"_" txt -> ()
      | Ppat_var _, Ppat_var {txt; loc = _} when path_matches_lident (Path.Pident tident) (Longident.Lident txt) -> ()
      | _ -> raise DontMatch
      end;
      match_expr texpr1 pexpr1;
      match_expr texpr2 pexpr2;
      match_expr texpr pexpr;

  | _ ->
      raise DontMatch

and match_typ texpr ptyp =
  match parse_type ptyp with
  | typ ->
      let env = Lazy.force initial_env in
      begin try Ctype.is_moregeneral env false typ texpr
      with Assert_failure _ ->
        (* When dealing with inline records [moregeneral] above calls
           [Env.find_type_full] in a context that should never occur in the
           typechecker which causes assert false to be thrown. *)
        false
      end
  | exception _ ->
      begin match Types.get_desc texpr, ptyp.Parsetree.ptyp_desc with
      | Tconstr (path, ty_args, _), Ptyp_constr ({Location.txt; loc = _}, pty_args) ->
          if path_matches_lident path txt then begin
            match pty_args with
            | [{ptyp_desc = Ptyp_constr({Location.txt = Lident "__"; loc = _}, []); _}] -> true
            | _ ->
                if List.length ty_args = List.length pty_args then
                  List.for_all2 match_typ ty_args pty_args
                else false
          end else false
      | _ -> false
      end

and match_pat : type k. k general_pattern -> _ -> _ = fun tpat ppat ->
  match tpat.pat_desc, ppat.ppat_desc with
  | Tpat_any, Ppat_any -> ()
  | _, Ppat_var {txt = "__"; _} -> ()
  | Tpat_var (_, {txt = s1; _}, _), Ppat_var {txt = s2; _} when is_wildcard s2 ->
      check_wildcard_lid s2 (Lident s1)
  | Tpat_var (_, {txt = s1; _}, _), Ppat_var {txt = s2; _} when s1 = s2 -> ()
  | Tpat_tuple tl, Ppat_tuple pl -> match_list match_pat tl pl
  | Tpat_constant tc, Ppat_constant pc when tconstant_equal_pconst tc pc -> ()
  | Tpat_construct (tcstr, _tconstr_desc, tpats, _), Ppat_construct (pcstr, ppat_opt) ->
      constructor_match tcstr.txt pcstr.txt;
      begin match ppat_opt, tpats with
      | None, [] -> ()
      | Some (_, {ppat_desc = Ppat_tuple ppats; _}), _ :: _ :: _ ->
          match_list match_pat tpats ppats
      | Some (_, ppat), [ tpat ] -> match_pat tpat ppat
      | _ -> raise DontMatch
      end
  | _, Ppat_constraint (ppat, pt) ->
      match_pat tpat ppat;
      let pt = parse_type pt in
      let env = Lazy.force initial_env in
      let eq = Ctype.is_moregeneral env false pt tpat.pat_type in
      if not eq then raise DontMatch
  | Tpat_or (t1, t2, _), Ppat_or (p1, p2) ->
      match_pat t1 p1;
      match_pat t2 p2
  | Tpat_value t, _ ->
      match_pat (t :> value general_pattern) ppat
  | _ -> raise DontMatch

and match_pat_expr : type k. k general_pattern -> _ -> _ = fun tpat pexpr ->
  match tpat.pat_desc, pexpr.pexp_desc with
  | Tpat_record (fields, _), Pexp_field ({pexp_desc = Pexp_ident {txt=Lident "__"; _}; _}, {txt = Lident s; _}) ->
      if not (List.exists (fun (_, {Types.lbl_name; _}, _) -> lbl_name = s) fields) then
        raise DontMatch
  | _ ->
      raise DontMatch

and match_exprs texprs pexprs =
  match_list match_expr texprs pexprs

and match_cases : type k. k case list -> _ -> _ = fun tcases pcases ->
  match_set match_case tcases pcases

and match_value_bindings t p =
  match_set match_value_binding t p

and match_value_binding
    {vb_pat; vb_expr; vb_attributes = _; vb_loc = _; vb_rec_kind = _}
    {pvb_pat; pvb_expr; pvb_attributes = _; pvb_loc = _; pvb_constraint = _} =
  match_expr vb_expr pvb_expr;
  match_pat vb_pat pvb_pat

and match_case : type k. k case -> _ -> _ = fun {c_lhs; c_guard; c_rhs} {pc_lhs; pc_guard; pc_rhs} ->
  match_pat c_lhs pc_lhs;
  match_opt match_expr c_guard pc_guard;
  match_expr c_rhs pc_rhs

let grep_cmt search =
  let expr =
    match Parse.implementation (Lexing.from_string search) with
    | [{Parsetree.pstr_desc = Pstr_eval (x, _); _}] -> x
    | _ -> failwith "Can only grep for an expression."
    | exception _ -> failwith "Could not parse search expression."
  in
  let search_cmt cmt =
    let open Cmt_format in
    let res = ref [] in
    let cmt_search =
      let open Tast_iterator in
      let super = default_iterator in
      let pat : type k. _ -> k general_pattern -> _ = fun self p ->
        try
          match_pat_expr p expr;
          res := p.Typedtree.pat_loc :: !res
        with DontMatch ->
          super.pat self p
      in
      let expr self e =
        wildcards := [];
        try
          match_expr e expr;
          res := e.Typedtree.exp_loc :: !res
        with DontMatch ->
          super.expr self e
      in
      {super with expr; pat}
    in
    begin match cmt.cmt_annots with
    | Implementation str -> cmt_search.Tast_iterator.structure cmt_search str
    | Interface sg -> cmt_search.Tast_iterator.signature cmt_search sg
    | _ -> ()
    end;
    List.sort Stdlib.compare !res
  in
  let rec walk dir =
    Array.iter (fun entry ->
        let entry = Filename.concat dir entry in
        if Sys.is_directory entry then
          walk entry
        else if Filename.check_suffix entry ".cmt" then begin
          match Cmt_format.read_cmt entry with
          | {Cmt_format.cmt_sourcefile = Some source; cmt_source_digest = Some digest; _} as cmt ->
              let source, pp_source =
                if Filename.check_suffix source ".pp.ml" then
                  Filename.chop_suffix source ".pp.ml" ^ ".ml", in_build_dir source
                else
                  let source = drop_prefix ~prefix:build_prefix source in
                  source, source
              in
              if not (Sys.file_exists pp_source) then ()
              else if digest <> Digest.file pp_source then
                Printf.eprintf "** Warning: %s does not correspond to %s (ignoring)\n%!"
                  entry pp_source
              else begin
                let file_color = color Green "%s" source in
                match search_cmt cmt with
                | exception Cannot_parse_type exn ->
                    failwith (Format.asprintf "%s: could not parse type: %a." entry Location.report_exception exn)
                | exception exn ->
                    Format.eprintf "%s: error while analysing %s: %a@." (color Yellow "Warning") entry Location.report_exception exn
                | [] -> ()
                | _ :: _ as locs ->
                    let src_lines = Array.of_list (read_lines source) in
                    List.iter
                      (fun {Location.loc_start; loc_end; _} ->
                         let i = loc_start.pos_lnum in
                         let s = src_lines.(i - 1) in
                         let c1 = loc_start.pos_cnum - loc_start.pos_bol in
                         let c2 =
                           if loc_end.pos_lnum = loc_start.pos_lnum then
                             loc_end.pos_cnum - loc_end.pos_bol
                           else
                             String.length s
                         in
                         print_results_with_color_range i c1 c2 s file_color
                      ) locs
              end
          | {cmt_sourcefile = None; _} | {cmt_source_digest = None; _} ->
              ()
          | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
              failwith "error reading cmt file"
        end
      ) (Sys.readdir dir)
  in
  walk (in_build_dir build_prefix)

(*** Command-line parsing ***)

let collect_cmi_dirs () =
  let res = ref [] in
  let rec walk dir =
    Array.iter (fun entry ->
        let entry = Filename.concat dir entry in
        if Sys.is_directory entry then begin
          if Filename.basename entry = "byte" && Array.exists (fun name -> Filename.check_suffix name ".cmi") (Sys.readdir entry) then
            res := entry :: !res;
          walk entry
        end
      ) (Sys.readdir dir)
  in
  walk (in_build_dir build_prefix);
  List.rev !res

let main () =
  let search = ref None in
  let usage_msg = "Usage: grep_cmt <string>" in
  Arg.parse [] (fun s -> search := Some s) usage_msg;
  let extra_includes = collect_cmi_dirs () in
  Load_path.init ~auto_include:Load_path.no_auto_include ~visible:(List.append extra_includes [Config.standard_library]) ~hidden:[];
  match !search with
  | None -> Arg.usage [] usage_msg; exit 0
  | Some s -> grep_cmt s

let () =
  try
    main ()
  with exn ->
    let s = match exn with Failure s | Sys_error s -> s | exn -> Printexc.to_string exn in
    Printf.eprintf "%s: %s\n%!" (color Red "Error") s;
    exit 1
