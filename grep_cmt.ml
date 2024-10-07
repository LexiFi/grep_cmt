(* This file is part of the grep-cmt package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                  *)
(* Copyright (C) 2000-2024 LexiFi                                                                  *)

(*
  Notes about structural search:

  - the wildcard __ matches any expression or any record field

  - a numbered wildcard __1, __2, ... matches any expression or record field
    and enforce strict equality of all the matching occurrences for the same
    number

  - an identifier (value or class) is matched as a suffix of the fully
    qualified path in the typed expression

  - labels and constructors identifiers are matched as a suffix of the
    identifier in the typed expression.

  - for function applications, it is allowed to omit in the pattern
    any argument of the actual function call; special forms
    are recognized to enforce that a given optional argument
    present or missing:

      foo ?arg:PRESENT
      foo ?arg:MISSING

  - an expression (... : typexpr) matches any expression matching
    ... and whose type is equal to typexpr

  - for try..with/match..with/functions expressions, the order
    of clauses doesn't matter.  A single clause of the searched
    expression can match several clauses of the code.  Same set-semantics
    for record expressions.

  - an expression [e1.lid1] matches [e2.lid2 <- e3] if [e1] matches [e2] and
    the label [lid1] matches [lid2].

  - the expression [__.id] matches any *pattern* of the form [{...; P.id; ...}]
    for any prefix [P]. This rule was added so that grepping for [__.foo] will
    return every "get" and "set" of the record field [foo], including reads in
    patterns.
*)

open Asttypes
open Parsetree
open Typedtree
open Longident

(*** Command-line parsing ***)

let extra_includes = ref []
let name_filter = ref None
let search = ref []
let case_sensitive = ref true
let exclude = ref false
let global_file_mode = ref false
let create_grep_file = ref true
let union = ref false
let from_start = ref false
let emacs_mode = ref false
let ctx = ref 0
let verbose = ref false

let no_grep_file =
  try
    ignore (Sys.getenv "NOGREPFILE");
    true
  with Not_found -> false

let () =
  let open Arg in
  let usage_msg =
    "usage: grep_svn <options> <string>"
  in
  let parsers =
    align
      [
        "-verbose", Set verbose, " verbose mode";
        "-C", Int ((:=) ctx), " context lines; mimic the C option of grep";
        "-root", Set from_start, " search from root directory";
        "-emacs", Set emacs_mode, " output is emacs friendly";
        "-i", Clear case_sensitive, " case insensitive search";
        "-I", String (fun s -> extra_includes := s :: !extra_includes), "<dir> extend load path";
      ]
  in
  let parse () =
    parse
      parsers
      (fun s -> search := s :: !search)
      usage_msg
  in
  parse ();
  emacs_mode := not (Unix.isatty Unix.stdout) || !emacs_mode;
  union := true;
  begin match !search, !name_filter with
  | [], None -> usage parsers usage_msg; exit 0
  | _ -> ()
  end


(*** Collect files ***)


let initial_cwd = Sys.getcwd ()

let run_command cmd =
  let inchan = Unix.open_process_in cmd in
  let buf = try input_line inchan with End_of_file -> "" in
  let err = Unix.close_process_in inchan in
  match err with
  | WEXITED 0 ->
      buf
  | _ ->
      Printf.eprintf "Command failed: %s\n%!" cmd;
      exit 2

let git_root =
  run_command "git rev-parse --show-toplevel"

let grep_file =
  if !emacs_mode || no_grep_file then
    None
  else
    let ret =
      let file = Lexifi.F.concat git_root "grep_cmt.grep" in
      if !create_grep_file then
        open_out file
      else
        open_out_gen [Open_wronly; Open_append; Open_text] 0o666 file
    in
    Some ret

let write str = Option.iter (fun file -> output_string file str; flush file) grep_file

let fwrite x = Printf.ksprintf write x

let () =
  let args =
    String.concat " "
      [
        "-*-";
        "mode: grep;";
        Printf.sprintf "compilation-directory: %S;" (Sys.getcwd());
        Printf.sprintf "compile-command: %S;"
          (String.concat " " (Array.to_list Sys.argv));
        "-*-";
      ]
  in
  fwrite "%s\n\n" args

let print_red_string s =
  if !emacs_mode then s
  else Printf.sprintf "\027[1;31m%s\027[00m" s

let print_green_string s =
  if !emacs_mode then s
  else Printf.sprintf "\027[1;32m%s\027[00m" s

let print_yellow_string s =
  if !emacs_mode then s
  else Printf.sprintf "\027[1;33m%s\027[00m" s

let print_yellow_int i =
  if !emacs_mode then string_of_int i
  else Printf.sprintf "\027[1;33m%i\027[00m" i

let expand_cwd =
  match
    Lexifi.S.drop_prefix ~prefix:(Lexifi.F.concat git_root "") initial_cwd
  with
  | None -> Fun.id
  | Some cwd -> Lexifi.F.concat cwd

let print_results_with_color_range i c1 c2 s file file_color =
  let i_color = print_yellow_int i in
  let s_color =
    let len = String.length s in
    if c2 > len || c1 > len then
      Printf.sprintf
        " Skipping this line with wrong indexes -- Maybe you should think about recompiling this file."
    else
      String.sub s 0 c1^
      print_red_string (String.sub s c1 (c2-c1))^
      String.sub s c2 (String.length s - c2)
  in
  if Lexifi.S.is_printable s then
    begin
      Printf.fprintf stdout "%s:%s:%s\n%!" file_color i_color s_color;
      fwrite "%s:%i:%s\n%!" file i s;
    end

let handle_global_match ~lines file =
  let file_color =
    if !emacs_mode then
      if !from_start then
        Lexifi.F.absolute_path file
      else
        file
    else
      print_green_string file
  in
  match List.sort_uniq Int.compare lines, !exclude with
  | line :: l, false ->
      let lines = Lexifi.L.to_string " " string_of_int l in
      let line_color = print_yellow_int line in
      Printf.fprintf stdout "%s:%s:<FOUND>:%s\n%!" file_color line_color lines;
      fwrite "%s:%i:<FOUND>:%s\n" (expand_cwd file) line lines;
  | [], true ->
      Printf.fprintf stdout "%s:1:<NOT FOUND>\n%!" file_color;
      fwrite "%s:1:<NOT FOUND>\n" (expand_cwd file);
  | _ ->
      ()

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

let parse_type t =
  let env = Compmisc.initial_env () in
  try (Typetexp.transl_type_scheme env (*true*) t).ctyp_type
  with e -> raise (Cannot_parse_type e)

let parse_type = Lexifi.H.memoize (Hashtbl.create 10) parse_type

let initial_env = lazy (Compmisc.initial_env ())

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

  | Texp_function {arg_label = Nolabel; cases = tcases; _}, Pexp_function pcases ->
      match_cases tcases pcases
  | Texp_function {arg_label = l1; cases = [{c_lhs=p1; c_guard=None; c_rhs=e1}]; _},
    Pexp_fun (l2, None, p2, e2)
    when l1 = l2 ->
      match_pat p1 p2;
      match_expr e1 e2

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

  | Texp_assert te, Pexp_assert pe
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
  | Tpat_var (_, {txt = s1; _}), Ppat_var {txt = s2; _} when is_wildcard s2 ->
      check_wildcard_lid s2 (Lident s1)
  | Tpat_var (_, {txt = s1; _}), Ppat_var {txt = s2; _} when s1 = s2 -> ()
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
    {vb_pat; vb_expr; vb_attributes = _; vb_loc = _}
    {pvb_pat; pvb_expr; pvb_attributes = _; pvb_loc = _} =
  match_expr vb_expr pvb_expr;
  match_pat vb_pat pvb_pat

and match_case : type k. k case -> _ -> _ = fun {c_lhs; c_guard; c_rhs} {pc_lhs; pc_guard; pc_rhs} ->
  match_pat c_lhs pc_lhs;
  match_opt match_expr c_guard pc_guard;
  match_expr c_rhs pc_rhs

let grep_svn () =
  if !from_start then Sys.chdir git_root;
  let search_list = !search in
  let search_expr =
    lazy
      begin try match !search with
        | [s] ->
            begin match Parse.implementation (Lexing.from_string s) with
            | [{Parsetree.pstr_desc = Pstr_eval (x, _); _}] -> x
            | _ -> failwith "Can only grep for an expression"
            end
        | _ ->
            assert false
      with
      | Syntaxerr.Error _ as exn ->
          Format.printf "Error while parsing search expression: %a@." Location.report_exception exn;
          exit 2
      | exn ->
          Printf.eprintf "Error while parsing search expression: %s\n%!"
            (Printexc.to_string exn);
          exit 2
      end
  in
  let search_cmt cmt =
    let open Cmt_format in
    let expr = Lazy.force search_expr in
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
  let search =
    List.map
      (fun search ->
         let search_color = print_red_string search in
         search, search_color
      )
      search_list
  in
  write "\n";
  let global_file_mode =
    !global_file_mode || search = []
  in
  let handle_global_match ~lines file =
    if global_file_mode then handle_global_match ~lines file
  in
  let git_prefix = run_command "git rev-parse --show-prefix" in
  let rec walk dir =
    Array.iter (fun entry ->
        let entry = Filename.concat dir entry in
        match (Unix.lstat entry).Unix.st_kind with
        | Unix.S_DIR ->
            walk entry
        | Unix.S_REG when Filename.check_suffix entry ".cmt" ->
            begin match Cmt_format.read_cmt entry with
            | {Cmt_format.cmt_sourcefile = Some source; cmt_source_digest = Some digest; _} as cmt ->
                if search = [] then
                  handle_global_match ~lines:[1] source
                else begin
                  let source =
                    match Lexifi.S.drop_prefix ~prefix:git_prefix source with
                    | None -> source
                    | Some source -> source
                  in
                  if not (Sys.file_exists source) then ()
                  else if digest <> Digest.file source then begin
                    Printf.eprintf "** Warning: %s does not correspond to %s (ignoring)\n%!"
                      entry source
                  end else begin
                    if !verbose then prerr_endline source;
                    let file_no_color, file_color =
                      if !emacs_mode then
                        if !from_start then
                          Lexifi.F.absolute_path source, Lexifi.F.absolute_path source
                        else
                          source, source
                      else
                        source, print_green_string source
                    in
                    match search_cmt cmt with
                    | exception Cannot_parse_type exn ->
                        Format.printf "Error while analysing %s@.Cannot parse type@.%a@.Aborting@."
                          entry Location.report_exception exn;
                        exit 2
                    | exception exn ->
                        Format.printf "Error while analysing %s: %a@." entry Location.report_exception exn
                    | [] -> ()
                    | _ :: _ as locs ->
                        let src_lines = Array.of_list (Lexifi.F.read_lines source) in
                        let lines =
                          List.map
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
                               if not global_file_mode then
                                 print_results_with_color_range i c1 c2 s file_no_color file_color;
                               i
                            ) locs
                        in
                        handle_global_match ~lines source
                  end
                end
            | {cmt_sourcefile = None; _} | {cmt_source_digest = None; _} ->
                ()
            | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
                failwith "error reading cmt file"
            end
        | _ -> ()
      ) (Sys.readdir dir)
  in
  prerr_endline
    (print_yellow_string
       "*** NOTE: if some hits seem to be missing (particularly in `applications/apropos'),\n\
       \    you need to do `dune build @check' in order to build all `.cmt' files.");
  walk (Filename.concat git_root (Filename.concat "_build/default" git_prefix))

let () =
  if !union then
    let search_list = List.rev !search in
    List.iteri
      (fun i s ->
         if i <> 0 then create_grep_file := false;
         search := [s]; grep_svn ()
      )
      search_list;
  else
    grep_svn ();
  Option.iter close_out grep_file
