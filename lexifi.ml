(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)


module L = struct
  module ObjSet = Set.Make(struct type t = Obj.t let compare = Stdlib.compare end)

  let mem_list_map f = function
    | [] -> fun _ -> false
    | [e] -> let e = f e in fun x -> Stdlib.compare x e = 0
    | [e1; e2] -> let e1 = f e1 and e2 = f e2 in fun x -> Stdlib.compare x e1 = 0 || Stdlib.compare x e2 = 0
    | [e1; e2; e3] -> let e1 = f e1 and e2 = f e2 and e3 = f e3 in fun x -> Stdlib.compare x e1 = 0 || Stdlib.compare x e2 = 0 || Stdlib.compare x e3 = 0
    | l ->
      let s = List.fold_left (fun acc e -> ObjSet.add (Obj.repr (f e)) acc) ObjSet.empty l in
      fun e -> ObjSet.mem (Obj.repr e) s

  let mem_list l = mem_list_map Fun.id l

  let to_string sep f = function
    | [] -> ""
    | hd :: tl ->
      let seplen = String.length sep in
      let rec aux len = function
        | [] -> Bytes.create len
        | hd :: tl ->
          let s = f hd in
          let slen = String.length s in
          let buf = aux (len + seplen + slen) tl in
          Bytes.blit_string sep 0 buf len seplen;
          Bytes.blit_string s 0 buf (len + seplen) slen;
          buf
      in
      let s = f hd in
      let slen = String.length s in
      let buf = aux slen tl in
      Bytes.blit_string s 0 buf 0 slen;
      Bytes.unsafe_to_string buf
end

module S = struct
  let replace_char s old_c new_c =
    let s = Bytes.of_string s in
    for i = 0 to Bytes.length s - 1 do
      if Bytes.unsafe_get s i = old_c then Bytes.unsafe_set s i new_c
    done;
    Bytes.unsafe_to_string s

  let cut_start i s =
    let l = String.length s in
    if i <= 0 then s
    else if l <= i then ""
    else String.sub s i (l - i)

  let cut_end i s =
    let l = String.length s in
    if i <= 0 then s
    else if l <= i then ""
    else String.sub s 0 (l - i)

  let prepare_kmp_table p =
    let m = String.length p in
    let next = Array.make m 0 in
    let rec f i j =
      if i < m - 1 then
        if p.[i] = p.[j]
        then
          begin next.(i + 1) <- j + 1; f (i + 1) (j + 1) end
        else
        if j = 0 then
          begin next.(i + 1) <- 0; f (i + 1) j end
        else
          f i (next.(j))
    in
    f 1 0;
    next

  let find_substring_from ~pat =
    let next = prepare_kmp_table pat in
    fun s i ->
      let m = String.length pat in
      let n = String.length s in
      let rec f i j =
        if m <= j then Some (i - m) else
        if n <= i then None else
        if s.[i] = pat.[j] then f (i + 1) (j + 1) else
        if j = 0 then f (i + 1) j else
          f i next.(j)
      in
      f i 0

  let find_substring ~pat =
    let find = find_substring_from ~pat in
    fun s -> find s 0

  let contains_string ~pattern =
    match String.length pattern with
    | 0 -> fun _ -> true
    | 1 -> fun s -> String.contains s pattern.[0]
    | _ ->
      let find = find_substring ~pat:pattern in
      fun s ->
        match find s with
        | None -> false
        | Some _ -> true

  let drop_prefix ~prefix s =
    if String.starts_with ~prefix s
    then Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
    else None

  let replace_string s pat s_new =
    if pat = "" then invalid_arg "replace_string: the old pattern shouldn't be empty";
    let find = find_substring_from ~pat in
    match find s 0 with
    | None -> s
    | Some j ->
      let buf = Buffer.create (String.length s) in
      Buffer.add_substring buf s 0 j;
      Buffer.add_string buf s_new;
      let rec loop i =
        match find s i with
        | None -> Buffer.add_substring buf s i (String.length s - i)
        | Some j ->
          Buffer.add_substring buf s i (j - i);
          Buffer.add_string buf s_new;
          loop (j + String.length pat)
      in
      loop (j + String.length pat);
      Buffer.contents buf

  let is_printable = String.for_all (fun c -> '\032' <= c || c = '\t' || c = '\r' || c = '\n')
end

module O = struct
  let unit_bool = function
    | Some () -> true
    | None -> false
end

module F = struct
  let slash_win f = S.replace_char f '/' '\\'

  let concat d f =
    let f = d ^ Filename.dir_sep ^ f in
    if Sys.win32 then slash_win f else f


  let extension s =
    match String.rindex_opt s '.' with
    | Some i -> Some (S.cut_start (i + 1) s |> String.lowercase_ascii)
    | None -> None

  (* Perform CRLF translation even under Linux *)
  let input_line_text ic =
    let s = input_line ic in
    let n = String.length s in
    if 0 < n && String.unsafe_get s (n - 1) = '\r' then String.sub s 0 (n - 1) else s

  let input_lines ?maybe_of_windows_1252 ic =
    let input_line_text =
      if O.unit_bool maybe_of_windows_1252 then
        fun ic -> Utf8.maybe_of_windows_1252 (input_line_text ic)
      else
        input_line_text
    in
    let rec aux accu =
      match input_line_text ic with
      | exception End_of_file -> List.rev accu
      | s -> aux (s :: accu)
    in
    match aux [] with
    | [] -> []
    | first :: rest as l ->
      match S.drop_prefix ~prefix:Utf8.bom first with
      | Some first -> first :: rest
      | None -> l

  let read_lines ?maybe_of_windows_1252 filename =
    let ic = open_in filename in
    match input_lines ?maybe_of_windows_1252 ic with
    | lines -> close_in ic; lines
    | exception e -> close_in_noerr ic; raise e

  let is_slash = function '/' | '\\' -> true | _ -> false

  let extended_path_prefix = "\\\\?\\"

  let absolute_path s =
    if String.starts_with ~prefix:extended_path_prefix s then
      s
    else
      let open Filename in
      let s = if is_relative s then concat (Sys.getcwd ()) s else s in
      let rec aux s =
        let base = basename s in
        let dir = dirname s in
        if dir = s then dir
        else if base = current_dir_name then aux dir
        else if base = parent_dir_name then dirname (aux dir)
        else concat (aux dir) base
      in
      (* Hack for UNC paths *)
      if Sys.win32 && String.length s > 2 && is_slash s.[0] && is_slash s.[1] then
        "\\" ^ aux ("\\" ^ String.sub s 2 (String.length s - 2))
      else
        aux s
end

module H = struct
  let memoize h f k =
    match Hashtbl.find_opt h k with
    | None -> let r = f k in Hashtbl.add h k r; r
    | Some r -> r
end
