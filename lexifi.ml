(* This file is part of the grep-cmt package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                  *)
(* Copyright (C) 2000-2024 LexiFi                                                                  *)


module L: sig
  val to_string: string -> ('a -> string) -> 'a list -> string
  (** [to_string sep f l] is equivalent to [String.concat sep (List.map f l)] *)
end = struct
  module ObjSet = Set.Make(struct type t = Obj.t let compare = Stdlib.compare end)

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

module S: sig
  val replace_char: string -> char -> char -> string
  (** [replace_char s c_old c_new] returns a copy of [s] with each occurrence of character [c_old] replaced with [c_new]. *)

  val cut_start: int -> string -> string
  (** [cut_start n s] returns the string s without its first [n] characters *)

  val cut_end: int -> string -> string
  (** [cut_end n s] returns the string s without its last [n] characters *)


  val drop_prefix: prefix:string -> string -> string option
  (** [drop_prefix ~prefix s] returns [Some s'] if [s = prefix ^ s'] and [None] if [prefix] is not a prefix of [s]. *)

  val is_printable: string -> bool
  (** Returns true if all the ascii code of the characters in [s] are either tab, new line, carriage return or between 32 and 255. *)
end = struct
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

  let drop_prefix ~prefix s =
    if String.starts_with ~prefix s
    then Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
    else None

  let is_printable = String.for_all (fun c -> '\032' <= c || c = '\t' || c = '\r' || c = '\n')
end

module F: sig
  val concat: string -> string -> string

  val read_lines: string -> string list

  val absolute_path: string -> string
  (** Make a path name absolute and simplify . and .. components. *)
end = struct
  let slash_win f = S.replace_char f '/' '\\'

  let concat d f =
    let f = d ^ Filename.dir_sep ^ f in
    if Sys.win32 then slash_win f else f

  let read_lines filename =
    let ic = open_in filename in
    let line = ref (In_channel.input_line ic) in
    let lines = ref [] in
    while !line <> None do
      lines := Option.get !line :: !lines;
      line := In_channel.input_line ic
    done;
    close_in ic;
    List.rev !lines

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

module H: sig
  val memoize: ('a, 'b) Hashtbl.t -> ('a -> 'b) -> ('a -> 'b)
  (** [memoize tbl f] returns a function that memoizes its calculation
      by using provided hash table. However, raised exceptions are not
      memoized. *)
end = struct
  let memoize h f k =
    match Hashtbl.find_opt h k with
    | None -> let r = f k in Hashtbl.add h k r; r
    | Some r -> r
end
