(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(*
  This file is a simplified version of the UTF8 module found in CDUCE:
  http://www.cduce.org/cgi-bin/viewcvs.cgi/cduce/trunk/misc/encodings.ml?revision=1956&view=markup
*)

let bom = "\239\187\191"

(* https://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT *)
let uchar_of_windows_1252 = function
  | '\x00' .. '\x7F' as c -> Uchar.of_char c
  | '\x80' -> Uchar.of_int 0x20AC  (* #EURO SIGN *)
  | '\x81' -> Uchar.rep            (* #UNDEFINED *)
  | '\x82' -> Uchar.of_int 0x201A  (* #SINGLE LOW-9 QUOTATION MARK *)
  | '\x83' -> Uchar.of_int 0x0192  (* #LATIN SMALL LETTER F WITH HOOK *)
  | '\x84' -> Uchar.of_int 0x201E  (* #DOUBLE LOW-9 QUOTATION MARK *)
  | '\x85' -> Uchar.of_int 0x2026  (* #HORIZONTAL ELLIPSIS *)
  | '\x86' -> Uchar.of_int 0x2020  (* #DAGGER *)
  | '\x87' -> Uchar.of_int 0x2021  (* #DOUBLE DAGGER *)
  | '\x88' -> Uchar.of_int 0x02C6  (* #MODIFIER LETTER CIRCUMFLEX ACCENT *)
  | '\x89' -> Uchar.of_int 0x2030  (* #PER MILLE SIGN *)
  | '\x8A' -> Uchar.of_int 0x0160  (* #LATIN CAPITAL LETTER S WITH CARON *)
  | '\x8B' -> Uchar.of_int 0x2039  (* #SINGLE LEFT-POINTING ANGLE QUOTATION MARK *)
  | '\x8C' -> Uchar.of_int 0x0152  (* #LATIN CAPITAL LIGATURE OE *)
  | '\x8D' -> Uchar.rep            (* #UNDEFINED *)
  | '\x8E' -> Uchar.of_int 0x017D  (* #LATIN CAPITAL LETTER Z WITH CARON *)
  | '\x8F' -> Uchar.rep            (* #UNDEFINED *)
  | '\x90' -> Uchar.rep            (* #UNDEFINED *)
  | '\x91' -> Uchar.of_int 0x2018  (* #LEFT SINGLE QUOTATION MARK *)
  | '\x92' -> Uchar.of_int 0x2019  (* #RIGHT SINGLE QUOTATION MARK *)
  | '\x93' -> Uchar.of_int 0x201C  (* #LEFT DOUBLE QUOTATION MARK *)
  | '\x94' -> Uchar.of_int 0x201D  (* #RIGHT DOUBLE QUOTATION MARK *)
  | '\x95' -> Uchar.of_int 0x2022  (* #BULLET *)
  | '\x96' -> Uchar.of_int 0x2013  (* #EN DASH *)
  | '\x97' -> Uchar.of_int 0x2014  (* #EM DASH *)
  | '\x98' -> Uchar.of_int 0x02DC  (* #SMALL TILDE *)
  | '\x99' -> Uchar.of_int 0x2122  (* #TRADE MARK SIGN *)
  | '\x9A' -> Uchar.of_int 0x0161  (* #LATIN SMALL LETTER S WITH CARON *)
  | '\x9B' -> Uchar.of_int 0x203A  (* #SINGLE RIGHT-POINTING ANGLE QUOTATION MARK *)
  | '\x9C' -> Uchar.of_int 0x0153  (* #LATIN SMALL LIGATURE OE *)
  | '\x9D' -> Uchar.rep            (* #UNDEFINED *)
  | '\x9E' -> Uchar.of_int 0x017E  (* #LATIN SMALL LETTER Z WITH CARON *)
  | '\x9F' -> Uchar.of_int 0x0178  (* #LATIN CAPITAL LETTER Y WITH DIAERESIS *)
  | '\xA0' .. '\xFF' as c -> Uchar.of_char c

let rec mk_windows_1252_aux buf s i n =
  if i <> n then
    (Buffer.add_utf_8_uchar buf (uchar_of_windows_1252 s.[i]); mk_windows_1252_aux buf s (succ i) n)

let of_windows_1252 s =
  let b = Buffer.create (String.length s) in
  mk_windows_1252_aux b s 0 (String.length s);
  Buffer.contents b

let byte_len = function
  | '\000'..'\127' -> 1
  | '\192'..'\223' -> 2
  | '\224'..'\239' -> 3
  | '\240'..'\247' -> 4
  | _ -> 1 (* error *)

let is_valid s =
  let n = String.length s in
  let i = ref 0 in
  let r = ref true in
  while !i < n do
    if !i + 7 < n && Int64.logand (String.get_int64_ne s !i) 0x8080_8080_8080_8080L = 0L then (i := !i + 8)
    else if !i + 3 < n && Int32.logand (String.get_int32_ne s !i) 0x8080_8080l = 0l then (i := !i + 4)
    else if Char.code (String.unsafe_get s !i) < 128 then incr i
    else
      let d = String.get_utf_8_uchar s !i in
      if Uchar.utf_decode_is_valid d then (i := !i + Uchar.utf_decode_length d)
      else (i := n; r := false)
  done;
  !r

let maybe_of_windows_1252 s =
  if is_valid s then s else of_windows_1252 s

let is_start c = Char.code c land 0b11000000 <> 0b10000000

let drop_last_n s n =
  let n = ref (String.length s - n) in
  if !n < 0 then ""
  else begin
    while !n > 0 && not (is_start s.[!n]) do decr n done;
    String.sub s 0 !n
  end

let drop_last s = drop_last_n s 1

let to_seq s =
  Seq.unfold
    (fun ofs ->
       if ofs >= String.length s then None
       else
         let d = String.get_utf_8_uchar s ofs in
         Some (Uchar.utf_decode_uchar d, ofs + Uchar.utf_decode_length d)
    )
    0

let mk f =
  let r = ref f in
  (fun f -> r := f), (fun s -> !r s)

let set_uppercase, uppercase =
  mk String.uppercase_ascii

let set_lowercase, lowercase =
  mk String.lowercase_ascii

let set_capitalize, capitalize =
  mk String.capitalize_ascii

let unaccent_map =
  " icL Y|p Ca\"  R-  23\'u . 10\"   ?\
   AAAAAAACEEEEIIII\
   DNOOOOOxOUUUUY s\
   aaaaaaaceeeeiiii\
   onooooo/0uuuuypy"

let () = assert (String.length unaccent_map = 6 * 16)

let unaccent_char c0 =
  let i = Uchar.to_int c0 in
  if 0xa0 <= i && i <= 0xFF then
    let c = Char.code unaccent_map.[i - 0xa0] in
    if c = 32 then c0 else Uchar.of_int c
  else
    c0

let map f s =
  let buf = Buffer.create (String.length s) in
  let rec loop pos =
    if pos >= String.length s then Buffer.contents buf
    else
      let d = String.get_utf_8_uchar s pos in
      let u = Uchar.utf_decode_uchar d in
      Buffer.add_utf_8_uchar buf (f u);
      loop (pos + Uchar.utf_decode_length d)
  in
  loop 0

let unaccent = map unaccent_char

let is_control u =
  let n = Uchar.to_int u in
  (0 <= n && n <= 0x1F) (* C0 *) || n = 0x7F (* DEL *) || (0x80 <= n && n <= 0x9F) (* C1 *)

let needs_escape s =
  let rec loop ofs =
    if ofs >= String.length s then false
    else
      let d = String.get_utf_8_uchar s ofs in
      if Uchar.utf_decode_is_valid d then
        let u = Uchar.utf_decode_uchar d in
        is_control u || u = Uchar.of_char '"' || u = Uchar.of_char '\\' ||
        loop (ofs + Uchar.utf_decode_length d)
      else
        true
  in
  loop 0

let really_escape s =
  let buf = Buffer.create (String.length s) in
  let rec loop ofs =
    if ofs >= String.length s then
      Buffer.contents buf
    else
      let d = String.get_utf_8_uchar s ofs in
      if Uchar.utf_decode_is_valid d then begin
        let u = Uchar.utf_decode_uchar d in
        if Uchar.is_char u then
          match Uchar.to_char u with
          | '\"' ->
              Buffer.add_string buf "\\\""
          | '\\' | '\n' | '\t' | '\r' | '\b' as c ->
              Buffer.add_string buf (Char.escaped c)
          | _ ->
              let n = Uchar.to_int u in
              if is_control u then
                Printf.bprintf buf "\\u{%X}" n
              else
                Buffer.add_utf_8_uchar buf u
        else
          Buffer.add_utf_8_uchar buf u
      end else begin
        for i = 0 to Uchar.utf_decode_length d - 1 do
          Printf.bprintf buf "\\x%02x" (Char.code s.[ofs + i])
        done
      end;
      loop (ofs + Uchar.utf_decode_length d)
  in
  loop 0

let escaped s =
  if needs_escape s then
    really_escape s
  else
    s
