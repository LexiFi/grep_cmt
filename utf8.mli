(***************************************************************************)
(*  Copyright (C) 2000-2024 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

val bom: string

val of_windows_1252: string -> string
(** Converts a windows-1252 string to utf-8. *)

val is_valid: string -> bool
(** [is_valid s] returns whether [s] is a valid UTF-8 string. *)

val maybe_of_windows_1252: string -> string
(** The identity if the string is already valid utf8, otherwise
    apply [of_windows_1252]. *)

val drop_last_n: string -> int -> string
(** Drop last n bytes from the utf8 string, rounding to utf-8 character. *)

val drop_last: string -> string
(** Drop last character from the utf8 string. *)

val byte_len: char -> int
(** The lengths (in bytes) of the utf-8 encoding (assumed to be valid)
    starting with a given byte. *)

val is_start: char -> bool
(** Is the character the start of an utf-8 encoding *)

val to_seq: string -> Uchar.t Seq.t
(** Return the sequence of code points in the string *)

val map: (Uchar.t -> Uchar.t) -> string -> string
(** Apply a function to all characters of a Unicode string *)

val uppercase: string -> string
(** Unicode-aware uppercase *)

val lowercase: string -> string
(** Unicode-aware lowercase *)

val capitalize: string -> string
(** Unicode-aware capitalize *)

val escaped: string -> string
(** Unicode-aware escaping.
    - ['"'], ['\n'], ['\b'], ['\r'], ['\t'] and ['\\'] are escaped by prefixing them
      with a backslash.
    - Control characters (C0 + C1 + DEL) are escaped using [\u{XX}] syntax.
    - Any other valid UTF-8 character is output as-is.
    - Invalid UTF-8 byte sequences are output using byte hex escapes [\xhh]. *)

val unaccent_char: Uchar.t -> Uchar.t
(** Replace accented and some other non-ASCII special characters by an ASCII equivalent *)

val unaccent: string -> string
(** Equivalent to [map unaccent_char] *)

(**/**)

val set_uppercase: (string -> string) -> unit
val set_lowercase: (string -> string) -> unit
val set_capitalize: (string -> string) -> unit
