(* let echo () =
 *   let rec echo_sub () =
 *     print_string (read_line ());
 *     print_newline ();
 *     echo_sub ()
 *   in
 *   try echo_sub () with End_of_file -> ()
 * in
 * echo (); *)

(* let cat filename =
 *   let fin = open_in filename in
 *   let rec cat_sub () =
 *     output_char stdout (input_char fin);
 *     cat_sub ()
 *   in
 *   try cat_sub () with End_of_file -> close_in fin
 * in
 * cat "test/json/07.json"; *)

(*
  input_char でも 1バイトずつ読む。
  違いは int/char の違い
*)
(* let cat_byte () =
 *   let rec cat_byte_sub () =
 *     let x = (input_byte stdin) in 
 *     (\* output_char stderr '/';
 *      * output_value stderr x;
 *      * output_char stderr '\n'; *\)
 *     output_byte stdout x;
 *     cat_byte_sub ()
 *   in
 *   try cat_byte_sub () with End_of_file -> ()
 * in
 * cat_byte () *)

(* let () =
 *   let print_byte (byte : int) =
 *     match byte with
 *     | 10 -> print_string "<LF>\n"
 *     | _  -> output_byte stdout byte
 *   in
 *   let rec cat_byte_sub () =
 *     print_byte (input_byte stdin);
 *     cat_byte_sub ()
 *   in
 *   try cat_byte_sub () with End_of_file -> () *)

exception Panic
exception Panic2 of string

(* -------------------------------- *)

let pkv_i k (v : int) =
  prerr_string (Printf.sprintf "%s (%d)\n" k v)

(* -------------------------------- *)

let read_stdin_all () =
  let input_char_opt () : char option =
    try Some (input_char stdin)
    with End_of_file -> None
  in
  let string_of_char (c : char) : string =
    Printf.sprintf "%c" c
  in
  let rec iter_read_stdin_all () : string =
    match input_char_opt () with
    | Some (c) -> string_of_char(c) ^ iter_read_stdin_all ()
    | None -> ""
  in
  iter_read_stdin_all ()

(* -------------------------------- *)

let substr (str : string) (n : int): string =
  String.sub str n ((String.length str) - n)

let str_head (str : string) (n : int): string =
  String.sub str 0 n

let starts_with (str : string) (head : string): bool =
  String.equal
    (str_head str (String.length head))
    head

(* -------------------------------- *)

let starts_with_digit (str : string): bool =
  '0' <= str.[0] && str.[0] <= '9'

let match_int str: int =
  let rec iter_match_int str i =
    if (String.length str) = 0 then
      i
    else if starts_with_digit str || starts_with str "-" then
      iter_match_int (substr str 1) (i + 1)
    else
      i
  in
  iter_match_int str 0
