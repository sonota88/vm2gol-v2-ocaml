open Utils
open Types

type token = (int * string * string)

let print_token (t : token) =
  let (lineno, kind, value) = t in
  Json.print [
      IntNode(lineno);
      StrNode(kind);
      StrNode(value)
    ]

let rec print_tokens ts =
  match ts with
    | [] -> ()
    | t :: ts ->
       (
         print_token t;
         print_tokens ts
       )

let ident_char_p c =
  ('a' <= c && c <= 'z')
  || ('0' <= c && c <= '9')
  || c = '_'

let match_ident tail =
  let rec iter_match_ident tail n =
    if ident_char_p tail.[0] then
      iter_match_ident (str_tail tail 1) (n + 1)
    else
      n
  in
  iter_match_ident tail 0

let match_sym tail =
  if
    (tail.[0] = '=' && tail.[1] = '=')
    || (tail.[0] = '!' && tail.[1] = '=')
  then
    2
  else if
    List.mem tail.[0] [
        '(';
        ')';
        '{';
        '}';
        ';';
        '=';
        ',';
        '+';
        '*'
      ]
  then
    1
  else
    0

let match_cmt tail =
  if tail.[0] = '/' && tail.[1] = '/' then
    String.index tail '\n'
  else
    0

let kw_p s =
  List.mem s [
      "func";
      "var";
      "set";
      "_cmt";
      "call";
      "return";
      "call_set";
      "while";
      "case"
    ]

let ident_to_kind ident_str =
  if kw_p ident_str then
    "kw"
  else
    "ident"

let lex src: token list =
  let rec iter tail lineno: token list =
    if String.length tail = 0 then
      []
    else
      (
        match tail.[0] with
        | ' ' -> iter (str_tail tail 1) lineno 
        | '\n' -> iter (str_tail tail 1) (lineno + 1) 
        | '"' ->
           let size = match_str tail in
           let temp_str = str_head (str_tail tail 1) size in
           (lineno, "str", temp_str) :: iter (str_tail tail (size + 2)) lineno
        | _ when 0 < match_sym tail ->
           let size = match_sym tail in
           let temp_str = str_head tail size in
           (lineno, "sym", temp_str) :: iter (str_tail tail size) lineno
        | _ when 0 < match_cmt tail ->
           let size = match_cmt tail in
           iter (str_tail tail size) lineno
        | _ when 0 < match_int tail ->
           let size = match_int tail in
           let temp_str = str_head tail size in
           (lineno, "int", temp_str) :: iter (str_tail tail size) lineno
        | _ when 0 < match_ident tail ->
           let size = match_ident tail in
           let temp_str = str_head tail size in
           let kind = ident_to_kind temp_str in
           (lineno, kind, temp_str) :: iter (str_tail tail size) lineno
        | _ ->
           (
             pkv_i "lineno" lineno;
             prerr_string (Printf.sprintf "tail >>%s<<\n" tail);
             failwith "unexpected pattern"
           )
      )
  in
  iter src 1

let () =
  let src = read_stdin_all () in
  let tokens = lex src in
  print_tokens tokens
