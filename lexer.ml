open Utils
open Types

type token = (int * string * string)

let print_token (t : token) =
  let (lineno, kind, value) = t in
  Json.print_oneline [
      IntNode lineno;
      StrNode kind;
      StrNode value
    ]

let rec print_tokens ts =
  match ts with
    | [] -> ()
    | t :: ts -> (
      print_token t;
      print_tokens ts
    )

let ident_char_p c =
  ('a' <= c && c <= 'z')
  || ('0' <= c && c <= '9')
  || c = '_'

let match_ident tail =
  let rec iter_match_ident tail n =
    if ident_char_p tail.[0] then (
      iter_match_ident (substr tail 1) (n + 1)
    ) else (
      n
    )
  in
  iter_match_ident tail 0

(* TODO mv to utils *)
let match_str str : int =
  if str.[0] = '"' then
    (String.index_from str 1 '"') - 1
  else
    raise (Panic "42")

let match_sym tail =
  if tail.[0] = '=' && tail.[1] = '=' then
    2
  else if
    tail.[0] = '('
    || tail.[0] = ')'
    || tail.[0] = '{'
    || tail.[0] = '}'
    || tail.[0] = ';'
    || tail.[0] = '='
    || tail.[0] = ','
    || tail.[0] = '+'
  then
    1
  else
    0

let match_cmt tail =
  if tail.[0] = '/' && tail.[1] = '/'
  then
    String.index tail '\n'
  else
    0

let kw_p s =
  s = "func"
  || s = "var"
  || s = "set"
  || s = "_cmt"
  || s = "call"
  || s = "return"
  || s = "call_set"

let ident_to_kind ident_str =
  if kw_p ident_str then
    "kw"
  else
    "ident"

let lex src: token list =
  let rec iter_lex src pos: token list =
    let tail = substr src pos in
    (* prerr_string (Printf.sprintf "tail %d >>%s<<\n" pos tail); *)
    if String.length tail = 0 then (
      []
    ) else (
      (* prerr_string (Printf.sprintf "  105 \n");
       * prerr_string (Printf.sprintf "  106 match sym %d \n" (match_sym tail));
       * prerr_string (Printf.sprintf "  106 match int %d \n" (match_int tail));
       * prerr_string (Printf.sprintf "  106 match str %d \n" (match_str tail)); *)
      match tail.[0] with
      | ' ' -> (
        iter_lex src (pos + 1)
      )
      | '\n' -> (
          (* TODO inc lineno *)
        iter_lex src (pos + 1)
      )
      | '"' -> (
        (* prerr_string (Printf.sprintf "  match str \n"); *)
        let size = match_str tail in
        let temp_str = str_head (substr tail 1) size in
        (1, "str", temp_str) :: iter_lex src (pos + size + 2)
      )
      | _ when 0 < match_sym tail -> (
        (* prerr_string (Printf.sprintf "  match sym \n"); *)
        let size = match_sym tail in
        let temp_str = str_head tail size in
        (1, "sym", temp_str) :: iter_lex src (pos + size)
      )
      | _ when 0 < match_cmt tail -> (
        let size = match_cmt tail in
        iter_lex src (pos + size)
      )
      | _ when 0 < match_int tail -> (
        (* prerr_string (Printf.sprintf "  match int \n"); *)
        let size = match_int tail in
        let temp_str = str_head tail size in
        (1, "int", temp_str) :: iter_lex src (pos + size)
      )
      | _ when 0 < match_ident tail -> (
        (* prerr_string (Printf.sprintf "  match ident \n"); *)
        let size = match_ident tail in
        let temp_str = str_head tail size in
        (
          1,
          ident_to_kind temp_str,
          temp_str
        ) :: iter_lex src (pos + size)
      )
      | _ -> (
        prerr_string (Printf.sprintf "pos (%d) " pos);
        prerr_string (Printf.sprintf "tail >>%s<<" tail);
        raise (Panic "135")
      )
    )
    (* [(1, "kw", "func")] *)
  in
  iter_lex src 0

let () =
  let src = read_stdin_all () in
  let tokens = lex src in
  (* List.map (fun t -> print_token t) tokens *)
  print_tokens tokens

(* print_string "[]" *)
