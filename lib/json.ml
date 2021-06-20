open Utils
open Types

let rec print_indent lv =
  match lv with
  | 0 -> ()
  | _ ->
     (
       print_string "  ";
       print_indent (lv - 1)
     )

let rec print_node (node : t_node) (lv : int) pretty_p =
  if pretty_p then print_indent lv;
  match node with
  | IntNode(n) ->
     print_int n
  | StrNode(s) ->
     print_string ("\"" ^ s ^ "\"")
  | ListNode(nodes) ->
     iter_print_list nodes 0 lv pretty_p

and iter_print_list nodes i lv pretty_p =
  if i == 0 then
    (
      print_string "[";
      if pretty_p then print_string "\n"
    );
  match nodes with
    | [] ->
       (
         if pretty_p then print_indent lv;
         print_string "]"
       )
    | node :: rest_nodes ->
       (
         print_node node (lv + 1) pretty_p;
         if 0 < (List.length rest_nodes) then
           print_string (if pretty_p then "," else ", ");
         if pretty_p then print_string "\n";
         iter_print_list rest_nodes (i + 1) lv pretty_p
       )

let print_list (nodes : t_list) pretty_p =
  iter_print_list nodes 0 0 pretty_p

let pretty_print (nodes : t_list) =
  print_list nodes true;
  print_string "\n"

let print (nodes : t_list) =
  print_list nodes false;
  print_string "\n"

(* -------------------------------- *)

let rec parse_list (tail : string) : (t_list * string) =
  let rec iter tail lineno: (t_list * string) =
    if (String.length tail) = 0 then (
      failwith "must not happen"
    ) else (
      match tail.[0] with
      | ']' -> ([], (str_tail tail 1))
      | '[' ->
         (
           let (xs, tail) = parse_list tail in
           let (list_tail, tail) = iter tail lineno in
           (ListNode(xs) :: list_tail, tail)
         )
      | '\n' -> iter (str_tail tail 1) (lineno + 1)
      | ' ' | ',' -> iter (str_tail tail 1) lineno
      | '"' ->
         (
           let size = match_str tail in
           let str = str_head (str_tail tail 1) size in
           let (list_tail, tail) = iter (str_tail tail (size + 2)) lineno in
           (
             StrNode(str) :: list_tail,
             tail
           )
         )
      | _ when 0 < match_int tail ->
         (
           let size = match_int tail in
           let n = int_of_string (str_head tail size) in
           let (list_tail, tail) = iter (str_tail tail size) lineno in
           (
             IntNode(n) :: list_tail,
             tail
           )
         )
      | _ ->
         (
           pkv_i "lineno" lineno;
           prerr_string (Printf.sprintf "tail >>%s<<\n" tail);
           failwith "unexpected pattern"
         )
    )
  in
  let (xs, tail) = iter (str_tail tail 1) 1 in
  (xs, tail)

let parse (json : string): t_list =
  let (xs, _) = parse_list json in
  xs
