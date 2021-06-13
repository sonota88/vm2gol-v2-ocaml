open Utils
open Types

(* 型名は小文字ではじまること *)
(* type t_node = int
 * ;; *)

let print_indent lv =
  let rec iter_print_indent lv =
    match lv with
      | 0 -> ()
      | _ -> (
        print_string "  ";
        iter_print_indent (lv - 1)
      )
  in
  iter_print_indent lv

let rec print_node ((node : t_node), (lv : int), pretty_p) =
  match node with
    | IntNode (n)    -> (
      if pretty_p then print_indent lv;
      print_int n
    )
    | StrNode (s) -> (
      if pretty_p then print_indent lv;
      print_string ("\"" ^ s ^ "\"")
    )
    | ListNode (nodes) ->
       iter_print_list (nodes, 0, List.length nodes, lv, pretty_p)

and iter_print_list (nodes, i, size, lv, pretty_p) =
  if i == 0 then (
    if pretty_p then print_indent lv; 
    print_string "[";
    if pretty_p then print_string "\n"
  );
  match nodes with
    | [] -> (
      if pretty_p then print_indent lv;
      print_string "]"
    )
    | hd :: tl -> (
      print_node (hd, lv + 1, pretty_p);
      if i < size - 1 then
        print_string (if pretty_p then "," else ", ");
      if pretty_p then print_string "\n";
      iter_print_list(tl, i + 1, size, lv, pretty_p)
    )

and print_list ((nodes : t_node list), pretty_p) =
  iter_print_list (nodes, 0, List.length nodes, 0, pretty_p);
  print_string "\n"

let print (nodes : t_node list) =
  print_list (nodes, true)

let print_oneline (nodes : t_node list) =
  print_list (nodes, false)

(* -------------------------------- *)

let starts_with_digit (str : string): bool =
  '0' <= str.[0] && str.[0] <= '9'

let rec parse_list (tail : string) (pos : int) : (t_node list * int) =
  let str_size str : int =
    (String.index_from str 1 '"') - 1
  in

  (* TODO lineno *)
  let rec iter_parse_list tail pos: (t_node list * int) =
    (* prerr_string "-->> iter_parse_list: ";
     * prerr_int pos;
     * prerr_string (" >>" ^ tail ^ "\n"); *)
    if (String.length tail) = 0 then (
      raise Panic
    ) else (
      match tail.[0] with
      | ']' -> (
        (* prerr_string ("  -->> end list\n"); *)
        ([], pos + 1)
      )
      | '[' -> (
        (* prerr_string ("  -->> 113\n"); *)
        let (xs, size) = parse_list tail pos in
        (* prerr_string (Printf.sprintf "  -->> 114 size(%d)\n" size); *)
        (
          let (xs2, pos2) = iter_parse_list (substr tail size) (pos + size) in
          (* prerr_string ("  -->> 117 \n"); *)
          ((ListNode xs) :: xs2, pos2)
        )

      )
      | '\n' -> iter_parse_list (substr tail 1) (pos + 1)
      | ' ' | ',' -> iter_parse_list (substr tail 1) (pos + 1)
      | _ when 0 < match_int tail -> (
        let size = match_int tail in
        (* prerr_string "size: ";
         * prerr_int size;
         * prerr_string "\n"; *)
        let temp_str = str_head tail size in
        (* prerr_string "temp_str: ";
         * prerr_string temp_str;
         * prerr_string "\n"; *)
        let (xs2, pos2) = iter_parse_list (substr tail size) (pos + size) in
        ((IntNode (int_of_string temp_str)) :: xs2, pos2)
      )
      | '"' -> (
        let size = str_size tail in
        let (xs2, pos2) = iter_parse_list (substr tail (size + 2)) (pos + size + 2) in
        (
          (StrNode (str_head (substr tail 1) size)) :: xs2,
          pos2
        )
      )
      | _ -> (
        prerr_string ("tail >>" ^ tail ^ "<<\n");
        raise Panic
      )
    )
  in
  let (xs, pos2) = iter_parse_list (substr tail 1) (pos + 1) in
  (* prerr_string (Printf.sprintf "pos: %d .. %d\n" pos pos2); *)
  (xs, (pos2 - pos))

let parse (json : string): t_node list =
  let (xs, _) = parse_list json 0 in
  xs
