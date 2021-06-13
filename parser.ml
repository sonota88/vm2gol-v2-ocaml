open Utils
open Types
open Json

type token = { lineno : int; kind : string; value : string }

let read_tokens (): token list =
  let rec iter_read_tokens () =
    try 
      let line = read_line () in
      let xs = Json.parse line in
      (* prerr_string line;
       * prerr_string "\n"; *)
      match xs with
      | [
          IntNode lineno;
          StrNode kind;
          StrNode value
        ] ->
         { lineno; kind; value } :: iter_read_tokens ()
      | _ -> raise (Panic "21")
    with
      End_of_file -> []
  in
    iter_read_tokens ()

let consume tokens pos value =
  match tokens with
  | [] -> raise (Panic "29")
  | hd :: tl -> (
    if hd.value = value then
      (tl, pos + 1)
    else
      raise (Panic (Printf.sprintf "34: expected(%s) actual(%s)" value hd.value))
  )

let get_value ts pos =
  match ts with
  | [] -> raise (Panic "29")
  | hd :: tl -> (
    (* pkv_i "pos 50" pos;
     * pkv_i "pos 50b" (List.length ts); *)
    (tl, pos + 1, hd.value)
  )

let skip ts pos =
  (List.tl ts, pos + 1)

(* -------------------------------- *)

let parse_args ts pos: (token list * int * t_node list) =
  let rec iter ts pos =
    if (List.hd ts).value = ")" then
      (ts, pos, [])
    else
      let (ts, pos, arg) =
        (
        match (List.hd ts).kind with
          | "int" ->
             (
               let (ts, pos, s) = get_value ts pos in
               (
                 ts,
                 pos,
                 IntNode (int_of_string s)
               )
             )
          | "ident" ->
             (
               let (ts, pos, s) = get_value ts pos in
               (ts, pos, StrNode s)
             )
          | _ -> raise (Panic "61")
        )
      in
      if (List.hd ts).value = "," then
        (
          let (ts, pos) = consume ts pos "," in
          let (ts, pos, rest_args) = iter ts pos in
          (ts, pos, (arg :: rest_args))
        )
      else
        (ts, pos, [arg])
  in
  iter ts pos

let rec _parse_expr_right ts pos =
  match (List.hd ts).value with
  | "+" | "==" ->
     (
       let (ts, pos, op_str) = get_value ts pos in
       let (ts, pos, expr_r) = parse_expr ts pos in
       Some(ts, pos, op_str, expr_r)
     )
  | _ -> None

and parse_expr ts pos: (token list * int * t_node) =
  let (ts, pos, expr_l) = (
      let t = List.hd ts in
      match t.kind with
      | "int" ->
         let (ts, pos) = skip ts pos in
         (
           ts, pos,
           IntNode (int_of_string t.value)
         )
      | "ident" ->
         let (ts, pos) = skip ts pos in
         (
           ts, pos,
           StrNode t.value
         )
      | "sym" ->
         let (ts, pos) = consume ts pos "(" in
         let (ts, pos, expr) = parse_expr ts pos in
         let (ts, pos) = consume ts pos ")" in
         (ts, pos, expr)
      | _ -> raise (Panic "103 TODO")
    )
  in

  let op_r = _parse_expr_right ts pos in
  match op_r with
  | Some (ts, pos, op_str, expr_r) ->
     (
       ts, pos,
       ListNode [
           StrNode op_str;
           expr_l;
           expr_r
         ]
     )
  | None ->
     (
       (ts, pos, expr_l)
     )

let parse_set ts pos =
    let (ts, pos) = consume ts pos "set" in
    let (ts, pos, var_name) = get_value ts pos in
    let (ts, pos) = consume ts pos "=" in

    let (ts, pos, expr) = parse_expr ts pos in

    let (ts, pos) = consume ts pos ";" in
    (
      ts,
      pos,
      ListNode [
          StrNode "set";
          StrNode var_name;
          expr
        ]
    )

let parse_funcall ts pos: (token list * int * t_node list) =
    let fn_name = (List.hd ts).value in
    let (ts, pos) = skip ts pos in

    let (ts, pos) = consume ts pos "(" in
    let (ts, pos, args) = parse_args ts pos in
    let (ts, pos) = consume ts pos ")" in

    (ts, pos, StrNode fn_name :: args)

let parse_call ts pos =
    let (ts, pos) = consume ts pos "call" in
    let (ts, pos, funcall) = parse_funcall ts pos in
    let (ts, pos) = consume ts pos ";" in
    (
      ts,
      pos,
      ListNode (
          StrNode "call"
          :: funcall
        )
    )

let parse_call_set ts pos =
    let (ts, pos) = consume ts pos "call_set" in
    let (ts, pos, var_name) = get_value ts pos in
    let (ts, pos) = consume ts pos "=" in
    let (ts, pos, funcall) = parse_funcall ts pos in
    let (ts, pos) = consume ts pos ";" in
    (
      ts,
      pos,
      ListNode (
          StrNode "call_set"
          :: StrNode var_name
          :: [ListNode funcall]
        )
    )

let parse_return ts pos =
  let (ts, pos) = consume ts pos "return" in

  if (List.hd ts).value = ";" then
    let (ts, pos) = consume ts pos ";" in
    (
      ts, pos,
      ListNode [
        StrNode "return";
      ]
    )
  else
    (
      let (ts, pos, expr) = parse_expr ts pos in
      let (ts, pos) = consume ts pos ";" in
      (
        ts, pos,
        ListNode [
          StrNode "return";
          expr
        ]
      )
    )

(* while *)
(* case *)

let parse_vm_comment ts pos =
  let (ts, pos) = consume ts pos "_cmt" in
  let (ts, pos) = consume ts pos "(" in

  let cmt = (List.hd ts).value in
  let (ts, pos) = skip ts pos in

  let (ts, pos) = consume ts pos ")" in
  let (ts, pos) = consume ts pos ";" in

  (
    ts, pos,
    ListNode [
        StrNode "_cmt";
        StrNode cmt
      ]
  )

let parse_var tokens pos: (token list * int * t_node) =
  let (ts, pos) = consume tokens pos "var" in
  (* let (ts, pos) = skip ts pos in (\* a *\) *)
  let (ts, pos, var_name) = get_value ts pos in

  if (List.hd ts).value = "=" then
    (
      let (ts, pos) = consume ts pos "=" in
      let (ts, pos, expr) = parse_expr ts pos in
      let (ts, pos) = consume ts pos ";" in
      (
        ts,
        pos,
        ListNode [
            StrNode "var";
            StrNode var_name;
            expr
          ]
      )
    )
  else
    (
      let (ts, pos) =  consume ts pos ";" in
      (
        ts,
        pos,
        ListNode [
            StrNode "var";
            StrNode var_name
          ]
      )
    )

let parse_stmt ts pos: (token list * int * t_node) =
  (* pkv_i "-->> parse_stmt" 0; *)
  match (List.hd ts).value with
  | "set" -> parse_set ts pos
  | "_cmt" -> parse_vm_comment ts pos
  | "call" -> parse_call ts pos
  | "return" -> parse_return ts pos
  | "call_set" -> parse_call_set ts pos
  | _ -> raise (Panic (Printf.sprintf "135 %s" (List.hd ts).value))

let parse_stmts ts pos =
  (* pkv_i "-->> parse_stmts" 0; *)
  let rec iter_parse_stmts ts pos =
    match (List.hd ts).value with
    | "set" -> (ts, pos, [])
    | _ -> raise (Panic "61")
  in
  iter_parse_stmts ts pos

let parse_func_def_body ts pos: (token list * int * t_node) =
  let rec iter ts pos: (token list * int * t_node list) =
    (
    if (List.hd ts).value = "}" then (
      (ts, pos, [])
    ) else if (List.hd ts).value = "var" then
      let (ts, pos, stmt) = (parse_var ts pos) in
      let (ts, pos, tail) = iter ts pos in
      (ts, pos, stmt :: tail)
    else
      (* (
       *   pkv_i "-->> 80" 0;
       *   raise (Panic "85")
       * ) *)
      let (ts, pos, stmt) = (parse_stmt ts pos) in
      let (ts2, pos2, tail) = iter ts pos in
      (ts2, pos2, stmt :: tail)
    )
  in

  let (ts, pos, stmts) = iter ts pos in
  (
    ts,
    pos,
    ListNode stmts
  )

let parse_func_def tokens pos: (token list * t_node list) =
  let (ts, pos) = consume tokens pos "func" in
  (* pkv_i "pos 51" pos; *)
  let (ts, pos, fn_name) = get_value ts pos in
  (* pkv_i "pos 52" pos; *)
  let (ts, pos) = consume ts pos "(" in
  let (ts, pos, args) = parse_args ts pos in
  let (ts, pos) = consume ts pos ")" in
  let (ts, pos) = consume ts pos "{" in

  let (ts, pos, stmts) = parse_func_def_body ts pos in

  let (ts, pos) = consume ts pos "}" in
  (* pkv_i "-->> 185" (List.length ts); *)
  (
    ts,
    [
      StrNode "func";
      StrNode fn_name;
      ListNode args;
      stmts
    ]
  )

let parse_top_stmt (tokens : token list) pos: t_node list =
  (* pkv_i "-->> parse_top_stmt" (List.length tokens); *)
  let rec iter ts: t_node list =
    match ts with
    | [] -> []
    | hd :: tl ->
       (
         (* pkv_i "-->> parse_top_stmt iter" (List.length ts); *)
         match hd.value with
         | "func" -> (
           let (ts, fn_def) = parse_func_def ts pos in
           (ListNode fn_def) :: (iter ts)
         )
         | _ -> raise (Panic "225")
       )
  in
  iter tokens

let parse_top_stmts tokens pos: t_node list =
  (StrNode "top_stmts") ::
    (parse_top_stmt tokens pos)

let () =
  let tokens = read_tokens () in
  let top_stmts = parse_top_stmts tokens 0 in
  Json.print top_stmts
