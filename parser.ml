open Utils
open Types

type token = { lineno : int; kind : string; value : string }

let read_tokens (): token list =
  let rec iter_read_tokens () =
    try 
      let line = read_line () in
      let xs = Json.parse line in
      match xs with
      | [
          IntNode(lineno);
          StrNode(kind);
          StrNode(value)
        ] ->
         { lineno; kind; value } :: iter_read_tokens ()
      | _ -> failwith "invalid format"
    with
      End_of_file -> []
  in
    iter_read_tokens ()

let consume tokens value =
  match tokens with
  | [] -> failwith "must not happen"
  | hd :: tl ->
     if hd.value = value then
       tl
     else
       failwith (Printf.sprintf "consume: expected(%s) actual(%s)" value hd.value)

let get_value ts =
  match ts with
  | [] -> failwith "must not happen"
  | hd :: tl -> (tl, hd.value)

let skip ts = List.tl ts

(* -------------------------------- *)

let parse_args ts: (token list * t_list) =
  let rec iter ts =
    if (List.hd ts).value = ")" then
      (ts, [])
    else
      let (ts, arg) =
        match (List.hd ts).kind with
        | "int" ->
           let (ts, s) = get_value ts in
           (
             ts,
             IntNode(int_of_string s)
           )
        | "ident" ->
           let (ts, s) = get_value ts in
           (ts, StrNode(s))
        | _ -> failwith "unexpected token kind"
      in

      if (List.hd ts).value = "," then
        let ts = consume ts "," in
        let (ts, rest_args) = iter ts in
        (ts, (arg :: rest_args))
      else
        (ts, [arg])
  in
  iter ts

let rec _parse_expr_right ts =
  match (List.hd ts).value with
  | "+" | "*" | "==" | "!=" ->
     let (ts, op_str) = get_value ts in
     let (ts, expr_r) = parse_expr ts in
     Some(ts, op_str, expr_r)
  | _ -> None

and _parse_expr_factor ts: (token list * t_node) =
  let t = List.hd ts in
  match t.kind with
  | "sym" ->
     let ts = consume ts "(" in
     let (ts, expr) = parse_expr ts in
     let ts = consume ts ")" in
     (ts, expr)
  | "int" ->
     let ts = skip ts in
     (
       ts,
       IntNode(int_of_string t.value)
     )
  | "ident" ->
     let ts = skip ts in
     (
       ts,
       StrNode(t.value)
     )
  | _ -> failwith "unexpected token kind"

and parse_expr ts: (token list * t_node) =
  let (ts, factor) = _parse_expr_factor ts
  in

  let op_r = _parse_expr_right ts in
  match op_r with
  | Some (ts, op_str, expr_r) ->
     (
       ts,
       ListNode([
             StrNode(op_str);
             factor;
             expr_r
         ])
     )
  | None -> (ts, factor)

let parse_set ts =
  let ts = consume ts "set" in
  let (ts, var_name) = get_value ts in
  let ts = consume ts "=" in
  let (ts, expr) = parse_expr ts in
  let ts = consume ts ";" in
  (
    ts,
    ListNode([
          StrNode("set");
          StrNode(var_name);
          expr
      ])
  )

let parse_funcall ts: (token list * t_list) =
  let fn_name = (List.hd ts).value in
  let ts = skip ts in
  let ts = consume ts "(" in
  let (ts, args) = parse_args ts in
  let ts = consume ts ")" in
  (
    ts,
    StrNode(fn_name) :: args
  )

let parse_call ts =
  let ts = consume ts "call" in
  let (ts, funcall) = parse_funcall ts in
  let ts = consume ts ";" in
  (
    ts,
    ListNode(
        StrNode("call")
        :: funcall
      )
  )

let parse_call_set ts =
  let ts = consume ts "call_set" in
  let (ts, var_name) = get_value ts in
  let ts = consume ts "=" in
  let (ts, funcall) = parse_funcall ts in
  let ts = consume ts ";" in
  (
    ts,
    ListNode(
        StrNode("call_set")
        :: StrNode(var_name)
        :: [ListNode(funcall)]
      )
  )

let parse_return ts =
  let ts = consume ts "return" in

  if (List.hd ts).value = ";" then
    let ts = consume ts ";" in
    (
      ts,
      ListNode([
        StrNode("return")
      ])
    )
  else
    let (ts, expr) = parse_expr ts in
    let ts = consume ts ";" in
    (
      ts,
      ListNode([
            StrNode("return");
            expr
        ])
    )

let parse_vm_comment ts =
  let ts = consume ts "_cmt" in
  let ts = consume ts "(" in
  let cmt = (List.hd ts).value in
  let ts = skip ts in
  let ts = consume ts ")" in
  let ts = consume ts ";" in
  (
    ts,
    ListNode([
        StrNode("_cmt");
        StrNode(cmt)
      ])
  )

let rec parse_while ts =
  let ts = consume ts "while" in
  let ts = consume ts "(" in
  let (ts, expr) = parse_expr ts in
  let ts = consume ts ")" in
  let ts = consume ts "{" in
  let (ts, stmts) = parse_stmts ts in
  let ts = consume ts "}" in
  (
    ts,
    ListNode([
          StrNode("while");
          expr;
          ListNode(stmts)
      ])
  )

and parse_case ts =
  let rec parse_when_clauses ts: (token list * t_list) =
    if (List.hd ts).value = "(" then
      (
        let ts = consume ts "(" in
        let (ts, expr) = parse_expr ts in
        let ts = consume ts ")" in
        let ts = consume ts "{" in
        let (ts, stmts) = parse_stmts ts in
        let ts = consume ts "}" in
        let (ts, when_clauses) = parse_when_clauses ts in
        (
          ts,
          ListNode([
                expr;
                ListNode(stmts)
            ]) :: when_clauses
        )
      )
    else
      (ts, [])
  in

  let ts = consume ts "case" in
  let ts = consume ts "{" in
  let (ts, when_clauses) : (token list * t_list) = parse_when_clauses ts in
  let ts = consume ts "}" in
  (
    ts,
    ListNode(StrNode("case") :: when_clauses)
  )

and parse_stmt ts: (token list * t_node) =
  match (List.hd ts).value with
  | "set"      -> parse_set        ts
  | "_cmt"     -> parse_vm_comment ts
  | "call"     -> parse_call       ts
  | "return"   -> parse_return     ts
  | "call_set" -> parse_call_set   ts
  | "while"    -> parse_while      ts
  | "case"     -> parse_case       ts
  | _ -> failwith (
             Printf.sprintf
               "parse_stmt: unexpected token: %s"
               (List.hd ts).value
           )

and parse_stmts ts: (token list * t_list) =
  let rec iter ts: (token list * t_list) =
    if (List.hd ts).value = "}" then
      (ts, [])
    else
      let (ts, stmt) = parse_stmt ts in
      let (ts, stmts) = iter ts in
      (ts, stmt :: stmts)
  in
  iter ts

let parse_var ts: (token list * t_node) =
  let ts = consume ts "var" in
  let (ts, var_name) = get_value ts in

  if (List.hd ts).value = "=" then
    let ts = consume ts "=" in
    let (ts, expr) = parse_expr ts in
    let ts = consume ts ";" in
    (
      ts,
      ListNode([
            StrNode("var");
            StrNode(var_name);
            expr
        ])
    )
  else
    let ts =  consume ts ";" in
    (
      ts,
      ListNode([
            StrNode("var");
            StrNode(var_name)
        ])
    )

let parse_func_def_body ts: (token list * t_node) =
  let rec iter ts: (token list * t_list) =
    match (List.hd ts).value with
    | "}" -> (ts, [])
    | "var" ->
       let (ts, stmt) = (parse_var ts) in
       let (ts, tail) = iter ts in
       (ts, stmt :: tail)
    | _ ->
       let (ts, stmt) = (parse_stmt ts) in
       let (ts2, tail) = iter ts in
       (ts2, stmt :: tail)
  in

  let (ts, stmts) = iter ts in
  (
    ts,
    ListNode(stmts)
  )

let parse_func_def ts: (token list * t_list) =
  let ts = consume ts "func" in
  let (ts, fn_name) = get_value ts in
  let ts = consume ts "(" in
  let (ts, args) = parse_args ts in
  let ts = consume ts ")" in
  let ts = consume ts "{" in
  let (ts, stmts) = parse_func_def_body ts in
  let ts = consume ts "}" in
  (
    ts,
    [
      StrNode("func");
      StrNode(fn_name);
      ListNode(args);
      stmts
    ]
  )

let parse_top_stmt (ts : token list): t_list =
  let rec iter ts: t_list =
    match ts with
    | [] -> []
    | hd :: tl ->
       (
         match hd.value with
         | "func" ->
            (
              let (ts, fn_def) = parse_func_def ts in
              ListNode(fn_def) :: (iter ts)
            )
         | _ -> failwith "parse_top_stmt: unexpected token"
       )
  in
  iter ts

let parse_top_stmts ts: t_list =
  StrNode("top_stmts") :: (parse_top_stmt ts)

let () =
  let tokens = read_tokens () in
  let top_stmts = parse_top_stmts tokens in
  Json.pretty_print top_stmts
