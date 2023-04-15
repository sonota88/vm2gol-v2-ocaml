open Utils
open Types

type t_env = {
    fn_arg_names : string list;
    lvar_names : string list;
    label_id : int
  }

let index xs x: int option =
  let rec iter xs x i =
    if xs = [] then
      None
    else
      match xs with
      | hd :: tl ->
         (
           if hd = x then
             Some i
           else
             iter tl x (i + 1)
         )
      | _ -> failwith "index: must not happen"
  in
  iter xs x 0

let lvar_disp lvar_names lvar_name: int =
  match index lvar_names lvar_name with
  | Some(i) -> -(i + 1)
  | _ -> failwith "lvar_disp: no such local variable"

let fn_arg_disp fn_arg_names fn_arg_name: int =
  match index fn_arg_names fn_arg_name with
  | Some(i) -> i + 2
  | _ -> failwith "fn_arg_disp: no such function argument"

(* -------------------------------- *)

let _gen_vm_comment comment =
  let replaced = Str.global_replace (Str.regexp " ") "~" comment in
  print_string ("  _cmt " ^ replaced ^ "\n")

let gen_vm_comment stmt =
  match stmt with
  | [ StrNode("_cmt"); StrNode(comment) ]
    -> _gen_vm_comment comment
  | _ -> failwith "gen_vm_comment: must not happen"

let _gen_expr_add () =
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  add_ab\n"

let _gen_expr_mult () =
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  mult_ab\n"

let _gen_expr_eq env: t_env =
  let label_id = env.label_id in
  let env = { env with label_id = env.label_id + 1 } in
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  compare\n";
  print_string (Printf.sprintf "  jump_eq then_%d\n" label_id);
  print_string "  cp 0 reg_a\n";
  print_string (Printf.sprintf "  jump end_eq_%d\n" label_id);
  print_string (Printf.sprintf "label then_%d\n" label_id);
  print_string "  cp 1 reg_a\n";
  print_string (Printf.sprintf "label end_eq_%d\n" label_id);
  env

let _gen_expr_neq env: t_env =
  let label_id = env.label_id in
  let env = { env with label_id = env.label_id + 1 } in
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  compare\n";
  print_string (Printf.sprintf "  jump_eq then_%d\n" label_id);
  print_string "  cp 1 reg_a\n";
  print_string (Printf.sprintf "  jump end_neq_%d\n" label_id);
  print_string (Printf.sprintf "label then_%d\n" label_id);
  print_string "  cp 0 reg_a\n";
  print_string (Printf.sprintf "label end_neq_%d\n" label_id);
  env

let rec _gen_expr_binary env (binop : t_list) : t_env =
  match binop with
  | [StrNode(op_str); expr_l; expr_r] ->
     (
       let env = gen_expr env expr_l in
       print_string "  push reg_a\n";
       let env = gen_expr env expr_r in
       print_string "  push reg_a\n";
       match op_str with
       | "+"  -> _gen_expr_add (); env
       | "*"  -> _gen_expr_mult (); env
       | "==" -> _gen_expr_eq env
       | "!=" -> _gen_expr_neq env
       | _ -> failwith "unsupported binop"
     )
  | _ -> failwith "_gen_expr_binary: must not happen"

and gen_expr env (expr : t_node) : t_env =
  match expr with
  | IntNode(n) ->
     (
       print_string (Printf.sprintf "  cp %d reg_a\n" n);
       env
     )
  | StrNode(s) ->
     (
       if (List.mem s env.lvar_names) then
         (
           let disp = lvar_disp env.lvar_names s in
           print_string (Printf.sprintf "  cp [bp:%d] reg_a\n" disp);
           env
         )
       else if (List.mem s env.fn_arg_names) then
         (
           let disp = fn_arg_disp env.fn_arg_names s in
           print_string (Printf.sprintf "  cp [bp:%d] reg_a\n" disp);
           env
         )
       else
         failwith "gen_expr: failed to look up identifier"
     )
  | ListNode(xs) -> _gen_expr_binary env xs

let _gen_set env var_name expr: t_env =
  let env = gen_expr env expr in
  if List.mem var_name env.lvar_names then
    (
      let disp = lvar_disp env.lvar_names var_name in
      print_string (Printf.sprintf "  cp reg_a [bp:%d]\n" disp);
      env
    )
  else
    failwith "_gen_set: no such local variable"

let gen_set env stmt: t_env =
  match stmt with
  | [ StrNode("set"); StrNode(var_name); expr ] ->
     _gen_set env var_name expr
  | _ -> failwith "gen_set: unexpected pattern"

let _gen_call_push_args env args: t_env =
  List.iter
    (fun arg -> (
       ignore (gen_expr env arg);
       print_string "  push reg_a\n"
    ))
    (List.rev args);
  env

let _gen_funcall env funcall =
  match funcall with
  | StrNode(fn_name) :: args ->
     (
       let env = _gen_call_push_args env args in
       _gen_vm_comment ("call  " ^ fn_name);
       print_string ("  call " ^ fn_name ^ "\n");
       print_string (
           Printf.sprintf
             "  add_sp %d\n"
             (List.length args)
         );
       env
     )
  | _ -> failwith "161"


let gen_call env stmt =
  match stmt with
  | StrNode("call") :: funcall ->
     _gen_funcall env funcall
  | _ -> failwith "168"

let gen_call_set env (stmt : t_list) =
  match stmt with
  | [ StrNode("call_set"); StrNode(var_name); ListNode(funcall) ] ->
     (
       let env = _gen_funcall env funcall in
       if List.mem var_name env.lvar_names then
         (
           let disp = lvar_disp env.lvar_names var_name in
           print_string (Printf.sprintf "  cp reg_a [bp:%d]\n" disp);
           env
         )
       else
         failwith "gen_call_set: must not happen"
     )
  | _ -> failwith "gen_call_set: unexpected pattern: stmt"

let gen_return env stmt =
  match stmt with
  | [ StrNode("return"); expr ] ->
     gen_expr env expr
  | _ -> failwith "gen_return: must not happen"

let rec gen_while env stmt =
  match stmt with
  | [ StrNode("while"); expr; ListNode(stmts) ] ->
     (
       let label_id = env.label_id in
       let env = { env with label_id = env.label_id + 1 } in

       let label_begin = Printf.sprintf "while_%d" label_id in
       let label_end = Printf.sprintf "end_while_%d" label_id in

       print_string (Printf.sprintf "label %s\n" label_begin);

       let env = gen_expr env expr in

       print_string "  cp 0 reg_b\n";
       print_string "  compare\n";
       print_string (Printf.sprintf "  jump_eq %s\n" label_end);

       let env = gen_stmts env stmts in

       print_string (Printf.sprintf "  jump %s\n" label_begin);
       print_string (Printf.sprintf "label %s\n" label_end);

       env
     )
  | _ -> failwith "gen_while: must not happen"

and gen_case env stmt =
  let rec gen_when_clauses env (when_clauses : t_list) case_id when_id =
    match when_clauses with
    | [] -> env
    | ListNode(when_clause) :: rest ->
       (
         match when_clause with
         | expr :: ListNode(stmts) :: [] ->
            (
              let label_end_case = Printf.sprintf "end_case_%d" case_id in
              let label_when = Printf.sprintf "when_%d_%d" case_id when_id in
              let label_end_when = Printf.sprintf "end_when_%d_%d" case_id when_id in

              let env = gen_expr env expr in

              print_string "  cp 1 reg_b\n";
              print_string "  compare\n";
              print_string (Printf.sprintf "  jump_eq %s\n" label_when);
              print_string (Printf.sprintf "  jump %s\n" label_end_when);

              print_string (Printf.sprintf "label %s\n" label_when);

              let env = gen_stmts env stmts in

              print_string (Printf.sprintf "  jump %s\n" label_end_case);
              print_string (Printf.sprintf "label %s\n" label_end_when);
              let env = gen_when_clauses env rest case_id (when_id + 1) in
              env
            )
         | _ -> failwith "gen_when_clauses: must not happen"
       )
    | _ -> failwith "gen_when_clauses: must not happen"
  in

  match stmt with
  | StrNode("case") :: when_clauses ->
     (
       let label_id = env.label_id in
       let env = { env with label_id = env.label_id + 1 } in

       let label_end = Printf.sprintf "end_case_%d" label_id in

       let env = gen_when_clauses env when_clauses label_id 0 in

       print_string (Printf.sprintf "label %s\n" label_end);
       env
     )
    | _ -> failwith "gen_case: must not happen"

and gen_stmt env (stmt : t_list) : t_env =
  match List.hd stmt with
  | StrNode(s) -> (
    match s with
    | "set"      -> gen_set      env stmt
    | "call"     -> gen_call     env stmt
    | "return"   -> gen_return   env stmt
    | "call_set" -> gen_call_set env stmt
    | "while"    -> gen_while    env stmt
    | "case"     -> gen_case     env stmt
    | "_cmt"     -> gen_vm_comment stmt; env
    | _ -> failwith "gen_stmt: unexpected token"
  )
  | _ -> failwith "gen_stmt: must not happen"

and gen_stmts env (stmts : t_list) =
  let rec iter env (stmts : t_list) =
    match stmts with
    | [] -> env
    | ListNode(stmt) :: rest ->
       (
         let env = gen_stmt env stmt in
         iter env rest
       )
    | _ -> failwith "gen_stmts must not happen"
  in
  iter env stmts

let gen_var env (stmt : t_list) : t_env =
  print_string "  sub_sp 1\n";
  match stmt with
  | [ StrNode("var"); StrNode(lvar_name); expr ] ->
     _gen_set env lvar_name expr
  | [ StrNode("var"); StrNode(_) ] -> env
  | _ -> failwith "gen_var: must not happen"

let gen_func_def_body env (stmts : t_list): t_env =
  let rec iter env (stmts : t_list): t_env =
    match stmts with
    | [] -> env
    | ListNode(stmt) :: tl -> (
      match stmt with
      | StrNode("var") :: StrNode(lvar_name) :: _ ->
         let lvar_names_next = env.lvar_names @ [lvar_name] in
         let env = { env with lvar_names = lvar_names_next } in
         let env = gen_var env stmt in
         iter env tl
      | _ ->
         let env = gen_stmt env stmt in
         iter env tl
    )
    | _ -> failwith "gen_func_def_body: must not happen"
  in

  iter env stmts

let gen_func_def env func_def: t_env =
  match func_def with
  | [
      StrNode("func");
      StrNode(fn_name);
      ListNode(fn_arg_names);
      ListNode(stmts)
    ] ->
     (
       let fn_arg_names = (
           List.map
             (fun sn ->
               match sn with
               | StrNode(s) -> s
               | _ -> failwith "gen_func_def: must not happen"
             )
             fn_arg_names
         )
       in
       let env = {
           env with
           fn_arg_names = fn_arg_names;
           lvar_names = []
         } in

       print_string (Printf.sprintf "label %s\n" fn_name);
       print_string "  push bp\n";
       print_string "  cp sp bp\n";

       let env = gen_func_def_body env stmts in

       print_string "  cp bp sp\n";
       print_string "  pop bp\n";
       print_string "  ret\n";

       env
     )
  | _ -> failwith "gen_func_def: must not happen"

let gen_top_stmts top_stmts: unit =
  let rec iter env top_stmts: t_env =
    match top_stmts with
    | [] -> env
    | ListNode(stmt) :: rest_stmts ->
       let env = gen_func_def env stmt in
       iter env rest_stmts
    | _ -> failwith "gen_top_stmts: must not happen"
  in
  let initial_env = {
      fn_arg_names = [];
      lvar_names = [];
      label_id = 1
    } in
  ignore (iter initial_env top_stmts);
  ()

let codegen ast =
  print_string "  call main\n";
  print_string "  exit\n";
  let top_stmts = List.tl ast in
  gen_top_stmts top_stmts

let () =
  let ast_src = read_stdin_all () in
  let ast = Json.parse ast_src in
  codegen ast
