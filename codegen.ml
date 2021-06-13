open Utils
open Types
open Json

let _gen_expr_add () =
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  add_ab\n"

let rec _gen_expr_binary (binop : t_list) =
  match binop with
  | [
      StrNode op_str;
      expr_l;
      expr_r
    ] ->
     (
       gen_expr expr_l;
       print_string "  push reg_a\n";
       gen_expr expr_r;
       print_string "  push reg_a\n";
       match op_str with
       | "+" -> _gen_expr_add ()
       | _ -> raise (Panic2 "24 not supported")
     )
  | _ -> raise (Panic2 "26 invalid expression")

and gen_expr (expr : t_node) =
  match expr with
  | IntNode n -> print_string (Printf.sprintf "  cp %d reg_a\n" n)
  | StrNode s ->
     (
       let disp = -1 in
       print_string (Printf.sprintf "  cp [bp:%d] reg_a\n" disp)
     )
  | ListNode xs -> _gen_expr_binary xs

let gen_set stmt =
  match stmt with
  | _ :: StrNode var_name :: expr ->
     (
       (* print_string "  cp 42 reg_a\n"; *)
       gen_expr (List.hd expr);
       print_string "  cp reg_a [bp:-1]\n"
     )
  | _ -> raise (Panic2 "46")

let _gen_call_push_args args =
  List.iter
    (fun arg -> (
       gen_expr arg;
       print_string "  push reg_a\n"
    ))
    (List.rev args)

let gen_call stmt =
  match stmt with
  | StrNode "call" :: StrNode fn_name :: args
    ->
     (
       _gen_call_push_args args;

       print_string "  _cmt call~~sub\n";
       print_string "  call sub\n";
       print_string (
           Printf.sprintf
             "  add_sp %d\n"
             (List.length args)
         )
     )
  | _ -> raise (Panic2 "25")

(* call set *)
(* return *)
(* while *)
(* case *)

let gen_vm_comment () =
  print_string "  _cmt vm~comment\n" (* TODO *)

let gen_stmt (stmt : t_list) =
  match List.hd stmt with
  | StrNode s -> (
    match s with
    | "set" -> gen_set stmt
    | "_cmt" -> gen_vm_comment ()
    | "call" -> gen_call stmt
    | _ -> raise (Panic2 "93 not supported")
  )
  | _ -> raise (Panic2 "95 must not happen")

(* stmts *)

let gen_var (stmt : t_list) =
  print_string "  sub_sp 1\n";
  if (List.length stmt) = 3 then
    (
      (* pkv_i "-- 56 1" 0; *)
      let expr = List.nth stmt 2 in
      (* pkv_i "-- 56 2" 0; *)
      gen_expr expr;
      print_string "  cp reg_a [bp:-1]\n";
    )

let gen_func_def_body (stmts : t_list) =
  let rec iter (stmts : t_list) =
    match stmts with
    | [] -> ()
    | ListNode stmt :: tl -> (
      match List.hd stmt with
      | StrNode s -> (
        if s = "var" then (
          gen_var stmt
        ) else (
          gen_stmt stmt;
        );
        iter tl
      )
      | _ -> raise (Panic2 "12")
    (* pkv_i "stmts size" (List.length stmts); *)
    )
    | _ -> raise (Panic2 "127")
  in

  iter stmts

let gen_func_def func_def =
  match func_def with
  | ListNode [
      StrNode "func";
      StrNode fn_name;
      ListNode arg_names;
      ListNode stmts
    ] ->
     (
       print_string (Printf.sprintf "label %s\n" fn_name);
       print_string "  push bp\n";
       print_string "  cp sp bp\n";

       gen_func_def_body stmts;

       print_string "  cp bp sp\n";
       print_string "  pop bp\n";
       print_string "  ret\n"
     )
  | _ -> raise (Panic2 "102")

let gen_top_stmts top_stmts =
  List.iter gen_func_def top_stmts

let codegen ast =
  print_string "  call main\n";
  print_string "  exit\n";
  let top_stmts = List.tl ast in
  gen_top_stmts top_stmts

let () =
  let ast_src = read_stdin_all () in
  let ast = Json.parse ast_src in
  codegen ast
