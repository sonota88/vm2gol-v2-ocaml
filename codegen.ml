open Utils
open Types
open Json

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
      | _ -> raise (Panic "13 must not happen")
  in
  iter xs x 0

let lvar_disp lvar_names lvar_name: int =
  match index lvar_names lvar_name with
  | Some i -> -(i + 1)
  | _ -> raise (Panic "25 must not happen")

let fn_arg_disp fn_arg_names fn_arg_name: int =
  match index fn_arg_names fn_arg_name with
  | Some i -> i + 2
  | _ -> raise (Panic "30 must not happen")

(* -------------------------------- *)

let _gen_expr_add () =
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  add_ab\n"

let _gen_expr_eq () =
  let label_id = 1 in
  print_string "  pop reg_b\n";
  print_string "  pop reg_a\n";
  print_string "  compare\n";
  print_string (Printf.sprintf "  jump_eq then_%d\n" label_id);
  print_string "  cp 0 reg_a\n";
  print_string (Printf.sprintf "  jump end_eq_%d\n" label_id);
  print_string (Printf.sprintf "label then_%d\n" label_id);
  print_string "  cp 1 reg_a\n";
  print_string (Printf.sprintf "label end_eq_%d\n" label_id)

let rec _gen_expr_binary
          (fn_arg_names : string list)
          (lvar_names : string list)
          (binop : t_list)
  =
  match binop with
  | [
      StrNode op_str;
      expr_l;
      expr_r
    ] ->
     (
       gen_expr fn_arg_names lvar_names expr_l;
       print_string "  push reg_a\n";
       gen_expr fn_arg_names lvar_names expr_r;
       print_string "  push reg_a\n";
       match op_str with
       | "+" -> _gen_expr_add ()
       | "==" -> _gen_expr_eq ()
       | _ -> raise (Panic "24 not supported")
     )
  | _ -> raise (Panic "26 invalid expression")

and gen_expr
(fn_arg_names : string list)
(lvar_names : string list)
(expr : t_node) =
  match expr with
  | IntNode n -> print_string (Printf.sprintf "  cp %d reg_a\n" n)
  | StrNode s ->
     (
       if (List.mem s lvar_names) then
         let disp = lvar_disp lvar_names s in
         print_string (Printf.sprintf "  cp [bp:%d] reg_a\n" disp)
       else if (List.mem s fn_arg_names) then
         let disp = fn_arg_disp fn_arg_names s in
         print_string (Printf.sprintf "  cp [bp:%d] reg_a\n" disp)
       else
         raise (Panic "61 failed to look up identifier")
     )
  | ListNode xs -> _gen_expr_binary fn_arg_names lvar_names xs

let gen_set fn_arg_names lvar_names stmt =
  match stmt with
  | _ :: StrNode var_name :: expr ->
     (
       (* print_string "  cp 42 reg_a\n"; *)
       gen_expr fn_arg_names lvar_names (List.hd expr);
       print_string "  cp reg_a [bp:-1]\n"
     )
  | _ -> raise (Panic "46")

let _gen_call_push_args fn_arg_names lvar_names args =
  List.iter
    (fun arg -> (
       gen_expr fn_arg_names lvar_names arg;
       print_string "  push reg_a\n"
    ))
    (List.rev args)

let gen_call fn_arg_names lvar_names stmt =
  match stmt with
  | StrNode "call" :: StrNode fn_name :: args
    ->
     (
       _gen_call_push_args fn_arg_names lvar_names args;

       print_string "  _cmt call~~sub\n";
       print_string "  call sub\n";
       print_string (
           Printf.sprintf
             "  add_sp %d\n"
             (List.length args)
         )
     )
  | _ -> raise (Panic "96")

let gen_call_set fn_arg_names lvar_names (stmt : t_list) =
  match stmt with
  | StrNode "call_set"
    :: StrNode var_name
    :: ListNode funcall
    :: []
    ->
     (
       match funcall with
         | StrNode fn_name :: args
           ->
            (
              _gen_call_push_args fn_arg_names lvar_names args;
              print_string ("  _cmt call~~" ^ fn_name ^ "\n");
              print_string ("  call " ^ fn_name ^ "\n");
              print_string (
                  Printf.sprintf
                    "  add_sp %d\n"
                    (List.length args)
                );
              let disp = lvar_disp lvar_names var_name in
              print_string (Printf.sprintf "  cp reg_a [bp:%d]\n" disp)
            )
         | _ -> raise (Panic "gen_call_set: unexpected pattern: funcall")
       ;
     )
  | _ -> raise (Panic "gen_call_set: unexpected pattern: stmt")

let gen_return fn_arg_names lvar_names stmt =
  match stmt with
  | StrNode "return" :: expr :: [] ->
     gen_expr fn_arg_names lvar_names expr
  | _ -> raise (Panic "TODO")

(* while *)
(* case *)

let gen_vm_comment () =
  print_string "  _cmt vm~comment\n" (* TODO *)

let gen_stmt fn_arg_names lvar_names (stmt : t_list) =
  match List.hd stmt with
  | StrNode s -> (
    match s with
    | "set" -> gen_set fn_arg_names lvar_names stmt
    | "_cmt" -> gen_vm_comment ()
    | "call" -> gen_call fn_arg_names lvar_names stmt
    | "return" -> gen_return fn_arg_names lvar_names stmt
    | "call_set" -> gen_call_set fn_arg_names lvar_names stmt
    | _ -> raise (Panic "93 not supported")
  )
  | _ -> raise (Panic "95 must not happen")

(* stmts *)

let gen_var fn_arg_names (lvar_names : string list) (stmt : t_list) =
  print_string "  sub_sp 1\n";
  if (List.length stmt) = 3 then
    (
      let expr = List.nth stmt 2 in
      gen_expr fn_arg_names lvar_names expr;
      print_string "  cp reg_a [bp:-1]\n";
    )

let gen_func_def_body fn_arg_names (stmts : t_list) =
  let rec iter (stmts : t_list) lvar_names =
    match stmts with
    | [] -> ()
    | ListNode stmt :: tl -> (
      match List.hd stmt with
      | StrNode s -> (
        if s = "var" then (
          match List.nth stmt 1 with
          | StrNode lvar_name ->
             (
               let lvar_names2 = lvar_name :: lvar_names in
               gen_var fn_arg_names lvar_names2 stmt;
               iter tl lvar_names2
             )
          | _ -> raise (Panic "144 must not happen")
        ) else (
          gen_stmt fn_arg_names lvar_names stmt;
          iter tl lvar_names
        );
      )
      | _ -> raise (Panic "12")
    (* pkv_i "stmts size" (List.length stmts); *)
    )
    | _ -> raise (Panic "127")
  in

  iter stmts []

let gen_func_def func_def =
  match func_def with
  | ListNode [
      StrNode "func";
      StrNode fn_name;
      ListNode fn_arg_names;
      ListNode stmts
    ] ->
     (
       let fn_arg_names = (
           List.map
             (fun sn ->
               match sn with
               | StrNode s -> s
               | _ -> raise (Panic "225")
             )
             fn_arg_names
         )
       in

       print_string (Printf.sprintf "label %s\n" fn_name);
       print_string "  push bp\n";
       print_string "  cp sp bp\n";

       gen_func_def_body fn_arg_names stmts;

       print_string "  cp bp sp\n";
       print_string "  pop bp\n";
       print_string "  ret\n"
     )
  | _ -> raise (Panic "102")

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
