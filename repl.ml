open Ast
open Heap
open Eval
open Util


let parse (s : string) : expr list =
  Parser.main Lexer.token (Lexing.from_string s)

let print_bindings =
  List.iter (fun (id, var) ->
    print_endline ("  " ^ id ^ ": " ^ (value_to_string !var)))

let print_help _ =
  print_endline "Available top-level commands:";
  print_endline "  :help       Shows this screen.";
  print_endline "  :bindings   Prints all the bindings in the current environment.";
  print_endline "              Bindings higher on the stack have higher priority.";
  print_endline "  :quit       Leaves the interpreter. You may also use Ctrl-C (Windows)";
  print_endline "              or Ctrl-D (*nix)\n"

let print_value (v : value) : unit =
  print_endline (">> " ^ value_to_string v)

(* read lines from the console, appending them to s,
 * until the user enters a blank line (load*)
let read_console () : string =
  let rec read_lines s =
    let input = read_line() in
    let input = String.trim input in
    if input = "" then s
    else read_lines (s ^ input ^ "\n") in
  read_lines ""

(* read from a file *)
let read_file (filename : string) : string =
  let input_channel =
    try open_in filename
    with Sys_error s -> runtime s in
  let rec read_lines (s : string) : string =
    try read_lines (s ^ (input_line input_channel) ^ "\n")
    with End_of_file -> s in
  read_lines ""

let rec eval_one (env : env) (expr : expr) : env =
  match expr with
      Def_e (xs, e) -> (match xs with
       | x::[] -> print_value (eval Nil_e e env); (bind x (eval Nil_e e env) env)
       | x::tl ->  print_value (eval Nil_e (Fun_e(tl,e)) env);
          (bind x (eval Nil_e (Fun_e(tl,e)) env) env)
       | [] -> runtime "Not possible")
    | Defrec_e (xs, e) -> (match xs with
       | x::[] -> print_value (eval Nil_e e env);
         let newenv = bind x (Undef) env in
           update x (eval Nil_e e newenv) newenv; newenv
       | x::tl -> let newenv = (bind x (Undef) env) in
           print_value (eval Nil_e (Fun_e(tl,e)) env);
           update x (eval Nil_e (Fun_e(tl,e)) newenv) newenv; newenv
       | _ -> runtime "Not possible rec")
    | Unop_e (Load, e) -> (match (eval Nil_e e env) with
       | Str s -> eval_list env (parse (read_file s))
       | _ -> print_string "Not possible load"; env)
    | _ ->
        let v = eval Nil_e expr env in
          print_value v; env

and eval_list (env : env) (expr_list : expr list) : env =
  List.fold_left eval_one env expr_list

let rec repl env global_env =
  print_string "> ";
  try
    let input = read_console() in
    if input = ":help\n" || input = ":h\n"then
      (print_help (); repl env global_env)
    else if input = ":quit\n" || input = ":q\n"then ()
    else if input = ":bindings\n" || input = ":b\n" then
      (print_bindings env; repl env global_env)
    else
      let expr_list = parse input in
      let env = eval_list env expr_list in
      repl env global_env
  with End_of_file -> print_endline ""
    | Runtime s ->
        (print_endline ("Runtime Exception: " ^ s);
         repl env global_env)
    | Parsing.Parse_error ->
        (print_endline "Parse Error";
         repl env global_env)

(* start with empty global environment *)

let global_env = ref
    ["+", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Plus, Id_e "a", Id_e "b")), []));
    "-", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Minus, Id_e "a", Id_e "b")), []));
    "*", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Mul, Id_e "a", Id_e "b")), []));
    "/", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Div, Id_e "a", Id_e "b")), []));
    "=", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Eq, Id_e "a", Id_e "b")), []));
    "!=", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Neq, Id_e "a", Id_e "b")), []));
    ">", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Gt, Id_e "a", Id_e "b")), []));
    ">=", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Geq, Id_e "a", Id_e "b")), []));
    "<", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Lt, Id_e "a", Id_e "b")), []));
    "<=", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Leq, Id_e "a", Id_e "b")), []));
    "&", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (And, Id_e "a", Id_e "b")), []));
    "|", ref (Closure
                 (Fun_e (["a"; "b"], Binop_e (Or, Id_e "a", Id_e "b")), []));
    "call-with-current-continuation", ref (Closure
                 (Fun_e (["a"], Callcc_e (Id_e "a")), []));
    "eval", ref (Closure (Fun_e (["a"], Eval_e (Id_e "a")), []));
    "~", ref (Closure (Fun_e (["a"], Unop_e (Not, Id_e "a")), []));
    "car", ref (Closure (Fun_e (["a"], Unop_e (Car, Id_e "a")), []));
    "cdr", ref (Closure (Fun_e (["a"], Unop_e (Cdr, Id_e "a")), []));
    "null", ref (Closure (Fun_e (["a"], Unop_e (Null, Id_e "a")), []));
    "load", ref (Closure (Fun_e (["a"], Unop_e (Load, Id_e "a")), []));
    "delay", ref (Closure (Fun_e (["a"], Delayed_e (Id_e "a")), []));
    "force", ref (Closure (Fun_e (["a"], Forced_e (Id_e "a")), []));
    "set!", ref (Closure (Fun_e (["a"], Set_e (Id_e "a", Id_e "b")), []));
    ]


let _ = repl [] global_env
