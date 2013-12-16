open Ast
open Util

type value =
  | Int of int
  | Str of string
  | Ast of expr
  | Bool of bool
  | Closure of expr * env
  | Cont of value list * expr list * env
  | Cons of value * value
  | Nil
  | Undef
and binding = id * value ref
and env = binding list

let is_cont = function
  | Cont (_,_,_) -> true
  | _ -> false

(* lookup a value in the environment *)
let lookup (x : id) (env : env) : value option =
 let filtlist = List.filter (fun y->let (id,value)=y in (id=x)) env in
  (match filtlist with
   | [] -> None
   | hd::tl -> Some !(snd(hd)))


(* update binding x to value v in the environment *)
let update (x : id) (v : value) (env : env) : unit =
 let filtlist = List.filter (fun y->let (id,value)=y in (id=x)) env in
  (match filtlist with
   | [] -> runtime "updating nothing"
   | hd::tl -> snd(hd):=v)

(* create new binding, append to environment *)
let bind (x : id) (v : value) (env : env) : env =
 (x,ref v)::env



let rec value_to_string (x : value) : string =
  let rec cons_to_string (z : value) : string list =
    match z with
    | Nil -> [];
    | Cons (x, y) -> value_to_string x :: cons_to_string y
    | y -> ["."; value_to_string y]
  in
    match x with
    | Int n -> string_of_int n
    | Str s -> s
    | Ast expr -> "'" ^ Ast.to_string expr
    | Bool b -> string_of_bool b
    | Closure (a,b) -> "<fun>"
    | Cont (_,_,_) -> "<k>"
    | Nil -> "()"
    | Cons  _ -> listify (cons_to_string x)
    | Undef -> failwith "Oppan"

let value_list_to_string vl =
  let rec step = function
    | [] -> ""
    | v :: l -> value_to_string v ^ "; " ^ step l
  in
  "[" ^ step vl ^ "]"


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
    "set!", ref (Closure (Fun_e (["a"; "b"], Set_e (Id_e "a", Id_e "b")), []));
    ]
