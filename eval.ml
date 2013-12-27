open Ast
open Heap
open Util
open Eval_cont

(** Original eval function, won't work correctly however, some changes has to
    be done to take care of the global environment *)

let rec eval (ast : expr) (env : env) : value =
  match ast with
  | Int_e n -> Int n
  | Str_e s -> Str s
  | Bool_e b -> Bool b
  | Def_e _ -> runtime "define may occur at top level only"
  | Defrec_e _ -> runtime "definerec may occur at top level only"
  | Nil_e -> Nil
  | Id_e id ->
    (match (lookup id env) with
    | None -> Nil
    | Some v -> v)
  | Cons_e (x, y) -> Cons ((eval x env),(eval y env))
  | Let_e (x, e1, e2) -> let newenv=(bind x (eval e1 env) env) in
                         (eval e2 newenv)
  | Letrec_e (x, e1, e2) ->
    let newenv = (bind x (Undef) env) in
    update x (eval e1 newenv) newenv; (eval e2 newenv)
  | If_e (b, e1, e2) ->
    (match (eval b env) with
    | Bool bee -> if (bee) then (eval e1 env) else (eval e2 env)
    | _ -> runtime "No bool case matched for if")
  | Apply_e (e1, es) ->
    let foo acc ele = apply ast acc (eval ele env) in
    let res = List.fold_left foo (eval e1 env) es in
    (match res with
    | Closure(Fun_e(xs,e),env) ->
      (match xs with
      | [] -> eval e env
      | _ -> res)
    | _ -> runtime "No closure match 2")
  | Fun_e (xs, e) ->
    Closure (Fun_e(xs,e),env)
  | Binop_e (op, e1, e2) ->
    let res1 = eval e1 env in
    let res2 = eval e2 env in
      apply_binop op res1 res2
  | Unop_e (op, e) -> apply_unop op (eval e env)
  | Delayed_e (ex) -> Closure (ex,env);
  | Forced_e (del_expr) ->
    let res = (eval del_expr env) in
    (match res with
      | Closure(a,b) -> eval a b
      | _ -> res)
  | Quote_e expr -> begin match expr with | Int_e _ | Str_e _ | Bool_e _ |
      Nil_e -> eval expr env | _ -> Ast expr end
  | Eval_e expr ->
    begin match eval expr env with Ast e -> eval e env | e -> e end
  | Callcc_e expr -> assert false
  | _ -> runtime "No implemented yet"


and apply ast (f : value) (v : value) : value =
 (match f with
   | Closure(Fun_e(xs,e),env) -> (match xs with
       | [] -> eval e env
       | idhd::idtl ->
         let newenv = bind idhd v env in Closure(Fun_e(idtl,e),newenv) )
   | _ -> runtime "No closure match")


(** This way, eval will use the continuation version *)
let eval e env =
  eval_with_k [] [] e env
