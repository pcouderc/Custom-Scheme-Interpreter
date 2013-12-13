open Ast
open Heap
open Util

let rec eval k (ast : expr) (env : env) : value =
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
  | Cons_e (x, y) -> Cons ((eval ast x env),(eval ast y env))
  | Let_e (x, e1, e2) -> let newenv=(bind x (eval ast e1 env) env) in
                         (eval ast e2 newenv)
  | Letrec_e (x, e1, e2) ->  let newenv = (bind x (Undef) env) in
                             update x (eval ast e1 newenv) newenv; (eval ast e2 newenv)
  | If_e (b, e1, e2) ->
    (match (eval ast b env) with
    | Bool bee -> if (bee) then (eval ast e1 env) else (eval ast e2 env)
    | _ -> runtime "No bool case matched for if")
  | Apply_e (e1, es) ->
    let foo acc ele = apply ast acc (eval ast ele env) in
    let res = List.fold_left foo (eval ast e1 env) es in
    (match res with
    | Closure(Fun_e(xs,e),env) ->
      (match xs with
      | [] -> eval ast e env
      | _ -> res)
    | _ -> runtime "No closure match 2")
  | Fun_e (xs, e) ->
    Closure (Fun_e(xs,e),env)
  | Binop_e (op, e1, e2) ->
    let res1 = eval ast e1 env in
    let res2 = eval ast e2 env in
      apply_binop op res1 res2
  | Unop_e (op, e) -> apply_unop op (eval ast e env)
  | Delayed_e (ex) -> Closure (ex,env);
  | Forced_e (del_expr) ->
    let res = (eval ast del_expr env) in
    (match res with
      | Closure(a,b) -> eval ast a b
      | _ -> res)
  | Quote_e expr -> begin match expr with | Int_e _ | Str_e _ | Bool_e _ |
      Nil_e -> eval ast expr env | _ -> Ast expr end
  | Eval_e expr ->
    begin match eval ast expr env with Ast e -> eval ast e env | e -> e end
  | Callcc_e expr ->
    begin match eval ast expr env with
      Closure (Fun_e ([k], e), env) -> assert false
    | _ -> runtime "argument type must be  ('a -> 'b) "
    end
  | _ -> runtime "No implemented yet"


and apply ast (f : value) (v : value) : value =
 (match f with
   | Closure(Fun_e(xs,e),env) -> (match xs with
       | [] -> eval ast e env
       | idhd::idtl ->
         let newenv = bind idhd v env in Closure(Fun_e(idtl,e),newenv) )
   | _ -> runtime "No closure match")


and apply_binop (op : op) (v1 : value) (v2 : value) : value =
  match op with
    Plus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m + n)
        | _ -> runtime "applying + to non-integer")
  | Minus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m - n)
        | _ -> runtime "applying - to non-integer")
  | Mul ->
      (match (v1, v2) with (Int m, Int n) -> Int (m * n)
        | _ -> runtime "applying * to non-integer")
  | Div ->
      (match (v1, v2) with (Int m, Int n) -> Int (m / n)
        | _ -> runtime "applying / to non-integer")
  | Mod ->
      (match (v1, v2) with (Int m, Int n) -> Int (m mod n)
        | _ -> runtime "applying % to non-integer")
  | Eq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m = n)
      | (Str m, Str n) -> Bool (m = n)
      | (Bool m, Bool n) -> Bool (m = n)
      | _ -> runtime "inappropriate comparison with =")
  | Neq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <> n)
      | (Str m, Str n) -> Bool (m <> n)
      | (Bool m, Bool n) -> Bool (m <> n)
      | _ -> runtime "inappropriate comparison with !=")
  | Lt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m < n)
      | (Str m, Str n) -> Bool (m < n)
      | (Bool m, Bool n) -> Bool (m < n)
      | _ -> runtime "inappropriate comparison with <")
  | Leq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <= n)
      | (Str m, Str n) -> Bool (m <= n)
      | (Bool m, Bool n) -> Bool (m <= n)
      | _ -> runtime "inappropriate comparison with <=")
  | Gt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m > n)
      | (Str m, Str n) -> Bool (m > n)
      | (Bool m, Bool n) -> Bool (m > n)
      | _ -> runtime "inappropriate comparison with >")
  | Geq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m >= n)
      | (Str m, Str n) -> Bool (m >= n)
      | (Bool m, Bool n) -> Bool (m >= n)
      | _ -> runtime "inappropriate comparison with >=")
  | And ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m && n)
      | _ -> runtime "applying & to non-boolean")
  | Or ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m || n)
      | _ -> runtime "applying | to non-boolean")
  | _ -> runtime "not a binary operator"

and apply_unop (op : op) (v : value) : value =
  match op with
  | Minus ->
      (match v with Int n -> Int (-n) | _ ->
         runtime "applying - to non-integer")
  | Not ->
      (match v with Bool b -> Bool (not b) | _ ->
         runtime "applying ~ to non-boolean")
  | Car ->
      (match v with Cons (x, y) -> x | _ ->
         runtime "inappropriate argument for car")
  | Cdr ->
      (match v with Cons (x, y) -> y | _ ->
         runtime "inappropriate argument for cdr")
  | Null ->
      (match v with Cons (x, y) -> Bool false
       | Nil -> Bool true
       | _ -> runtime "inappropriate argument for null")
  | Load -> runtime "load may only occur at top level"
  | _ -> runtime "not a unary operator"


and to_continuation first env ast =
  match ast with
  | Int_e _ | Str_e _ | Bool_e _
  | Def_e _ | Defrec_e _ | Nil_e | Id_e _ -> [ast]
  | Cons_e (x, y) -> assert false
  | Let_e (x, e1, e2) -> assert false
  | Letrec_e (x, e1, e2) -> assert false
  | If_e (b, e1, e2) -> assert false
  | Apply_e (e1, es) -> assert false
  | Fun_e (xs, e) -> [ast]
  | Binop_e (op, e1, e2) ->
    (Fun_e (["x"], Binop_e (op, Id_e "x", e2))) ::
      [Fun_e (["x"], Binop_e (op, e1, Id_e "x"))]
  | Unop_e (op, e) -> [Fun_e (["x"], Unop_e (op, (Id_e "x")))]
  | Delayed_e (ex) -> [Fun_e (["x"], Delayed_e (Id_e "x"))]
  | Forced_e (del_expr) -> [Fun_e (["x"], Forced_e (Id_e "x"))]
  | Quote_e expr -> [ast]
  | Eval_e expr -> [Fun_e (["x"], Eval_e (Id_e "x"))]
  | Callcc_e expr -> [Fun_e (["x"], Callcc_e (Id_e "x"))]
  | K -> assert false

(** Somes tests to represent and evaluate using continuation *)

and cont heap k env =
  (* Format.printf "%s@." @@ value_list_to_string heap; *)
  match k with
  | [] -> List.hd heap
  | ast :: k -> eval_with_k heap k ast env

and eval_with_k heap k ast env =
  match ast with
  | K -> runtime "K shouldn't be evaluated"
  | Int_e n -> cont ((Int n) :: heap) k env
  | Str_e s -> cont ((Str s) :: heap) k env
  | Bool_e b -> cont ((Bool b) :: heap) k env
  | Def_e _ -> runtime "define may occur at top level only"
  | Defrec_e _ -> runtime "definerec may occur at top level only"
  | Nil_e -> cont (Nil :: heap) k env
  | Id_e id ->
    begin
      match (lookup id env) with
      | None -> cont (Nil :: heap) k env
      | Some v -> cont (v :: heap) k env
    end

  | Cons_e (K, K) ->
    let y = List.hd heap in
    let x = List.hd @@ List.tl heap in
    let heap = List.tl @@ List.tl heap in
    cont (Cons (x, y) :: heap) k env
  | Cons_e (K, y) ->
    eval_with_k heap (Cons_e (K, K) :: k) y env
  | Cons_e (x, y) ->
    eval_with_k heap (Cons_e (K, y) :: k) x env

  | Let_e (x, K, K) ->
    cont heap k env
  | Let_e (x, K, e2) ->
    let e = List.hd heap in
    let heap = List.tl heap in
    let newenv=(bind x e env) in
    eval_with_k heap (Let_e (x, K, K) :: k) e2 newenv
  | Let_e (x, e1, e2) ->
    eval_with_k heap (Let_e (x, K, e2) :: k) e1 env

  | If_e (K, e1, e2) ->
    let b = List.hd heap in
    let heap = List.tl heap in
    begin
      match b with
      | Bool bee ->
        if bee then
          eval_with_k heap k e1 env
        else
          eval_with_k heap k e2 env
      | _ -> runtime "No bool case matched for if"
    end
  | If_e (b, e1, e2) ->
    eval_with_k heap (If_e (K, e1, e2) :: k) b env

  | Apply_e (K, [K]) -> assert false
  | Apply_e (K, es) ->
    let foo acc ele = apply_with_k heap k acc (eval_with_k heap k ele env) in
    let f = List.hd heap in
    let heap = List.tl heap in
    let res = List.fold_left foo f es in
    (match res with
    | Closure(Fun_e(xs,e),env) ->
      (match xs with
      | [] -> eval_with_k heap k e env
      | _ -> res)
    | _ -> runtime "No closure match 2")
  | Apply_e (e1, es) ->
    eval_with_k heap (Apply_e (K, es) :: k) e1 env

  | Letrec_e (x, e1, e2) ->
    Format.printf "Not evaluated with continuation yet@.";
    let newenv = (bind x (Undef) env) in
    update x (eval ast e1 newenv) newenv; (eval ast e2 newenv)

  | Fun_e (xs, e) ->
    Closure (Fun_e(xs,e),env)

  | Binop_e (op, K, K) ->
    let y = List.hd heap in
    let x = List.hd @@ List.tl heap in
    let heap = List.tl @@ List.tl heap in
    let res = apply_binop op x y in
    cont (res :: heap) k env
  | Binop_e (op, K, y) ->
    eval_with_k heap (Binop_e (op, K, K) :: k) y env
  | Binop_e (op, x, y) ->
    eval_with_k heap (Binop_e (op, K, y) :: k) x env

  | Unop_e (op, K) ->
    let x = List.hd heap in
    let heap = List.tl heap in
    let res = apply_unop op x in
    cont (res :: heap) k env
  | Unop_e (op, e) ->
    eval_with_k heap (Unop_e (op, K) ::k) e env

  | Delayed_e (ex) -> Closure (ex,env)
  | Forced_e (del_expr) ->
    let res = (eval_with_k heap k del_expr env) in
    (match res with
      | Closure(a,b) -> eval_with_k heap k a b
      | _ -> res)

  | Quote_e K ->
    cont heap k env
  | Quote_e expr ->
    begin
      match expr with
      | Int_e _ | Str_e _ | Bool_e _ |  Nil_e ->
        eval_with_k heap (Quote_e K :: k) expr env
      | _ -> cont (Ast expr :: heap) k env
    end

  | Eval_e K ->
    begin
      match List.hd heap with
        Ast e -> eval_with_k heap k e env
      | e -> cont heap k env
    end
  | Eval_e expr ->
    eval_with_k heap (Eval_e K :: k) expr env

  | Callcc_e expr ->
    begin match eval ast expr env with
      Closure (Fun_e ([k], e), env) -> assert false
    | _ -> runtime "argument type must be  ('a -> 'b) "
    end
  (* | _ -> runtime "No implemented yet" *)

and apply_with_k heap k (f : value) (v : value) : value =
 (match f with
   | Closure(Fun_e(xs,e),env) -> (match xs with
       | [] -> eval_with_k heap k e env
       | idhd::idtl ->
         let newenv = bind idhd v env in Closure(Fun_e(idtl,e),newenv) )
   | _ -> runtime "No closure match")


let eval _ e env =
  eval_with_k [] [] e env
