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
  | Letrec_e (x, e1, e2) ->
    let newenv = (bind x (Undef) env) in
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


(* and to_continuation first env ast = *)
(*   match ast with *)
(*   | Int_e _ | Str_e _ | Bool_e _ *)
(*   | Def_e _ | Defrec_e _ | Nil_e | Id_e _ -> [ast] *)
(*   | Cons_e (x, y) -> assert false *)
(*   | Let_e (x, e1, e2) -> assert false *)
(*   | Letrec_e (x, e1, e2) -> assert false *)
(*   | If_e (b, e1, e2) -> assert false *)
(*   | Apply_e (e1, es) -> assert false *)
(*   | Fun_e (xs, e) -> [ast] *)
(*   | Binop_e (op, e1, e2) -> *)
(*     (Fun_e (["x"], Binop_e (op, Id_e "x", e2))) :: *)
(*       [Fun_e (["x"], Binop_e (op, e1, Id_e "x"))] *)
(*   | Unop_e (op, e) -> [Fun_e (["x"], Unop_e (op, (Id_e "x")))] *)
(*   | Delayed_e (ex) -> [Fun_e (["x"], Delayed_e (Id_e "x"))] *)
(*   | Forced_e (del_expr) -> [Fun_e (["x"], Forced_e (Id_e "x"))] *)
(*   | Quote_e expr -> [ast] *)
(*   | Eval_e expr -> [Fun_e (["x"], Eval_e (Id_e "x"))] *)
(*   | Callcc_e expr -> [Fun_e (["x"], Callcc_e (Id_e "x"))] *)
(*   | K -> assert false *)
(*   | Cont_e _ -> assert false *)
(*   | _ -> runtime "Not implemented yet" *)

(** Somes tests to represent and evaluate using continuation *)

and cont stack k env =
  (* Format.printf "cont: @.stack:%s@.cont:%s@.-----------@." *)
  (*   (value_list_to_string stack) *)
  (*   (ast_list_to_string k); *)
  match k with
  | [] | K :: _ -> List.hd stack
  | ast :: k -> eval_with_k stack k ast env

and eval_with_k stack k ast env =
  (* Format.printf "eval: %s@.stack:%s@.cont:%s@." *)
  (*   (to_string ast) *)
  (*   (value_list_to_string stack) *)
  (*   (ast_list_to_string k); *)
  (* Format.printf "env:"; print_env env; Format.printf"@.-------@."; *)
  match ast with
  | K -> runtime "K shouldn't be evaluated"
  | Int_e n -> cont ((Int n) :: stack) k env
  | Str_e s -> cont ((Str s) :: stack) k env
  | Bool_e b -> cont ((Bool b) :: stack) k env
  | Def_e _ -> runtime "define may occur at top level only"
  | Defrec_e _ -> runtime "definerec may occur at top level only"
  | Nil_e -> cont (Nil :: stack) k env
  | Id_e id ->
    begin
      match (lookup id env) with
      | None ->
        begin match (g_lookup id) with
        | None -> cont (Nil :: stack) k env
        | Some v -> cont (v :: stack) k env
        end
      | Some v -> cont (v :: stack) k env
    end
  | Begin_e [] ->
    cont stack k env
  | Begin_e [K] ->
    cont stack k env
  | Begin_e (K :: l) ->
    let e = List.hd l in
    let l = List.tl l in
    eval_with_k (List.tl stack) ((Begin_e l) :: k) e env
  | Begin_e (e :: l) ->
    eval_with_k stack ((Begin_e (K :: l)) :: k) e env

  | Cons_e (K, K) ->
    let y = List.hd stack in
    let x = List.hd @@ List.tl stack in
    let stack = List.tl @@ List.tl stack in
    cont (Cons (x, y) :: stack) k env
  | Cons_e (K, y) ->
    eval_with_k stack (Cons_e (K, K) :: k) y env
  | Cons_e (x, y) ->
    eval_with_k stack (Cons_e (K, y) :: k) x env

  | Let_e (x, K, K) ->
    cont stack k env
  | Let_e (x, K, e2) ->
    let e = List.hd stack in
    let stack = List.tl stack in
    let newenv=(bind x e env) in
    eval_with_k stack (Let_e (x, K, K) :: k) e2 newenv
  | Let_e (x, e1, e2) ->
    eval_with_k stack (Let_e (x, K, e2) :: k) e1 env

  | If_e (K, e1, e2) ->
    let b = List.hd stack in
    let stack = List.tl stack in
    begin
      match b with
      | Bool bee ->
        if bee then
          eval_with_k stack k e1 env
        else
          eval_with_k stack k e2 env
      | _ -> runtime "No bool case matched for if"
    end
  | If_e (b, e1, e2) ->
    eval_with_k stack (If_e (K, e1, e2) :: k) b env

  | Apply_e (K, [K]) -> assert false
  | Apply_e (K, es) ->
    (* Format.printf "In Apply_e (K, es) case@."; *)
    let f = List.hd stack in
    let stack = List.tl stack in
    let foo acc ele = apply_with_k stack k env acc
      (eval_with_k stack (K :: k) ele env) in
    (* let k = if is_cont f then K :: k else k in *)
    (* Format.printf "fun: %s, stack: %s@." *)
    (*   (value_to_string f) (value_list_to_string stack); *)
    let res = List.fold_left foo f es in
    (* Format.printf "res: %s@." (value_to_string res); *)
    (match res with
    | Closure(Fun_e(xs,e),env) ->
      (match xs with
      | [] -> eval_with_k stack k e env
      | _ -> res)
    | Cont (stack, k, env) ->
      (* Format.printf "In cont case@;";  *)
      cont stack k env
    | _ -> runtime "No closure match 2")
  | Apply_e (e1, es) ->
    eval_with_k stack (Apply_e (K, es) :: k) e1 env



  | Letrec_e (x, K, K) ->
    cont stack k env
  | Letrec_e (x, K, e2) ->
    let e = List.hd stack in
    let stack = List.tl stack in
    update x e env;
    eval_with_k stack (Letrec_e (x, K, K) :: k) e2 env
  | Letrec_e (x, e1, e2) ->
    let env = bind x Undef env in
    eval_with_k stack (Letrec_e (x, K, e2) :: k) e1 env
  (* | Letrec_e (x, e1, e2) -> *)
  (*   Format.printf "Not evaluated with continuation yet@."; *)
  (*   let newenv = (bind x (Undef) env) in *)
  (*   update x (eval ast e1 newenv) newenv; (eval ast e2 newenv) *)

  | Fun_e (xs, e) ->
    cont (Closure (Fun_e(xs,e),env) :: stack) k env

  | Binop_e (op, K, K) ->
    let y = List.hd stack in
    let x = List.hd @@ List.tl stack in
    let stack = List.tl @@ List.tl stack in
    let res = apply_binop op x y in
    cont (res :: stack) k env
  | Binop_e (op, K, y) ->
    eval_with_k stack (Binop_e (op, K, K) :: k) y env
  | Binop_e (op, x, y) ->
    eval_with_k stack (Binop_e (op, K, y) :: k) x env

  | Unop_e (op, K) ->
    let x = List.hd stack in
    let stack = List.tl stack in
    let res = apply_unop op x in
    cont (res :: stack) k env
  | Unop_e (op, e) ->
    eval_with_k stack (Unop_e (op, K) ::k) e env

  | Delayed_e (ex) -> Closure (ex,env)
  | Forced_e (del_expr) ->
    let res = (eval_with_k stack k del_expr env) in
    (match res with
      | Closure(a,b) -> eval_with_k stack k a b
      | _ -> res)

  | Quote_e K ->
    cont stack k env
  | Quote_e expr ->
    begin
      match expr with
      | Int_e _ | Str_e _ | Bool_e _ |  Nil_e ->
        eval_with_k stack (Quote_e K :: k) expr env
      | _ -> cont (Ast expr :: stack) k env
    end

  | Eval_e K ->
    begin
      match List.hd stack with
        Ast e -> eval_with_k (List.tl stack) k e env
      | e -> cont (e :: stack) k env
    end
  | Eval_e expr ->
    eval_with_k stack (Eval_e K :: k) expr env

  | Callcc_e K ->
    let expr = List.hd stack in
    let stack = List.tl stack in
    begin match expr with
      Closure (Fun_e ([i], e) as f, c_env) ->
        let env = bind i (Cont (stack, k, env)) env in
        eval_with_k stack k (Apply_e (f, [Cont_e i])) env
    | _ -> runtime "argument must be of type ('a -> 'b) "
    end
  | Callcc_e expr ->
    eval_with_k stack (Callcc_e K :: k) expr env

  | Set_e (ex1, K) ->
    let id = begin match ex1 with Id_e i -> i
      | _ -> runtime "first argument must be a ident" end in
    let expr = List.hd stack in
    let stack = List.tl stack in
    begin
      match lookup id env with
      | Some _ -> Format.printf "In Local@."; update id expr env
      | None ->
        begin match g_lookup id with
          | Some _ -> g_update id expr
          | None ->
            runtime ("undefined variable " ^ id) end
    end;
    cont (Nil::stack) k env
  | Set_e (ex1, ex2) ->
    eval_with_k stack (Set_e (ex1, K) :: k) ex2 env
  | Cont_e i ->
    let res = match lookup i env with
      | None -> Nil
      | Some e -> e in
    cont (res :: stack) (K :: k) env
  (* | _ -> runtime "No implemented yet" *)

and apply_with_k stack k env (f : value) (v : value) : value =
  (* Format.printf "apply: %s : %s@.stack:%s@.cont:%s@.-----------@." *)
  (*   (value_to_string f) (value_to_string v) *)
  (*   (value_list_to_string stack) *)
  (*   (ast_list_to_string k); *)
 (match f with
   | Closure(Fun_e(xs,e),cenv) -> (match xs with
       | [] -> eval_with_k stack k e env
       | idhd::idtl ->
         let newenv = bind idhd v cenv in
         let newenv = newenv @ env in
         Closure(Fun_e(idtl,e),newenv))
   | Cont (stack, k, env) ->
     (* Format.printf *)
     Cont ((v :: stack),k,env)
   | _ -> runtime "No closure match")


let eval _ e env =
  eval_with_k [] [] e env
