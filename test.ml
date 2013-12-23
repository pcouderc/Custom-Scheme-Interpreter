
open Format


(* type value = *)
(*     Int of int *)
(*   | Id of Ast.id *)
(*   | Str of string *)
(*   | Ast of Ast.expr *)
(*   | Bool of bool *)
(*   | Closure of Ast.expr * env *)
(*   | Cont of value list * Ast.expr list * env *)
(*   | Cons of value * value *)
(*   | Nil *)
(*   | Undef *)

(* type expr = *)
(*   | Int_e of int *)
(*   | Str_e of string *)
(*   | Bool_e of bool *)
(*   | Id_e of id *)
(*   | Nil_e *)
(*   | Begin_e of expr list *)
(*   | Cons_e of expr * expr *)
(*   | Let_e of id * expr * expr *)
(*   | Letrec_e of id * expr * expr *)
(*   | If_e of expr * expr * expr *)
(*   | Apply_e of expr * expr list *)
(*   | Fun_e of id list * expr *)
(*   | Def_e of id list * expr *)
(*   | Defrec_e of id list * expr *)
(*   | Binop_e of op * expr * expr *)
(*   | Unop_e of op * expr *)
(*   | Delayed_e of expr *)
(*   | Forced_e of expr *)
(*   | Callcc_e of expr *)
(*   | Set_e of expr * expr *)

(*   | K (\* takes an already evaluated argument that waits this expr as its *)
(*          continuation *\) *)
(*   | Cont_e of id (\* id of the reified continuation binded in the environnement *\) *)
(*   | Eval_e of expr *)
(*   | Quote_e of expr *)


open Heap
open Ast

let _ =
  Format.printf "Testing...@."

let eval = Eval.eval
let ev e = Eval.eval e []

let test =
  let n = ref 0 in
  fun b -> incr n;
  printf "Test %d\t: " !n;
  if b then printf "OK@." else printf "KO@."

    (* 1 => 1 *)
let _ = test (ev (Int_e 1) = Int 1)

  (* '1 => 1 *)
let _ = test (ev (Quote_e (Int_e 1)) = Int 1)

  (* 'a => a *)
let _ = test (ev (Quote_e (Id_e "a")) = Ast (Id_e "a"))

  (* '"a" =>  "a" *)
let _ = test (ev (Quote_e (Str_e "a")) = Str "a")

  (* '(lambda (x) x) =>  '(lambda (x) x) *)
let _ = test (ev (Quote_e (Fun_e (["x"], (Id_e "x")))) =
    Ast (Fun_e (["x"], (Id_e "x"))))

let appid1 = (Apply_e ((Fun_e (["x"], (Id_e "x"))), [Int_e 1]))

  (* '((lambda (x) x) 1) => '((lambda (x) x) 1) *)
let _ = test (ev (Quote_e appid1) = Ast appid1)

  (* (eval '((lambda (x) x) 1)) => 1 *)
let _ = test (ev (Eval_e (Quote_e appid1)) = Int 1)

  (* (eval 'a => 1 when binding (a, 1) exists in context *)
let _ = test (eval (Eval_e (Quote_e (Id_e "x"))) ["x", ref (Int 1)] = Int 1)

  (* callcc (lambda (x) (x 1)) => 1 *)
let _ = test (ev (Callcc_e (Fun_e (["x"], (Apply_e (Id_e "x", [Int_e 1]))))) = Int 1)

let _ = test (ev (Callcc_e (Fun_e (["x"], Id_e "x"))) = Cont ([],[],[]))
