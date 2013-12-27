(* Authors:
    Remy El Sibaïe - Besognet <besognet.remy@gmail.com>
    Pierrick Couderc <pierrick.couderc@gmail.com>
    students at Université Pierre et Marie Curie *)

(* Those two functions are from the original interpreter *)

(** Apply a binop operation to two values *)
val apply_binop : Ast.op -> Heap.value -> Heap.value -> Heap.value

(** Apply a unop to a value *)
val apply_unop : Ast.op -> Heap.value -> Heap.value


(** [eval_with_k stack cont expr env] evaluates an expression to a value, but is
    specialized for call-with-current-continuation.
    - [stack] is a stack of already evaluated values
    - [cont] is the remaining operations to evaluate after the expression given
    - [expr] is the expression to evaluate
    - [env] is the local env

    The common case is to call it like this : [eval_with_k [] [] expr []], you
    shouldn't need to give the other arguments.

    The behaviour is explained in README-UPMC.
*)
val eval_with_k :
  Heap.value list -> Ast.expr list -> Ast.expr -> Heap.env -> Heap.value
