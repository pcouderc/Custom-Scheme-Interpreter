val eval_with_k :
  Heap.value list -> Ast.expr list -> Ast.expr -> Heap.env -> Heap.value

val apply_binop : Ast.op -> Heap.value -> Heap.value -> Heap.value
val apply_unop : Ast.op -> Heap.value -> Heap.value
