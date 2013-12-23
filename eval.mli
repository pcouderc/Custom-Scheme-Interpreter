val eval : Ast.expr -> Heap.env -> Heap.value
val apply : Ast.expr -> Heap.value -> Heap.value -> Heap.value
val apply_binop : Ast.op -> Heap.value -> Heap.value -> Heap.value
val apply_unop : Ast.op -> Heap.value -> Heap.value
