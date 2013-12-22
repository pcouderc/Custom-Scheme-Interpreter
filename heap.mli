type value =
    Int of int
  | Id of Ast.id
  | Str of string
  | Ast of Ast.expr
  | Bool of bool
  | Closure of Ast.expr * env
  | Cont of value list * Ast.expr list * env
  | Cons of value * value
  | Nil
  | Undef
and binding = Ast.id * value ref
and env = binding list

val global_env : env ref

val is_cont : value -> bool

(* Looks up a value in the environment. None if not found. *)
val lookup : Ast.id -> env -> value option

(* Updates a binding in the environment to a new value,
 * as a side effect. *)
val update : Ast.id -> value -> env -> unit

(* Creates a new binding in the environment with the given value. *)
val bind : Ast.id -> value -> env -> env

val value_to_string : value -> string

val value_list_to_string : value list -> string

(** Functions to manipulate the global environment *)

val g_lookup : Ast.id -> value option

val g_update : Ast.id -> value -> unit

val g_bind : Ast.id -> value -> unit

val print_env : env -> unit
