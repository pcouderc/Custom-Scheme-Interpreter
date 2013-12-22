{
open Parser
exception LexError

let keywords =
    [
     "lambda", LAMBDA;
     "let", LET;
     "if", IF;
     "define", DEFINE;
     "begin", BEGIN;
     "list", LIST;
     "nil", NIL;
     "cons", CONS;
     "letrec", LETREC;
     "definerec", DEFINEREC;
     "quote", QUOTE;
     "set!", SET;
     (* "eval", EVAL; *)
    ]


let keyword_tbl = Hashtbl.create 256
let uncurry f (a, b) = f a b
let _ = List.iter (uncurry (Hashtbl.replace keyword_tbl)) keywords

}

let op = (['+' '*' '/' '%' '~' '&' '|' '=' '<' '>' '-' '!']
             | "<=" | "!=" | ">=")
let id =  (op | (['a'-'z' 'A'-'Z']))
  (op | [ '_' 'A'-'Z' 'a'-'z' '0'-'9'])*
let nl = ['\n' '\r']
let ws = ['\n' '\t' '\r' ' ']
let int = ['0'-'9']+

rule token = parse
    id as id   { try Hashtbl.find keyword_tbl (String.lowercase id) with Not_found -> ID id }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "'"            { QUOTE }
  | "\""           { STRING (String.concat "" (string lexbuf)) }
  | "#t"           { TRUE }
  | "#f"           { FALSE }
  | "."            { DOT }
  | int as int     { INT (int_of_string int) }
  | ws             { token lexbuf }
  | ";"            { comment lexbuf }
  | eof            { EOF }
  | _              { raise LexError }
and string = parse
    "\\\\"           { "\\" :: (string lexbuf)}
  | "\\\""           { "\"" :: (string lexbuf) }
  | "\\n"            { "\n" :: (string lexbuf) }
  | "\\t"            { "\t" :: (string lexbuf) }
  | "\""             { [] }
  | _ as c           { (String.make 1 c) :: (string lexbuf) }
(* comments *)
and comment = parse
    nl               { token lexbuf }
  | eof              { EOF }
  | _                { comment lexbuf }
