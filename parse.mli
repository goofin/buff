(*
 * Error type
 *)

type error

val error_to_string : error -> string

(*
 * Parsing functions
 *)

val lexbuf : Lexing.lexbuf -> (Ast.t, error) result
val stdin : unit -> (Ast.t, error) result
val string : string -> (Ast.t, error) result
