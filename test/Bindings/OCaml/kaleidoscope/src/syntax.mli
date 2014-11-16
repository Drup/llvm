(** Parsing and lexing. Also see {!Ast}. *)

(** On what operate the lexer/parser. *)
type lexbuf

val create_lexbuf :
  ?file:string -> Sedlexing.lexbuf -> lexbuf

(** Parse a list of statement, ended by [eof]. *)
val parse_program :
  lexbuf -> Ast.statement list

(** Parse a statement or [eof] (and return [None] in this case). *)
val parse_statement :
  lexbuf -> Ast.statement option


(** {2 Error handling} *)

(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

val string_of_ParseError : (string * int * int * string) -> string
