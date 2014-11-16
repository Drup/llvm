(** Abstract Syntax Tree (aka Parse Tree) *)

type expr =

  (** [Number 1.0] *)
  | Number of float

  (** [Variable x] *)
  | Variable of string

  (** [Unary (symbol, arg)] *)
  | Unary of string * expr

  (** [Binary (symbol, arg1, arg2)] *)
  | Binary of string * expr * expr

  (** [Call (func, [| arg1 ; arg2 ; ... |]) ]
      [ <id> ( <expr> (, <expr>)* ) ]
  *)
  | Call of string * expr array

  (** [If (test, then_body, else_body)]
      [if <expr> then <expr> else <expr>]
  *)
  | If of expr * expr * expr

  (** [For (id, init, bound, step, body)]
      [for <id> = <expr> , <expr> (, <expr>)? = <expr>]
  *)
  | For of string * expr * expr * expr option * expr

  (** [Var ([|(id, value); .. |], body) ]
      [var <id> = <expr> (, <id> = <expr> )* in <expr>]
  *)
  | Var of (string * expr) array * expr

  (** [Assign (id, value)]
      [ <id> = <expr> ]
  *)
  | Assign of (string * expr)

[@@deriving show]


(** The prototype of a function is a name, and its arguments' names. *)
type prototype = {
  name : string ;
  arguments : string array ;
}
[@@deriving show]

(** Function definition. *)
type func = {
  proto : prototype ;
  body : expr ;
}
[@@deriving show]

(** Create an anonymous function, with a dummy prototype, given a body.
    Used for execution of toplevel expression.
*)
let anon_function body = {
  proto = { name = "" ; arguments = [||] } ;
  body
}

(** A toplevel statement. *)
type statement =
  | Extern of prototype
  | Def of func
  | Expr of expr
[@@deriving show]
