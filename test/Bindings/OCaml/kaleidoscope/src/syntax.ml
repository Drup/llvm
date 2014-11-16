(** Syntax stuff *)


(** Some sedlexing stuff. *)

(** The state of the parser, a stream and a position. *)
type lexbuf = {
  stream : Sedlexing.lexbuf ;
  mutable pos : Lexing.position ;
}

(** Initialize with the null position. *)
let create_lexbuf ?(file="") stream =
  let pos = {Lexing.
    pos_fname = file;
    pos_lnum = 1; (* Start lines at 1, not 0 *)
    pos_bol = 0;
    pos_cnum = 0;
  }
  in { pos ; stream }

(** Register a new line in the lexer's position. *)
let new_line ?(n=0) lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <-
    {lcp with
       pos_lnum = lcp.pos_lnum + 1;
       pos_bol = lcp.pos_cnum;
    }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- {p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream } = Sedlexing.Utf8.lexeme stream


(** [ParseError (file, line, col, token)] *)
exception ParseError of (string * int * int * string)

let raise_ParseError lexbuf =
  let { pos } = lexbuf in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let tok = lexeme lexbuf in
  raise @@ ParseError (pos.pos_fname, line, col, tok)

let string_of_ParseError (file, line, cnum, tok) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, colunm %i, token %s"
    (file_to_string file)
    line cnum tok


(** The Lexing. *)

let ident_table =
  let open Parser in
  function
    | "def"    -> Def
    | "extern" -> Extern
    | "if"     -> If
    | "then"   -> Then
    | "else"   -> Else
    | "for"    -> For
    | "in"     -> In
    | "var"    -> Var
    | id       -> Ident id

let rec lex lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
    (* On new lines, update the position. *)
    | '\n' ->
        update lexbuf ; new_line lexbuf ;
        lex lexbuf

    (* Skip any whitespace. *)
    | white_space ->
        update lexbuf ;
        lex lexbuf

    (* Identifier: [A-Za-z][A-Za-z0-9]* *)
    | ('A' .. 'Z' | 'a' .. 'z'), Star ('A' .. 'Z' | 'a' .. 'z'|'0' .. '9') ->
        update lexbuf ;
        ident_table @@ lexeme lexbuf

    (* number: [0-9][0-9.]* *)
    | ('0' .. '9'), Star ('0' .. '9' | '.' ) ->
        update lexbuf ;
        Parser.Number (float_of_string @@ lexeme lexbuf)

    (* Comment until end of line. *)
    | '#', Star (Compl '\n'), '\n' ->
        update lexbuf ; new_line lexbuf ;
        lex lexbuf

    (* end of stream. *)
    | eof ->
        update lexbuf ;
        Parser.Eof

    (* The various other symbols. *)
    | "("  -> update lexbuf ; LParen
    | ")"  -> update lexbuf ; RParen
    | ","  -> update lexbuf ; Comma
    | ";"  -> update lexbuf ; Semi
    | ">"  -> update lexbuf ; Ge
    | "<"  -> update lexbuf ; Le
    | "==" -> update lexbuf ; EqualEqual
    | "="  -> update lexbuf ; Equal
    | "+"  -> update lexbuf ; Plus
    | "-"  -> update lexbuf ; Minus
    | "*"  -> update lexbuf ; Time
    | "/"  -> update lexbuf ; Div
    | "!"  -> update lexbuf ; Bang

    | _ ->
        update lexbuf ;
        raise_ParseError lexbuf


let parse f lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lex lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised f
  in
  try
    parser lexer
  with
    | Parser.Error
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> raise_ParseError lexbuf

let parse_program lexbuf =
  parse Parser.main lexbuf


let parse_statement lexbuf =
  parse Parser.main_statement lexbuf
