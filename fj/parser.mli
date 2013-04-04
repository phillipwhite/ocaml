type token =
  | LPAREN
  | RPAREN
  | LCURL
  | RCURL
  | SEMI
  | DOT
  | COLON
  | CLASS
  | SUPER
  | RET
  | NEW
  | EXTENDS
  | THIS
  | EQ
  | ID of (string)

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.program
