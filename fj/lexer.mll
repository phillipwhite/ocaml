{
let reservedWords = [
  (* Keywords *)
  ("class", Parser.CLASS);
  ("super", Parser.SUPER);
  ("return", Parser.RET);
  ("new", Parser.NEW);
  ("extends", Parser.EXTENDS);
  ("this", Parser.THIS)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

  (* | "-"? ['0'-'9']+ *)
  (*     { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) } *)

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "{" { Parser.LCURL }
| "}" { Parser.RCURL}
| ";" { Parser.SEMI }
| "." { Parser.DOT }
| "," { Parser.COLON }
| "=" { Parser.EQ} 
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }


