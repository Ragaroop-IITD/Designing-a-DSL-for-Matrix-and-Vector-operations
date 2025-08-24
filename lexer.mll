{
  open Parser  (* Use Parser's tokens, not Token module *)
  exception LexingError of string
}

let digit = ['0'-'9']
let int = digit+
let float = digit+ '.' digit+ ('e' ['+' '-']? digit+)?
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_' | '\'')*
let whitespace = [' ' '\t' '\r' '\n']+
let filename = (alpha | digit | '_' | '.' | '-' | '/'|' ')+ (* filename pattern *)

rule tokenize = parse
  | whitespace     { tokenize lexbuf }   (* Skip whitespace *)
  | "//"[^'\n']*   { tokenize lexbuf }   (* Single line comment *)
  | "/*"           { comment lexbuf }    (* Start of multi-line comment *)
  
  (* Keywords *)
  | "Input"        { INPUT }
  | "Print"        { PRINT }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "for"          { FOR }
  | "to"           { TO }
  | "while"        { WHILE }
  | "bool"         { BOOL }
  | "int"          { INT }
  | "float"        { FLOAT }
  | "vector"       { VECTOR }
  | "matrix"       { MATRIX }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "angle"        { ANGLE }
  | "magnitude"    { MAGNITUDE }
  | "dim"          { DIMENSION }
  | "trans"        { TRANSPOSE }
  | "det"          { DETERMINANT }
  
  (* Constants *)
  | int as i       { INT_CONST(int_of_string i) }
  | float as f     { FLOAT_CONST(float_of_string f) }
  
  (* Operators - note: put := before : to ensure proper matching *)
  | ":="           { ASSIGN }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIVIDE }
  | '%'            { MOD }
  | '.'            { DOT }
  
  (* Comparison operators *)
  | "=="           { EQ }
  | "!="           { NEQ }
  | '<'            { LT }
  | '>'            { GT }
  | "<="           { LE }
  | ">="           { GE }
  
  (* Boolean operators *)
  | "&&"           { AND }
  | "||"           { OR }
  | '!'            { NOT }
  
  (* Delimiters *)
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRACKET }
  | ']'            { RBRACKET }
  | ';'            { SEMICOLON }
  | ','            { COMMA }
  | "inv"          { INV }
  | '['            { LBRACKET }
  | ']'            { RBRACKET }
  
  (* Special filename handling for Input/Print *)
  | '"' (filename as fname) '"' { FILENAME(fname) }
  
  (* Identifiers - must come after keywords *)
  | ident as id    { IDENT(id) }
  
  (* End of file *)
  | eof            { EOF }
  
  (* Error handling *)
  | _ as c         { raise (LexingError ("Unexpected character: " ^ String.make 1 c)) }

(* Handle multi-line comments *)
and comment = parse
  | "*/"           { tokenize lexbuf }  (* End of comment *)
  | _              { comment lexbuf }   (* Skip comment content *)
  | eof            { raise (LexingError "Unclosed comment") }
