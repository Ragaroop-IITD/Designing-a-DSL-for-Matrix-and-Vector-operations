type token =
  (* Keywords *)
  | INPUT
  | PRINT
  | IF
  | THEN
  | ELSE
  | FOR
  | TO        (* Added for for-loops *)
  | WHILE
  | BOOL   
  | INT    
  | FLOAT  
  | VECTOR 
  | MATRIX 
  
  (* Constants *)
  | TRUE
  | FALSE
  | INT_CONST of int
  | FLOAT_CONST of float
  
  (* Operations *)
  | PLUS       
  | MINUS      
  | TIMES      
  | DIVIDE     
  | MOD        
  | DOT        
  | ANGLE      
  | MAGNITUDE  
  | DIMENSION  
  | TRANSPOSE  
  | DETERMINANT
  
  (* Comparison Operators *)
  | EQ         
  | NEQ        
  | LT         
  | GT         
  | LE         
  | GE         
  
  (* Boolean Operators *)
  | AND        
  | OR         
  | NOT        
  
  (* Delimiters *)
  | LPAREN     (* ( *)
  | RPAREN     (* ) *)
  | LBRACE     (* { *)
  | RBRACE     (* } *)
  | LBRACKET   (* [ *)
  | RBRACKET   (* ] *)
  | SEMICOLON  (* ; *)
  | COMMA      (* , *)
  | ASSIGN     (* := *)
  
  (* Identifiers and literals *)
  | IDENT of string
  | FILENAME of string
  | INV 
  (* End of file *)
  | EOF 
