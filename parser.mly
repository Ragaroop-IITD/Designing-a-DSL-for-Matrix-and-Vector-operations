%{
  open Ast

  (* Helper function to decide if a list of expressions forms a matrix *)
  let try_build_matrix exprs =
    match exprs with
    | [] -> Some [] (* An empty list can be an empty matrix *)
    | (VectorConst(r) :: _) as rows ->
        let first_row_len = List.length r in
        if first_row_len = 0 then Some [] (* Matrix of empty rows? Or Vector of empty vectors? Let's say matrix. *)
        else if List.for_all (function
          | VectorConst(row) -> List.length row = first_row_len
          | _ -> false) rows
        then
          (* All elements are vectors of the same non-zero length *)
          Some (List.map (function VectorConst(row) -> row | _ -> failwith "Impossible") rows)
        else
          None (* Not all elements are vectors or lengths differ *)
    | _ -> None (* First element isn't a vector, cannot be a matrix *)

%}

/* Tokens */
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> IDENT FILENAME
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MOD DOT
%token EQ NEQ LT GT LE GE
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COMMA ASSIGN
%token INPUT PRINT IF THEN ELSE FOR TO WHILE
%token BOOL INT FLOAT VECTOR MATRIX
%token ANGLE MAGNITUDE DIMENSION TRANSPOSE DETERMINANT
%token INV
%token EOF

/* Precedence and associativity */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left DOT
%right NOT
%nonassoc UMINUS
      

/* Starting point */
%start program
%type <Ast.program> program

%%

program:
  | stmt EOF                                         { $1 }
  ;

stmt:
  | IDENT ASSIGN expr SEMICOLON                      { Assign($1, $3) }
  | block                                            { $1 }
  | IF expr THEN stmt ELSE stmt                      { If($2, $4, $6) }
  | FOR IDENT ASSIGN expr TO expr block              { For($2, $4, $6, $7) }
  | WHILE expr block                                 { While($2, $3) }
  | INPUT LPAREN RPAREN SEMICOLON                    { Input(None) }
  | INPUT LPAREN FILENAME RPAREN SEMICOLON           { Input(Some($3)) }
  | PRINT LPAREN expr RPAREN SEMICOLON               { Print($3) }
  ;

block:
  | LBRACE stmt_list RBRACE                          { Seq($2) }
  ;

stmt_list:
  | /* empty */                                      { [] }
  | stmt stmt_list                                   { $1 :: $2 }
  ;

expr:
  | TRUE                                             { BoolConst(true) }
  | FALSE                                            { BoolConst(false) }
  | INT_CONST                                        { IntConst($1) }
  | FLOAT_CONST                                      { FloatConst($1) }
  | IDENT                                            { Var($1) }
  
  | LBRACKET expr_list_opt RBRACKET                {
                                                       match try_build_matrix $2 with
                                                       | Some matrix_rows -> MatrixConst(matrix_rows)
                                                       | None -> VectorConst($2)
                                                     }
  | expr PLUS expr                                   { BinOp(Add, $1, $3) }
  | expr MINUS expr                                  { BinOp(Sub, $1, $3) }
  | expr TIMES expr                                  { BinOp(Mult, $1, $3) }
  | expr DIVIDE expr                                 { BinOp(Div, $1, $3) }
  | expr MOD expr                                    { BinOp(Mod, $1, $3) }
  | expr AND expr                                    { BinOp(And, $1, $3) }
  | expr OR expr                                     { BinOp(Or, $1, $3) }
  | expr EQ expr                                     { BinOp(Eq, $1, $3) }
  | expr NEQ expr                                    { BinOp(Neq, $1, $3) }
  | expr LT expr                                     { BinOp(Lt, $1, $3) }
  | expr GT expr                                     { BinOp(Gt, $1, $3) }
  | expr LE expr                                     { BinOp(Le, $1, $3) }
  | expr GE expr                                     { BinOp(Ge, $1, $3) }
  | expr DOT expr                                    { BinOp(DotProd, $1, $3) }
  | MINUS expr %prec UMINUS                          { UnOp(Neg, $2) }
  | NOT expr                                         { UnOp(Not, $2) }
  | ANGLE LPAREN expr COMMA expr RPAREN              { Angle($3, $5) }
  | MAGNITUDE LPAREN expr RPAREN                     { Magnitude($3) }
  | DIMENSION LPAREN expr RPAREN                     { Dimension($3) }
  | TRANSPOSE LPAREN expr RPAREN                     { Transpose($3) }
  | DETERMINANT LPAREN expr RPAREN                   { Determinant($3) }
  | LPAREN expr RPAREN                               { $2 }
  | INV LPAREN expr RPAREN                           { Inverse($3) }
  | expr LBRACKET expr RBRACKET                      { Index($1, $3) }
  ;

(* NEW: Optional comma-separated list of expressions *)
expr_list_opt:
  | /* empty */                                      { [] }
  | expr_list_nonempty                             { $1 }
  ;

(* NEW: Non-empty comma-separated list of expressions *)
expr_list_nonempty:
  | expr                                             { [$1] }
  | expr COMMA expr_list_nonempty                  { $1 :: $3 }
  ;


%%
