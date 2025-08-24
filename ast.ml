(* Types in our language *)
type typ =
  | TBool
  | TInt
  | TFloat
  | TVector of int
  | TMatrix of int * int

(* Binary operators *)
type binop =
  | Add | Sub | Mult | Div | Mod     (* arithmetic operators *)
  | And | Or                         (* boolean operators *)
  | Eq | Neq | Lt | Gt | Le | Ge     (* comparison operators *)
  | DotProd                          (* vector dot product *)

(* Unary operators *)
type unop =
  | Neg (* negation for numbers, vectors, matrices *)
  | Not (* boolean negation *)

(* Expressions in our language *)
type expr =
  | BoolConst of bool
  | IntConst of int
  | FloatConst of float
  | VectorConst of expr list           (* [a, b, c] *)
  | MatrixConst of expr list list      (* [[a, b], [c, d]] *)
  | Var of string                      (* variable reference *)
  | BinOp of binop * expr * expr       (* binary operation *)
  | UnOp of unop * expr                (* unary operation *)
  | Angle of expr * expr               (* angle between vectors *)
  | Magnitude of expr                  (* magnitude of a vector *)
  | Dimension of expr                  (* dimension of vector/matrix *)
  | Transpose of expr                  (* transpose of a matrix *)
  | Determinant of expr                (* determinant of a matrix *)
  | Inverse of expr
  | Index of expr * expr  

(* Statements in our language *)
type stmt =
  | Assign of string * expr            (* x := expression *)
  | Seq of stmt list                   (* { statement1; statement2; ... } *)
  | If of expr * stmt * stmt           (* if condition then stmt else stmt *)
  | For of string * expr * expr * stmt (* for i = start to end { ... } *)
  | While of expr * stmt               (* while condition { ... } *)
  | Input of string option             (* Input() or Input("filename") *)
  | Print of expr                      (* Print(expression) *)

(* A program is just a statement *)
type program = stmt
