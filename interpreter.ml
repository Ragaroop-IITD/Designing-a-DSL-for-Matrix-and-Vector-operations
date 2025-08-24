(* interpreter.ml - Interpreter for our DSL *)
open Ast
(* Add at the top of interpreter.ml *)
open Str

(* Module for matrix operations, built on top of our Vector module *)
module Matrix = struct
  (* A matrix is a list of vectors (rows) *)
  type matrix = Vector.Vector.vector list

  (* Create a matrix filled with a constant value *)
  let create m n x =
    if m < 1 || n < 1 then raise Vector.DimensionError
    else List.init m (fun _ -> Vector.Vector.create n x)

  (* Get dimensions of matrix: (rows, cols) *)
  let dim m =
    match m with
    | [] -> (0, 0)
    | row :: _ -> (List.length m, Vector.Vector.dim row)

  (* Matrix addition *)
  let add m1 m2 =
    if List.length m1 <> List.length m2 then raise Vector.DimensionError
    else List.map2 Vector.Vector.addv m1 m2

  (* Scale a matrix *)
  let scale c m = List.map (Vector.Vector.scale c) m

  (* Matrix negation *)
  let neg m = scale (-1.0) m

  (* Matrix multiplication *)
  let mult m1 m2 =
    let (rows1, cols1) = dim m1 in
    let (rows2, cols2) = dim m2 in
    
    (* Verify dimensions *)
    if cols1 <> rows2 then
      raise Vector.DimensionError
    else
      (* For each position (i,j) in result matrix *)
      List.init rows1 (fun i ->
        (* Get row i from m1 *)
        let row_i = List.nth m1 i in
        
        (* Create each column vector from m2 *)
        List.init cols2 (fun j ->
          let col_j = List.map (fun row -> List.nth row j) m2 in
          (* Compute dot product of row_i and col_j *)
          Vector.Vector.dot_prod row_i col_j
        )
      )

  (* Transpose a matrix *)
  let transpose m =
    match m with
    | [] -> []
    | row :: _ ->
        let cols = Vector.Vector.dim row in
        let rows = List.length m in
        List.init cols (fun j ->
          List.init rows (fun i ->
            List.nth (List.nth m i) j
          )
        )

  (* Calculate determinant of a square matrix *)
  let rec det m =
    let (rows, cols) = dim m in
    if rows <> cols then
      raise (Failure "Determinant requires a square matrix")
    else if rows = 1 then
      List.hd (List.hd m)  (* 1x1 matrix *)
    else if rows = 2 then
      (* 2x2 matrix: ad - bc *)
      let a = List.hd (List.hd m) in
      let b = List.nth (List.hd m) 1 in
      let c = List.hd (List.nth m 1) in
      let d = List.nth (List.nth m 1) 1 in
      a *. d -. b *. c
    else
      (* For larger matrices, use cofactor expansion along first row *)
      let first_row = List.hd m in
      
      (* Create submatrix by removing row i and column j *)
      let submatrix i j mat =
        List.mapi (fun row_idx row ->
          if row_idx <> i then
            List.mapi (fun col_idx elem ->
              if col_idx <> j then Some elem else None
            ) row |> List.filter_map (fun x -> x)
          else []
        ) mat |> List.filter (fun row -> row <> [])
      in
      
      (* Sum of cofactors *)
      List.mapi (fun j elem ->
        let sign = if j mod 2 = 0 then 1.0 else -1.0 in
        let minor = submatrix 0 j m in
        sign *. elem *. det minor
      ) first_row |> List.fold_left (+.) 0.0
  (* In the Matrix module, replace the current inverse function *)
let rec inverse m =
  let d = det m in
  if d = 0.0 then raise (Failure "Singular matrix")
  else match m with
    | [[a; b]; [c; d]] -> 
      (* Special case for 2x2 matrices - more efficient *)
      let inv_det = 1.0 /. d in
      [[d *. inv_det; -.b *. inv_det];
       [-.c *. inv_det; a *. inv_det]]
    | _ -> 
      (* General case for nxn matrices *)
      let n = List.length m in
      
      (* Create cofactor matrix *)
      let cofactor_matrix =
        List.init n (fun i ->
          List.init n (fun j ->
            (* Calculate cofactor at position (i,j) *)
            let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
            
            (* Create minor matrix by removing row i and column j *)
            let minor = 
              List.mapi (fun r row ->
                if r <> i then
                  List.mapi (fun c elem ->
                    if c <> j then Some elem else None
                  ) row |> List.filter_map (fun x -> x)
                else []
              ) m |> List.filter (fun row -> row <> [])
            in
            
            sign *. (det minor)
          )
        )
      in
      
      (* Transpose the cofactor matrix to get adjugate *)
      let adjugate = transpose cofactor_matrix in
      
      (* Multiply each element by 1/determinant *)
      let inv_det = 1.0 /. d in
      List.map (List.map (fun x -> x *. inv_det)) adjugate

end

(* Value types for our interpreter *)
type value =
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VVector of float list
  | VMatrix of float list list

(* Environment type - maps variable names to values *)
type environment = (string * value) list

(* Exception for type errors *)
exception TypeError of string
exception RuntimeError of string

(* Helper function to convert any numeric value to float *)
let to_float = function
  | VInt i -> float_of_int i
  | VFloat f -> f
  | _ -> raise (TypeError "Expected a numeric value")

(* Pretty printing for values *)
let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VVector v -> 
      "[" ^ String.concat ", " (List.map string_of_float v) ^ "]"
  | VMatrix m -> 
      "[" ^ String.concat ", " (List.map 
        (fun row -> "[" ^ String.concat ", " (List.map string_of_float row) ^ "]") 
        m) ^ "]"

(* Print a value *)
let print_value v =
  print_endline (string_of_value v)

(* Convert expression to int value, with runtime type checking *)
let as_int = function
  | VInt i -> i
  | _ -> raise (TypeError "Expected an integer")

(* Convert expression to bool value, with runtime type checking *)
let as_bool = function
  | VBool b -> b
  | _ -> raise (TypeError "Expected a boolean")

(* Convert expression to vector value, with runtime type checking *)
let as_vector = function
  | VVector v -> v
  | _ -> raise (TypeError "Expected a vector")

(* Convert expression to matrix value, with runtime type checking *)
let as_matrix = function
  | VMatrix m -> m
  | _ -> raise (TypeError "Expected a matrix")

(* Evaluate a binary operation *)
let eval_binop op v1 v2 =
  match (op, v1, v2) with
  (* Arithmetic operations for integers *)
  | (Add, VInt a, VInt b) -> VInt (a + b)
  | (Sub, VInt a, VInt b) -> VInt (a - b)
  | (Mult, VInt a, VInt b) -> VInt (a * b)
  | (Div, VInt a, VInt b) -> 
      if b = 0 then raise (RuntimeError "Division by zero")
      else VInt (a / b)
  | (Mod, VInt a, VInt b) -> 
      if b = 0 then raise (RuntimeError "Modulo by zero")
      else VInt (a mod b)
  
  (* Arithmetic operations for floats *)
  | (Add, VFloat a, VFloat b) -> VFloat (a +. b)
  | (Sub, VFloat a, VFloat b) -> VFloat (a -. b)
  | (Mult, VFloat a, VFloat b) -> VFloat (a *. b)
  | (Div, VFloat a, VFloat b) -> 
      if b = 0.0 then raise (RuntimeError "Division by zero")
      else VFloat (a /. b)
  
  (* Mixed integer and float operations - convert to float *)
  | (Add, VInt a, VFloat b) -> VFloat ((float_of_int a) +. b)
  | (Add, VFloat a, VInt b) -> VFloat (a +. (float_of_int b))
  | (Sub, VInt a, VFloat b) -> VFloat ((float_of_int a) -. b)
  | (Sub, VFloat a, VInt b) -> VFloat (a -. (float_of_int b))
  | (Mult, VInt a, VFloat b) -> VFloat ((float_of_int a) *. b)
  | (Mult, VFloat a, VInt b) -> VFloat (a *. (float_of_int b))
  | (Div, VInt a, VFloat b) -> 
      if b = 0.0 then raise (RuntimeError "Division by zero")
      else VFloat ((float_of_int a) /. b)
  | (Div, VFloat a, VInt b) -> 
      if b = 0 then raise (RuntimeError "Division by zero")
      else VFloat (a /. (float_of_int b))
  
  (* Boolean operations *)
  | (And, VBool a, VBool b) -> VBool (a && b)
  | (Or, VBool a, VBool b) -> VBool (a || b)
  
  (* Comparison operations for integers *)
  | (Eq, VInt a, VInt b) -> VBool (a = b)
  | (Neq, VInt a, VInt b) -> VBool (a <> b)
  | (Lt, VInt a, VInt b) -> VBool (a < b)
  | (Gt, VInt a, VInt b) -> VBool (a > b)
  | (Le, VInt a, VInt b) -> VBool (a <= b)
  | (Ge, VInt a, VInt b) -> VBool (a >= b)
  
  (* Comparison operations for floats *)
  | (Eq, VFloat a, VFloat b) -> VBool (a = b)
  | (Neq, VFloat a, VFloat b) -> VBool (a <> b)
  | (Lt, VFloat a, VFloat b) -> VBool (a < b)
  | (Gt, VFloat a, VFloat b) -> VBool (a > b)
  | (Le, VFloat a, VFloat b) -> VBool (a <= b)
  | (Ge, VFloat a, VFloat b) -> VBool (a >= b)
  
  (* Mixed comparisons - convert to float *)
  | (Eq, VInt a, VFloat b) -> VBool ((float_of_int a) = b)
  | (Eq, VFloat a, VInt b) -> VBool (a = (float_of_int b))
  | (Neq, VInt a, VFloat b) -> VBool ((float_of_int a) <> b)
  | (Neq, VFloat a, VInt b) -> VBool (a <> (float_of_int b))
  | (Lt, VInt a, VFloat b) -> VBool ((float_of_int a) < b)
  | (Lt, VFloat a, VInt b) -> VBool (a < (float_of_int b))
  | (Gt, VInt a, VFloat b) -> VBool ((float_of_int a) > b)
  | (Gt, VFloat a, VInt b) -> VBool (a > (float_of_int b))
  | (Le, VInt a, VFloat b) -> VBool ((float_of_int a) <= b)
  | (Le, VFloat a, VInt b) -> VBool (a <= (float_of_int b))
  | (Ge, VInt a, VFloat b) -> VBool ((float_of_int a) >= b)
  | (Ge, VFloat a, VInt b) -> VBool (a >= (float_of_int b))
  
  (* Vector operations *)
  | (Add, VVector v1, VVector v2) -> 
      begin
        try VVector (Vector.Vector.addv v1 v2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Vector addition requires same dimensions")
      end
  | (Sub, VVector v1, VVector v2) -> 
      begin
        try
          let negv2 = Vector.Vector.inv v2 in
          VVector (Vector.Vector.addv v1 negv2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Vector subtraction requires same dimensions")
      end
  | (Mult, VFloat s, VVector v) -> VVector (Vector.Vector.scale s v)
  | (Mult, VInt s, VVector v) -> VVector (Vector.Vector.scale (float_of_int s) v)
  | (Mult, VVector v, VFloat s) -> VVector (Vector.Vector.scale s v)
  | (Mult, VVector v, VInt s) -> VVector (Vector.Vector.scale (float_of_int s) v)
  | (DotProd, VVector v1, VVector v2) -> 
      begin
        try VFloat (Vector.Vector.dot_prod v1 v2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Vector dot product requires same dimensions")
      end
  
  (* Matrix operations *)
  | (Add, VMatrix m1, VMatrix m2) -> 
      begin
        try VMatrix (Matrix.add m1 m2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Matrix addition requires same dimensions")
      end
  | (Sub, VMatrix m1, VMatrix m2) -> 
      begin
        try
          let negm2 = Matrix.neg m2 in
          VMatrix (Matrix.add m1 negm2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Matrix subtraction requires same dimensions")
      end
  | (Mult, VFloat s, VMatrix m) -> VMatrix (Matrix.scale s m)
  | (Mult, VInt s, VMatrix m) -> VMatrix (Matrix.scale (float_of_int s) m)
  | (Mult, VMatrix m, VFloat s) -> VMatrix (Matrix.scale s m)
  | (Mult, VMatrix m, VInt s) -> VMatrix (Matrix.scale (float_of_int s) m)
  | (Mult, VMatrix m1, VMatrix m2) -> 
      begin
        try VMatrix (Matrix.mult m1 m2)
        with Vector.DimensionError -> 
          raise (RuntimeError "Matrix multiplication dimension mismatch")
      end  
  | (Mult, VMatrix m, VVector v) -> 
      let cols = match m with [] -> 0 | row::_ -> Vector.Vector.dim row in
      if cols <> List.length v then raise (RuntimeError "Dimension mismatch") else
      VVector (List.map (fun row -> Vector.Vector.dot_prod row v) m)
    
  | _ -> raise (TypeError ("Type error in binary operation: " ^ 
                          (string_of_value v1) ^ " and " ^ 
                          (string_of_value v2)))

(* Evaluate a unary operation *)
let eval_unop op v =
  match (op, v) with
  | (Neg, VInt i) -> VInt (-i)
  | (Neg, VFloat f) -> VFloat (-.f)
  | (Neg, VVector vec) -> VVector (Vector.Vector.inv vec)
  | (Neg, VMatrix mat) -> VMatrix (Matrix.neg mat)
  | (Not, VBool b) -> VBool (not b)
  | _ -> raise (TypeError ("Type error in unary operation: " ^ (string_of_value v)))

(* Create a vector of floats from a list of values *)
let vector_of_values values =
  List.map to_float values

(* Create a matrix of floats from a list of list of values *)
let matrix_of_values values_list =
  List.map vector_of_values values_list

(* Recursive evaluation of expressions *)
let rec eval_expr env = function
  | BoolConst b -> VBool b
  | IntConst i -> VInt i
  | FloatConst f -> VFloat f
  | VectorConst elems -> 
      let values = List.map (eval_expr env) elems in
      VVector (vector_of_values values)
  | MatrixConst rows -> 
      let value_rows = List.map (fun row -> List.map (eval_expr env) row) rows in
      
      (* Check that all rows have the same length *)
      let row_lengths = List.map List.length value_rows in
      if List.length (List.sort_uniq compare row_lengths) > 1 then
        raise (RuntimeError "All rows in a matrix must have the same length")
      else
        VMatrix (matrix_of_values value_rows)
  | Var id -> 
      begin
        try List.assoc id env
        with Not_found -> raise (RuntimeError ("Undefined variable: " ^ id))
      end
  | BinOp (op, e1, e2) -> 
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      eval_binop op v1 v2
  | UnOp (op, e) -> 
      let v = eval_expr env e in
      eval_unop op v
  | Angle (e1, e2) -> 
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      begin
        match (v1, v2) with
        | (VVector vec1, VVector vec2) -> 
            begin
              try VFloat (Vector.Vector.angle vec1 vec2)
              with Vector.DimensionError -> 
                raise (RuntimeError "Angle calculation requires vectors of same dimension")
            end
        | _ -> raise (TypeError "Angle requires two vectors")
      end
  | Magnitude e -> 
      let v = eval_expr env e in
      begin
        match v with
        | VVector vec -> VFloat (Vector.Vector.length vec)
        | _ -> raise (TypeError "Magnitude requires a vector")
      end
  | Dimension e -> 
      let v = eval_expr env e in
      begin
        match v with
        | VVector vec -> VInt (Vector.Vector.dim vec)
        | VMatrix mat -> 
            let (rows, cols) = Matrix.dim mat in
            VVector [float_of_int rows; float_of_int cols]
        | _ -> raise (TypeError "Dimension requires a vector or matrix")
      end
  | Transpose e -> 
      let v = eval_expr env e in
      begin
        match v with
        | VMatrix mat -> VMatrix (Matrix.transpose mat)
        | _ -> raise (TypeError "Transpose requires a matrix")
      end
  | Determinant e -> 
      let v = eval_expr env e in
      begin
        match v with
        | VMatrix mat -> 
            begin
              try VFloat (Matrix.det mat)
              with 
              | Failure msg -> raise (RuntimeError msg)
              | Vector.DimensionError -> 
                  raise (RuntimeError "Determinant requires a square matrix")
            end
        | _ -> raise (TypeError "Determinant requires a matrix")
      end
  | Inverse e ->
      let mat = as_matrix (eval_expr env e) in
      VMatrix (try Matrix.inverse mat with Failure msg -> raise (RuntimeError msg))
  
  | Index(e1, e2) ->
    let vec = eval_expr env e1 in
    let idx = eval_expr env e2 in
    begin
      match (vec, idx) with
      | (VVector v, VInt i) ->
          if i < 0 || i >= List.length v then
            raise (RuntimeError ("Index out of bounds: " ^ string_of_int i))
          else
            VFloat (List.nth v i)
      | (VMatrix m, VInt i) ->
          if i < 0 || i >= List.length m then
            raise (RuntimeError ("Row index out of bounds: " ^ string_of_int i))
          else
            VVector (List.nth m i)
      | (VMatrix m, VVector [row; col]) ->
          let r = int_of_float row in
          let c = int_of_float col in
          if r < 0 || r >= List.length m then
            raise (RuntimeError ("Row index out of bounds: " ^ string_of_int r))
          else if c < 0 || c >= List.length (List.nth m r) then
            raise (RuntimeError ("Column index out of bounds: " ^ string_of_int c))
          else
            VFloat (List.nth (List.nth m r) c)
      | _ -> raise (TypeError "Indexing requires a vector/matrix and an integer index")
    end
      

(* Recursive evaluation of statements *)
let rec eval_stmt env = function
  | Assign (id, expr) -> 
      let value = eval_expr env expr in
      (id, value) :: env  (* Update environment with new binding *)
  | Seq stmts -> 
      List.fold_left eval_stmt env stmts
  | If (cond, then_stmt, else_stmt) -> 
      let cond_val = eval_expr env cond in
      if as_bool cond_val then
        eval_stmt env then_stmt
      else
        eval_stmt env else_stmt
  | For (id, start_expr, end_expr, body) -> 
      let start_val = eval_expr env start_expr in
      let end_val = eval_expr env end_expr in
      let start_int = as_int start_val in
      let end_int = as_int end_val in
      
      (* Initialize loop with iterator variable *)
      let rec loop i env' =
        let loop_env = (id, VInt i) :: env' in
        if i > end_int then
          env'  (* Return environment after loop *)
        else
          let new_env = eval_stmt loop_env body in
          (* Remove the iterator variable binding for next iteration *)
          let filtered_env = List.filter (fun (var_id, _) -> var_id <> id) new_env in
          loop (i + 1) filtered_env
      in
      loop start_int env
  | While (cond, body) -> 
      let rec loop env' =
        let cond_val = eval_expr env' cond in
        if as_bool cond_val then
          let new_env = eval_stmt env' body in
          loop new_env
        else
          env'  (* Return environment when condition becomes false *)
      in
      loop env
  | Input opt_filename -> 
    begin
      match opt_filename with
      | None -> 
          (* Interactive input - not implemented for simplicity *)
          print_endline "Interactive input not supported in this implementation.";
          env
      | Some filename -> 
          try
            let chan = open_in filename in
            let first_line = input_line chan in
            
            (* Parse the first line to determine if it's a vector or matrix *)
            if String.contains first_line ',' then
              (* Matrix format: rows,cols followed by data *)
              let dimensions = Str.split (Str.regexp ",") first_line in
              let rows = int_of_string (List.nth dimensions 0) in
              let cols = int_of_string (List.nth dimensions 1) in
              
              (* Read the matrix data from the second line *)
              let matrix_line = input_line chan in
              let matrix_str = String.sub matrix_line 2 (String.length matrix_line - 4) in (* Remove [[ and ]] *)
              let row_strs = Str.split (Str.regexp "\\],\\s*\\[") matrix_str in
              
              if List.length row_strs <> rows then
                raise (RuntimeError ("Matrix rows mismatch: expected " ^ string_of_int rows))
              else
                let matrix = List.map (fun row_str ->
                  let elem_strs = Str.split (Str.regexp ", *") row_str in
                  if List.length elem_strs <> cols then
                    raise (RuntimeError ("Matrix columns mismatch: expected " ^ string_of_int cols))
                  else
                    List.map float_of_string elem_strs
                ) row_strs in
                
                close_in chan;
                (* Return the matrix in the environment with file basename as variable name *)
                let var_name = Filename.remove_extension (Filename.basename filename) in
                (var_name, VMatrix matrix) :: env
            else
              (* Vector format: dimension followed by data *)
              let dim = int_of_string first_line in
              
              (* Read the vector data from the second line *)
              let vector_line = input_line chan in
              let vector_str = String.sub vector_line 1 (String.length vector_line - 2) in (* Remove [ and ] *)
              let elem_strs = Str.split (Str.regexp ", *") vector_str in
              
              if List.length elem_strs <> dim then
                raise (RuntimeError ("Vector dimension mismatch: expected " ^ string_of_int dim))
              else
                let vector = List.map float_of_string elem_strs in
                
                close_in chan;
                (* Return the vector in the environment with file basename as variable name *)
                let var_name = Filename.remove_extension (Filename.basename filename) in
                (var_name, VVector vector) :: env
          with
          | Sys_error msg -> 
              raise (RuntimeError ("File error: " ^ msg))
          | End_of_file -> 
              raise (RuntimeError ("Unexpected end of file: " ^ filename))
          | Failure msg -> 
              raise (RuntimeError ("Parse error in file " ^ filename ^ ": " ^ msg))
    end
  | Print expr -> 
      let value = eval_expr env expr in
      print_value value;
      env  (* Return unchanged environment *)

(* Main interpreter function *)
let interpret program =
  try
    let _ = eval_stmt [] program in
    print_endline "Program executed successfully."
  with
  | TypeError msg -> prerr_endline ("Type Error: " ^ msg)
  | RuntimeError msg -> prerr_endline ("Runtime Error: " ^ msg)
  | Vector.DimensionError -> prerr_endline "Dimension Error: Vector or matrix dimensions don't match"
  | e -> prerr_endline ("Unknown error: " ^ Printexc.to_string e)
