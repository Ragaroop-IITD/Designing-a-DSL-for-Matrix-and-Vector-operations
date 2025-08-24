# Designing-a-DSL-for-Matrix-and-Vector-operations

A complete implementation of a domain-specific language (DSL) designed for matrix and vector operations, built from scratch using OCaml, OCaml-Lex, and OCaml-Yacc.

## 🚀 Project Overview

This project implements a full-featured programming language specifically designed for mathematical computations involving vectors and matrices. The implementation includes a complete compilation pipeline from source code to execution, demonstrating expertise in compiler design, language theory, and functional programming.

## ✨ Key Features

### Language Capabilities
- **Vector Operations**: Addition, scalar multiplication, dot product, magnitude, angle calculation
- **Matrix Operations**: Addition, multiplication, transpose, determinant, scalar operations
- **Control Flow**: Conditional statements (if-then-else), for loops, while loops
- **Type System**: Static type checking with support for booleans, integers, floats, vectors, and matrices
- **I/O Operations**: File input/output and interactive user input
- **Error Handling**: Comprehensive error reporting for lexical, syntax, type, and runtime errors

### Technical Implementation
- **Lexical Analysis**: Custom tokenizer using OCaml-Lex with support for comments, scientific notation, and complex identifiers
- **Syntax Analysis**: Context-free grammar parser using OCaml-Yacc with conflict resolution
- **Type Checking**: Static type system ensuring mathematical operation correctness
- **Interpreter**: Complete execution engine with environment management and dynamic evaluation

## 🏗️ Architecture

```
Source Code (.dsl) → Lexer → Parser → Type Checker → Interpreter → Results
```

### Core Components

1. **Lexical Analyzer** (`lexer.mll`)
   - Tokenizes source code using regular expressions
   - Handles keywords, operators, literals, identifiers, and comments
   - Supports scientific notation for floating-point numbers

2. **Parser** (`parser.mly`) 
   - Builds Abstract Syntax Trees (AST) from token streams
   - Implements operator precedence and associativity
   - Resolves grammar conflicts for unambiguous parsing

3. **Type Checker** (`type_checker.ml`)
   - Ensures type safety for all operations
   - Validates vector/matrix dimension compatibility
   - Provides detailed error messages for type mismatches

4. **Interpreter** (`interpreter.ml`)
   - Evaluates ASTs with environment management
   - Implements all mathematical operations using functional programming
   - Handles control flow and variable scoping

5. **Vector Operations** (`vector.ml`)
   - Core mathematical library for vector computations
   - Efficient implementations of linear algebra operations
   - Custom exception handling for dimension errors

## 💻 Language Syntax Examples

### Basic Operations
```ocaml
// Vector operations
v1 := [1.0, 2.0, 3.0];
v2 := [4.0, 5.0, 6.0];
sum := v1 + v2;                    // Vector addition
dot_product := v1 . v2;            // Dot product
mag := magnitude(v1);              // Vector magnitude

// Matrix operations  
A := [[1.0, 2.0], [3.0, 4.0]];
B := [[5.0, 6.0], [7.0, 8.0]];
C := A * B;                        // Matrix multiplication
At := trans(A);                    // Matrix transpose
det_A := det(A);                   // Determinant
```

### Control Flow
```ocaml
// Conditional execution
if det(A) != 0.0 then {
    inverse := (1.0 / det(A)) * adj(A);
    Print("Matrix is invertible");
} else {
    Print("Matrix is singular");
}

// Loops
for i := 0 to 9 {
    sum := sum + i;
}

while norm > threshold {
    A := A * 0.9;
    norm := magnitude(A);
}
```

### File I/O
```ocaml
Input("matrix_data.txt");          // Read from file
Print(result);                     // Display output
Print("output.txt");               // Write to file
```

## 🛠️ Technical Skills Demonstrated

- **Compiler Design**: Complete implementation of lexical analysis, parsing, and interpretation
- **Functional Programming**: Extensive use of OCaml features including pattern matching, recursion, and immutable data structures
- **Language Theory**: Grammar design, conflict resolution, and formal language specification
- **Type Systems**: Implementation of static type checking with custom error reporting
- **Mathematical Computing**: Efficient algorithms for linear algebra operations
- **Software Architecture**: Modular design with clear separation of concerns

## 🚦 Installation and Usage

### Prerequisites
- OCaml compiler (4.08 or higher)
- OCaml-Lex and OCaml-Yacc tools

### Building the Project
```bash
# Generate lexer and parser
ocamllex lexer.mll
ocamlyacc parser.mly

# Compile all components
ocamlc -o matrix_dsl token.mli ast.ml vector.ml parser.mli \
       parser.ml lexer.ml interpreter.ml main.ml

# Run the interpreter
./matrix_dsl program.dsl
```

### Running Tests
```bash
# Execute comprehensive test suite
./matrix_dsl test_programs/matrix_operations.dsl
./matrix_dsl test_programs/control_flow.dsl
./matrix_dsl test_programs/vector_math.dsl
```

## 📋 Test Coverage

The implementation includes comprehensive testing for:

- **Lexical Analysis**: All token types, comments, error handling
- **Parsing**: Grammar coverage, precedence rules, error recovery
- **Type Checking**: Type compatibility, dimension validation, error messages
- **Mathematical Operations**: Vector/matrix arithmetic, advanced operations
- **Control Flow**: Conditionals, loops, nested structures
- **Error Handling**: Lexical errors, syntax errors, type errors, runtime exceptions

## 🎯 Sample Programs

### Matrix Inversion
```ocaml
{
    A := [[2.0, 1.0], [1.0, 1.0]];
    if det(A) != 0.0 then {
        // Calculate cofactor matrix
        cofactor := [[det([[1.0]]), -det([[1.0]])], 
                    [-det([[1.0]]), det([[2.0]])]];
        adjoint := trans(cofactor);
        inverse := (1.0 / det(A)) * adjoint;
        Print(inverse);
    } else {
        Print("Matrix not invertible");
    }
}
```

### Gaussian Elimination Setup
```ocaml
{
    A := [[2.0, 1.0, -1.0], [1.0, 3.0, 2.0], [1.0, 0.0, 0.0]];
    b := [8.0, 13.0, 3.0];
    
    // Forward elimination process
    for i := 0 to 2 {
        for j := i+1 to 2 {
            factor := A[j][i] / A[i][i];
            // Update row operations...
        }
    }
}
```

## 🔧 Advanced Features

- **Scientific Notation**: Support for `1.23e-4` format in floating-point literals
- **Flexible Identifiers**: Variables can include underscores and prime symbols (`matrix'`, `vector_sum`)
- **Comprehensive Comments**: Both single-line (`//`) and multi-line (`/* */`) comment support
- **Robust Error Messages**: Detailed error reporting with line numbers and context
- **Dimension Validation**: Automatic checking of vector/matrix dimension compatibility

## 📈 Performance Considerations

- **Efficient Data Structures**: Vectors represented as OCaml lists for functional programming benefits
- **Lazy Evaluation**: Strategic use of lazy evaluation for complex mathematical operations
- **Memory Management**: Leverages OCaml's garbage collection for automatic memory management
- **Tail Recursion**: Optimized recursive implementations for large data processing

## 🎓 Learning Outcomes

This project demonstrates proficiency in:

- **Programming Language Design**: From syntax specification to implementation
- **Compiler Construction**: Complete understanding of compilation pipeline
- **Functional Programming**: Advanced OCaml programming techniques
- **Mathematical Computing**: Implementation of linear algebra algorithms
- **Software Engineering**: Modular design, testing, and documentation practices
- **Problem Solving**: Complex algorithm design and optimization

## 📝 Documentation

- **Language Specification**: Complete formal grammar and semantic rules
- **API Documentation**: Detailed function signatures and usage examples
- **Test Documentation**: Comprehensive test cases with expected outputs
- **Design Decisions**: Rationale for implementation choices and trade-offs

## 🔮 Future Enhancements

Potential extensions for the language include:

- **Advanced Linear Algebra**: Eigenvalue/eigenvector computation, SVD decomposition
- **Optimization Features**: JIT compilation, vectorization
- **Extended Type System**: Generic types, polymorphic functions
- **Concurrency Support**: Parallel matrix operations
- **Standard Library**: Built-in mathematical functions and constants


**Note :**
*This project showcases a complete programming language implementation, demonstrating deep understanding of compiler design principles, functional programming paradigms, and mathematical computing concepts.*
