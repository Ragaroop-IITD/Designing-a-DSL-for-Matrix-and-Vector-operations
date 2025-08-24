(* main.ml - Main entry point for our DSL interpreter *)

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.program Lexer.tokenize lexbuf in
    close_in chan;
    ast
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      close_in chan;
      prerr_string "Parse error at line ";
      prerr_int line;
      prerr_string ", column ";
      prerr_int col;
      prerr_newline ();
      exit 1
  | Lexer.LexingError msg ->
      close_in chan;
      prerr_endline ("Lexing error: " ^ msg);
      exit 1
  | e ->
      close_in chan;
      prerr_endline ("Unexpected error: " ^ Printexc.to_string e);
      exit 1

let () =
  if Array.length Sys.argv < 2 then
    prerr_endline "Usage: ./main.byte <filename.dsl>"
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    Interpreter.interpret ast
