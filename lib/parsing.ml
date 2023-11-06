open Lexing


module I = Parser.MenhirInterpreter

exception Syntax_error of Location.lexeme_pos * string

let raise_syntax_error lexbuf msg =
  let pos = Location.to_lexeme_position lexbuf in
  raise (Syntax_error (pos, msg))

let fail lexbuf _ =
  raise_syntax_error lexbuf @@ "syntax error at lexeme \"" ^ Lexing.lexeme lexbuf ^ "\"."

let loop func lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier func lexbuf in
  I.loop_handle Fun.id (fail lexbuf) supplier result

let parse func lexbuf = loop func lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
