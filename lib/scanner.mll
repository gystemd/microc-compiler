{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let raise_lexer_error buf err=
        let position = Location.to_lexeme_position buf in
        raise (Lexing_error(position, err))

    let create_hashtable size init =
        let table = Hashtbl.create size in
        List.iter(fun (key, value) -> Hashtbl.add table key value ) init;
        table


    let keywords = create_hashtable 15 [
        ("if", IF);
        ("return",RETURN);
        ("else", ELSE);
        ("for",FOR);
        ("while", WHILE);
        ("int",INT);
        ("char", CHAR);
        ("void",VOID);
        ("NULL", NULL);
        ("bool",BOOL);
        ("true",TRUE);
        ("false",FALSE);
        ("do",DO);
        ("float",FLOAT);
        ("struct",STRUCT)
    ]
}

(* Scanner specification *)

let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let exponent   = ['e' 'E'] ['-' '+']? digit+
let float      = (digit digit* '.' digit* | digit* '.' digit digit*) exponent? | digit+ exponent
let id = ('_' | letter )('_' | letter | digit)*
let newline = '\r'|'\n'| '\n' '\r'
let ws = [' ' '\t']+
rule next_token = parse
    | ws+       {next_token lexbuf}
    | newline {Lexing.new_line lexbuf; next_token lexbuf}
    | id as word
        {
          match Hashtbl.find_opt keywords word with
          | Some kw -> kw
          | None -> ID(word)
        }
    | digit+ as integer { INTEGER(int_of_string integer)}
    | float as fl {FLOATLIT(float_of_string fl)}
    | "true" { TRUE}
    | "false"{ FALSE}
    |  "'" { get_char lexbuf}
    | "//" { singlelinecomment lexbuf}
    | "/*" {  multilinecomment lexbuf}
    | '(' { LPAREN}
    | ')' { RPAREN}
    | '{' { LBRACE}
    | '}' { RBRACE}
    | '[' { LBRACKET}
    | ']' { RBRACKET}
    | '"' {get_string (Buffer.create 15) lexbuf}
    | ';' { SEMI}
    | ',' { COMMA}
    | '+' { PLUS}
    | '-' { MINUS}
    | '*' { TIMES}
    | '/' { DIVIDE}
    | '%' { MOD}
    | '=' { ASSIGN}
    | '.' {DOT}
    | "==" {EQ}
    | "!=" { NEQ}
    | '<' { LT}
    | '>' { GT}
    | "<=" { LEQ}
    | ">=" { GEQ}
    | '!' { NOT}
    | '&' { ADDRESS}
    | "&&" { AND}
    | "++" {INCREMENT}
    | "--" {DECREMENT}
    | "+=" {SHORTADD}
    | "-=" {SHORTMIN}
    | "*=" {SHORTMUL}
    | "/=" {SHORTDIV}
    | "%=" {SHORTMOD}
    | "|" {BOR}
    | "||" {OR}
    | "^" {BXOR}
    | "~" {BNOT}
    | "<<" {LSHIFT}
    | ">>" {RSHIFT}
    | eof   { EOF}
    | _ as c           { raise_lexer_error lexbuf @@ "Illegal character " ^ Char.escaped c }

and singlelinecomment = parse
    | newline {Lexing.new_line lexbuf; next_token lexbuf}
    | _ {singlelinecomment lexbuf}

and multilinecomment = parse
    | "*/" {next_token lexbuf}
    | newline {Lexing.new_line lexbuf; multilinecomment lexbuf}
    | eof {raise_lexer_error lexbuf "Unexpected end of file in comment"}
    | _ { multilinecomment lexbuf}

and get_char = parse
    | '\\' '/' "'" { CHARLIT('/') }
    | '\\' '\\' "'" {CHARLIT( '\\')}
    | '\\' '0' "'"  {CHARLIT(Char.chr(0))}
    | '\\' 'b' "'" {CHARLIT('\b')}
    | '\\' 'f'  {CHARLIT('\012')}
    | '\\' 'n' "'"{CHARLIT('\n')}
    | '\\' 'r' "'" {CHARLIT('\r')}
    | '\\' 't' "'" {CHARLIT('\t')}
    |  _  [^ '\''] {raise_lexer_error lexbuf "character is not terminated"}
    | [^'\\'] as c  "'" {CHARLIT(c)}
    | _  {raise_lexer_error lexbuf @@ "Illegal character " ^Lexing.lexeme lexbuf}

and get_string  buffer = parse
    | '"'       {STRING (Buffer.contents buffer)}
    | '\\' '/'  { Buffer.add_char buffer '/'; get_string buffer lexbuf }
    | '\\' '\\' { Buffer.add_char buffer '\\'; get_string buffer lexbuf }
    | '\\' 'b'  { Buffer.add_char buffer '\b'; get_string buffer lexbuf }
    | '\\' 'f'  { Buffer.add_char buffer '\012'; get_string buffer lexbuf }
    | '\\' 'n'  { Buffer.add_char buffer '\n'; get_string buffer lexbuf }
    | '\\' 'r'  { Buffer.add_char buffer '\r'; get_string buffer lexbuf }
    | '\\' 't'  { Buffer.add_char buffer '\t'; get_string buffer lexbuf }
    | '\\' '0'  { Buffer.add_char buffer (Char.chr(0)); get_string buffer lexbuf}
    | [^ '"' '\\']+
    { Buffer.add_string buffer (Lexing.lexeme lexbuf);
      get_string buffer lexbuf
    }
    | eof {raise_lexer_error lexbuf "string is not terminated"}
    | _   {raise_lexer_error lexbuf @@ "Illegal string character " ^Lexing.lexeme lexbuf}
