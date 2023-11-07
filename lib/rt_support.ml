open Ast
let rt_functions =
  [
    ( "print",
      ( Location.dummy_code_pos,
        {
          typ = TypV;
          fname = "print";
          formals = [ (TypI, "a") ];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
    ( "getint",
      ( Location.dummy_code_pos,
        {
          typ = TypI;
          fname = "getint";
          formals = [];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
    ( "getfloat",
      ( Location.dummy_code_pos,
        {
          typ = TypF;
          fname = "getfloat";
          formals = [];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
    ( "getcharacter",
      ( Location.dummy_code_pos,
        {
          typ = TypC;
          fname = "getcharacter";
          formals = [];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
    ( "printchar",
      ( Location.dummy_code_pos,
        {
          typ = TypV;
          fname = "printchar";
          formals = [ (TypC, "c") ];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
    ( "printfloat",
      ( Location.dummy_code_pos,
        {
          typ = TypF;
          fname = "printfloat";
          formals = [ (TypF, "f") ];
          body = { loc = Location.dummy_code_pos; node = FunctionBlock []; id = 0 };
        } ) );
  ]

