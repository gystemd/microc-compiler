exception Semantic_error of Location.code_pos * string

open Ast
open Symbol_table
let rec string_of_type  = function
  | TypI -> "int" 
  | TypC -> "char"
  | TypF -> "float"
  | TypB -> "bool"
  | TypV -> "void"
  | TypS(s) -> "struct "^s
  | TypP t1 -> "*"^string_of_type t1
  | TypNull -> "null"
  | TypA(t, v) -> string_of_type t ^ "["^( Option.fold ~none:"" ~some:string_of_int v) ^ "]"

let string_of_uop = function 
  
| Neg -> "-"
| Not -> "!"
| PreInc | PostInc -> "++"
| PreDec | PostDec -> "--"

let string_of_binop = function 
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Comma -> ","
type var_info = Location.code_pos * typ

type fun_info = Location.code_pos * fun_decl

type struct_info = Location.code_pos * struct_decl

type symbols = {
  fun_symbols : fun_info Symbol_table.t;
  var_symbols : var_info Symbol_table.t;
  struct_symbols : struct_info Symbol_table.t;
}
(*
  Function that allows strings to be used as variable initializer

  - If the array is declared with no initial size, the assigned size will be equal to string_length + 1(for null)
  - Otherwise we only assign the string if the array has bigger size. 

  This function  could be modified to handle general array literals
 *)
let string_var_initialization loc vars array_length id string =
  try
    let length = string |> String.length in
    
    if array_length = 0 then
      Symbol_table.add_entry id (loc, TypA (TypC, Some (length + 1))) vars
      |> ignore
    else if length + 1 > array_length then raise
      @@ Semantic_error
           ( loc,
             "Null terminated string length is "
             ^ (length + 1 |> string_of_int)
             ^ " but array was declared with size "
             ^ (array_length |> string_of_int) )
    else
      
      Symbol_table.add_entry id (loc, TypA (TypC, Some array_length)) vars
      |> ignore
  with DuplicateEntry id->
    raise @@ Semantic_error( loc,
    "Variable " ^ id ^ " already defined in current scope")

let rec defined_type_size t =
  (*checks if the given type is complete *)
  match t with
  | TypA (t, Some _) -> defined_type_size t
  | TypA (_, None) -> false
  | _ -> true

let rec check_type structs loc t =
  match t with
  | TypA (TypV, _) ->
      raise @@ Semantic_error( loc, "Trying to define a void array")

  | TypA (_, Some i) when i < 1 ->
      raise @@ Semantic_error( loc, "Array must have size > 0")
  | TypA (t, _) when not (defined_type_size t) ->
      raise @@ Semantic_error( loc, "Array size undefined")
  | TypP TypV -> raise @@ Semantic_error( loc, "Trying to define a void pointer")
  | TypP t -> check_type structs loc t
  | TypS s -> (
      match Symbol_table.lookup s structs with
      | Some _ -> ()
      | None -> raise @@ Semantic_error(loc, "Undefined structure " ^ s))
  | _ -> ()

let check_var_type structs loc t =
  match t with
  | TypV -> raise @@ Semantic_error( loc, "Trying to define a void variable")
  | TypA (_, None) ->
      raise @@ Semantic_error( loc,
        "Array must be declared with an initial size")
  | _ -> check_type structs loc t

let check_var_decl scope loc (t, i) =
  check_var_type scope.struct_symbols loc t;
  try Symbol_table.add_entry i (loc, t) scope.var_symbols |> ignore
  with DuplicateEntry i->
    raise @@ Semantic_error( loc, "Variable " ^ i ^ " already defined in current scope")

let check_fun_type loc t =
  match t with
  | TypA (_, _) | TypP _ | TypNull ->
      raise @@ Semantic_error(
       loc,
       "Illegal function type " ^ string_of_type t)
  | _ -> ()

(*
- Checks that arrays can be unified
- Allows NULL to be assigned to pointers with different element type

*)
let rec match_types loc t1 t2 =
  match (t1, t2) with
  | TypA (t1, Some v), TypA (t2, Some v2) when v = v2 -> match_types loc t1 t2
  | TypA (_, Some v), TypA (_, Some v2) when v <> v2 ->
      raise @@ Semantic_error( loc, "Array size must be the same")
  | TypA (t1, None), TypA (t2, _) | TypA (t1, _), TypA (t2, None) ->
      match_types loc t1 t2
  | TypP _, TypNull -> true
  | TypNull, TypP _ -> true
  | TypP t1, TypP t2 -> match_types loc t1 t2
  | t1, t2 -> t1 = t2


let binaryexp_type loc op et1 et2 =

  match (op, et1, et2) with
  | (Add | Sub | Mult | Div | Mod | Comma), TypI, TypI -> TypI
  | (Add | Sub | Mult | Div | Mod | Comma), TypF, TypF -> TypF
  | (Equal | Neq | Less | Leq | Greater | Geq), TypI, TypI -> TypB
  | (Equal | Neq | Less | Leq | Greater | Geq), TypF, TypF -> TypB
  | (Equal | Neq), TypC, TypC -> TypB (*For simplicity only equality checks are allowed on characters *)
  | (Equal | Neq), TypP _, TypNull -> TypB
  | (Equal | Neq), TypNull, TypP _ -> TypB
  | (Equal | Neq), TypP t1, TypP t2 when match_types loc t1 t2 -> TypB
  | (And | Or | Equal | Neq), TypB, TypB -> TypB
  | _ ->
      raise @@ Semantic_error( loc,
       "Operator " ^ string_of_binop op
      ^ " is not defined when the operands have type " ^ string_of_type et1
      ^ " and " ^ string_of_type et2)

let unaryexp_type loc u et =
  match (u, et) with
  | Neg, TypI -> TypI
  | Neg, TypF -> TypF
  | Not, TypB -> TypB
  | (PreInc | PreDec | PostInc | PostDec), (TypI | TypF) -> et
  | (PreInc | PreDec | PostInc | PostDec), _ | Neg, _ | Not, _ ->
      raise @@ Semantic_error( loc,
      "Operator " ^ string_of_uop u ^ " not defined for type "
      ^ string_of_type et)

(* Assigns a type to the given expression, following language rules *)
let rec expr_type scope e =
  match e.node with
  | Access a -> access_type scope a
  | Assign (a, e) -> (
      let at = access_type scope a in
      match at with
      | TypA (_, _) ->
          raise @@ Semantic_error (e.loc, "trying to reassign array")
      | _ ->
          let et = expr_type scope e in
          if match_types e.loc at et then at
          else
            raise @@ Semantic_error ( e.loc,
            "Cannot assign a value of type " ^ string_of_type et
            ^ " to a variable of type " ^ string_of_type at))
  | ShortAssign (a, op, e) ->
    (* Desugar a += e to a = a+e, to simplify checks *)
      let a_expr = { loc = e.loc; node = Access a; id = e.id } in
      let bin_expr =
        { loc = e.loc; node = BinaryOp (op, a_expr, e); id = e.id }
      in
      expr_type scope { loc = e.loc; node = Assign (a, bin_expr); id = e.id }
  | Addr a ->
      let at = access_type scope a in
      TypP at
  | ILiteral _ -> TypI
  | CLiteral _ -> TypC
  | FLiteral _ -> TypF
  | BLiteral _ -> TypB
  | String s -> TypA (TypC, Some (String.length s + 1))
  | Null -> TypNull
  | UnaryOp (u, e1) ->
      let et = expr_type scope e1 in
      unaryexp_type e.loc u et
  | BinaryOp (op, e1, e2) ->
      let et1 = expr_type scope e1 in
      let et2 = expr_type scope e2 in
      binaryexp_type e.loc op et1 et2
  | Call (id, params) -> (
      (*
        - Checks that the called function exists in the current scope
        - Checks that the correct number of arguments is passed
        . Checks that the parameters passed have the correct type  
       *)
      let params_types = List.map (expr_type scope) params in
      match Symbol_table.lookup id scope.fun_symbols with
      | Some (_, f) -> (
          let formals_types = List.map (fun (t, _) -> t) f.formals in
          match (List.length params_types, List.length formals_types) with
          | l1, l2 when l1 < l2 ->
              raise @@ Semantic_error( e.loc,
                "Missing one or more arguments in function call")
          | l1, l2 when l1 > l2 ->
              raise @@ Semantic_error( e.loc,
                "Too many arguments in function call")
          | _ ->
              List.iter2
                (fun ft pt ->
                  if match_types e.loc ft pt then ()
                  else
                    raise @@ Semantic_error( e.loc,
                     "Function " ^ f.fname ^ " expects a parameter with type "
                    ^ string_of_type ft ^ " but an expression with type "
                    ^ string_of_type pt ^ " was passed"))
                formals_types params_types;
              f.typ)
      | None ->
          raise @@ Semantic_error( e.loc,  "Function " ^ id ^ "not defined"))
and access_type scope a =
  match a.node with
  | AccVar v -> (
      match Symbol_table.lookup v scope.var_symbols with
      | Some (_, t) -> t
      | None ->
          raise @@ Semantic_error( a.loc,
          "Variable " ^ v ^ " not defined in current scope"))
  | AccDeref e -> (
      match expr_type scope e with
      | TypP t -> t
      | _ ->
          raise @@ Semantic_error( a.loc, "Trying to dereference a non pointer"))
  | AccIndex (a, e) -> (
      match expr_type scope e with
      | TypI -> (
          match access_type scope a with
          | TypA (t, _) -> t
          | _ ->
              raise @@ Semantic_error( a.loc, "Cannot access index of non-array")
          )
      | _ -> raise @@ Semantic_error( a.loc, "Index of array must be an integer")
      )
  | AccField (s, f) -> (
    (* 
      - Checks that the variable is an existing structure
      - CHecks that the field exists
    *)
      match access_type scope s with
      | TypS s -> (
          match Symbol_table.lookup s scope.struct_symbols with
          | Some (_, s) -> (
              match List.find_opt (fun (_, i) -> i = f) s.fields with
              | Some (t, _) -> t
              | None ->
                  raise @@ Semantic_error (a.loc,
                  "Field " ^ f ^ " does not exists in structure " ^ s.sname))
          | None ->
              raise @@ Semantic_error( a.loc,
              "Structure " ^ s ^ " does not exists"))
      | _ ->
          raise @@ Semantic_error( a.loc,
            "Trying to access field of non structure variable"))

let rec check_stmt scope ftype s =
  match s.node with
| If (e, s1, s2) ->
    if expr_type scope e <> TypB then
      raise @@ Semantic_error( s.loc, "If condition is not boolean")
    else
      begin
        let _ = check_stmt scope ftype s1 in
        let _ = check_stmt scope ftype s2 in
        true
      end
  | DoWhile (e, s)
  | While (e, s) ->
      if expr_type scope e <> TypB then
        raise @@ Semantic_error( s.loc, " Loop condition is not boolean")
      else
        let _ = check_stmt scope ftype s in
        true
  | Expr e -> expr_type scope e |> ignore; true
  | Return (Some e) ->
      if expr_type scope e <> ftype then
        raise @@ Semantic_error (s.loc,
          "Return type does not match function signature")
      else false
  | Return None ->
      if ftype <> TypV then
        raise @@ Semantic_error( s.loc, "missing return value")
      else false 
| Block stmts ->
    let new_scope =
      { scope with var_symbols = Symbol_table.begin_block scope.var_symbols }
    in
    let result = List.fold_left (fun acc stmtordec ->
      if not acc then
        raise @@ Semantic_error(stmtordec.loc, "instruction after return found")
      else
       acc && check_stmtordec new_scope ftype stmtordec) true stmts in
    Symbol_table.end_block new_scope.var_symbols |> ignore;
    result

and check_stmtordec scope ftype s =
  match s.node with
  | Dec (t, i, None) -> check_var_decl scope s.loc (t, i); true
  | Dec (t, i, Some e) -> (
      match (t, e.node) with
      | TypA (TypC, None), String str ->
          string_var_initialization s.loc scope.var_symbols 0 i str;
      | TypA (TypC, Some v), String str ->
          string_var_initialization s.loc scope.var_symbols v i str;
      | _ -> (
          check_var_decl scope s.loc (t, i);
          let et = expr_type scope e in
          match et with
          | TypA (_, _) -> (* Array initializers are disallowed *)
              raise @@ Semantic_error( s.loc,
                "Array is not a valid value initializer")
          | _ ->
              if match_types s.loc t et then ()
              else raise @@ Semantic_error( s.loc, "Value of different type"))); true
  | Stmt s -> check_stmt scope ftype s


let check_parameter scope loc (t, i) =
  (* Function parameters are treated slightly different from normal variables. We only forbid void variables, but unsized arrays are allowed *)
  match t with
  | TypV -> raise @@ Semantic_error (loc, "Illegal void parameter " ^ i)
  | _ -> (
      check_type scope.struct_symbols loc t;
      try Symbol_table.add_entry i (loc, t) scope.var_symbols |> ignore
      with DuplicateEntry i ->
        raise @@ Semantic_error (loc,
        "Parameter " ^ i ^ " already defined in current scope"))


(*
  - Checks that the function type is allowed
  - Adds function name to the symbol table, for recursive calls
  - Checks that paramets are properly defined
  - Recursively checks the function body
*)
let check_func f scope loc =

  check_fun_type loc f.typ;
  let rec_scope =
    try Symbol_table.add_entry f.fname (loc, f) scope.fun_symbols
    with Symbol_table.DuplicateEntry d->
      raise @@ Semantic_error( loc,
      "function " ^ d ^ " already defined")
  in
  let new_scope =
    {
      scope with
      fun_symbols = rec_scope;
      var_symbols = Symbol_table.begin_block scope.var_symbols;
    }
  in
  List.iter (check_parameter new_scope loc) f.formals;
  match f.body.node with
  | Block stmts ->
    List.fold_left (fun acc stmtordec ->
      if not acc then
        raise @@ Semantic_error(stmtordec.loc, "instruction after return found")
      else
        acc && check_stmtordec new_scope f.typ stmtordec) true stmts |> ignore;
  | _ -> check_stmt new_scope f.typ f.body |> ignore

let rec global_expr_type scope loc e =
  (*checks that a global variable is initialized with a constant value *)
  match e.node with
  | ILiteral _ | CLiteral _ | BLiteral _ | FLiteral _ | String _ | Null ->
      expr_type scope e
  | UnaryOp (u, e) ->
      let et = global_expr_type scope loc e in
      unaryexp_type loc u et
  | BinaryOp (o, e1, e2) ->
      let et1 = global_expr_type scope loc e1 in
      let et2 = global_expr_type scope loc e2 in
      binaryexp_type loc o et1 et2
  | _ ->
      raise @@ Semantic_error( loc,
        "Cannot assign non-constant value at compile time to a global variable")

let check_topdecl scope node =
  match node.node with
  | Fundecl f -> check_func f scope node.loc
  | Vardec (t, i, None) -> check_var_decl scope node.loc (t, i)
  | Vardec (t, i, Some e) -> (
      match (t, e.node) with
      | TypA (TypC, None), String str ->
          string_var_initialization node.loc scope.var_symbols 0 i str
      | TypA (TypC, Some v), String str ->
          string_var_initialization node.loc scope.var_symbols v i str
      | _ ->
          check_var_decl scope node.loc (t, i);
          let et = global_expr_type scope node.loc e in
          if match_types node.loc t et then () (* since global expression cannot be arrays we can directly use match_types *)
          else raise @@ Semantic_error( node.loc, "Value of different type"))
  | Structdecl s -> (
      try
        (*immediately adds its own name to defined structs *)
        Symbol_table.add_entry s.sname (node.loc, s) scope.struct_symbols
        |> ignore;
        let struct_scope =
          {
            scope with
            var_symbols = Symbol_table.begin_block scope.var_symbols;
          }
        in
        (*checks that fields are properly declared  and adds them to the inner scope*)
        List.iter
          (fun f ->
            match f with
            | TypS f, id when f = s.sname ->
                raise @@ Semantic_error( node.loc,
                "Field " ^ id ^ " has incomplete type")
            | _ -> check_var_decl struct_scope node.loc f)
          s.fields;
        Symbol_table.end_block struct_scope.var_symbols |> ignore
      with DuplicateEntry d->
        raise @@ Semantic_error( node.loc,
        "Structure " ^ d ^ " already defined"))

let check_global_properties scope =
  (*function used to check "global properties about the program", here it's used to verify the presence of main function *)
  let m = Symbol_table.lookup "main" scope.fun_symbols in
  match m with
  | Some (_, { typ = TypV; fname = "main"; formals = []; body=_}) -> ()
  | Some (_, { typ = TypI; fname = "main"; formals = []; body=_ }) -> ()
  | Some (loc, _) -> raise @@ Semantic_error( loc, "Invalid signature of main")
  | None -> raise @@ Semantic_error( Location.dummy_code_pos, " No main function defined")

let rt_support=
(*adds library functions info *)
  let init_scope = Symbol_table.empty_table () in
  List.iter
    (fun (name, f) -> Symbol_table.add_entry name f init_scope |> ignore)
    Rt_support.rt_functions;
  init_scope

let type_check (Prog topdecls) =
  let toplevel_scope =
    {
      fun_symbols = rt_support;
      var_symbols = Symbol_table.empty_table ();
      struct_symbols = Symbol_table.empty_table ();
    }
  in
  (*Iterate and check top declarations *)
  List.iter (check_topdecl toplevel_scope) topdecls;
  check_global_properties toplevel_scope |> ignore;
  Prog topdecls