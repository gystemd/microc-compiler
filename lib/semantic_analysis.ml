exception Semantic_error of Location.code_pos * string

open Ast
open Symbol_table

type context =
  { fun_symbols : (Location.code_pos * fun_decl) Symbol_table.t
  ; var_symbols : (Location.code_pos * typ) Symbol_table.t
  ; struct_symbols : (Location.code_pos * struct_decl) Symbol_table.t
  }

let add_fun_struct_sign symbols topdecl =
  match topdecl.node with
  | Structdecl s ->
    (try Symbol_table.add_entry s.sname (topdecl.loc, s) symbols.struct_symbols with
     | DuplicateEntry d ->
       raise @@ Semantic_error (topdecl.loc, "structure " ^ d ^ " already defined"))
    |> ignore
  | Fundecl f ->
    (try Symbol_table.add_entry f.fname (topdecl.loc, f) symbols.fun_symbols with
     | DuplicateEntry d ->
       raise @@ Semantic_error (topdecl.loc, "function " ^ d ^ " already defined"))
    |> ignore
  | _ -> ()
;;

let check_main_signature symbols =
  let m = Symbol_table.lookup "main" symbols.fun_symbols in
  match m with
  | None -> raise @@ Semantic_error (Location.dummy_code_pos, "main function missing")
  | Some (_, { typ = TypI; fname = "main"; formals = []; body = _ }) -> () (*int main()*)
  | Some (_, { typ = TypV; fname = "main"; formals = []; body = _ }) -> () (*void main()*)
  | Some (loc, _) -> raise @@ Semantic_error (loc, "main function signature is not valid")
;;

let init_string location var_symbols arr_length id string =
  try
    let length = string |> String.length in
    if arr_length = 0
    then
      Symbol_table.add_entry id (location, TypA (TypC, Some (length + 1))) var_symbols
      |> ignore
    else if length + 1 > arr_length
    then
      raise
      @@ Semantic_error
           ( location
           , "Null terminated string length is "
             ^ (length + 1 |> string_of_int)
             ^ " but array was declared with size "
             ^ (arr_length |> string_of_int) )
    else
      Symbol_table.add_entry id (location, TypA (TypC, Some arr_length)) var_symbols
      |> ignore
  with
  | DuplicateEntry id -> raise @@ Semantic_error (location, "error: redefinition of " ^ id)
;;

(** Checks if a type has a completely defined size at compile time
    @param typ The type to be checked
    @return true if the type is complete, false otherwise *)
let rec is_complete_type typ =
  match typ with
  | TypA (t, Some _) -> is_complete_type t
  | TypA (_, None) -> false
  | _ -> true
;;

(** Checks if two types can be unified
    @param location The location of the expression in the source code
    @param t1 The first type
    @param t2 The second type
    @return true if the types can be unified, false otherwise *)
let rec compare_types location t1 t2 =
  match t1, t2 with
  | TypA (t1, Some v), TypA (t2, Some v2) when v = v2 -> compare_types location t1 t2
  | TypA (_, Some v), TypA (_, Some v2) when v <> v2 ->
    raise @@ Semantic_error (location, "Array size must be the same")
  | TypA (t1, None), TypA (t2, _) | TypA (t1, _), TypA (t2, None) ->
    compare_types location t1 t2
  | TypP _, TypNull -> true
  | TypNull, TypP _ -> true
  | TypP t1, TypP t2 -> compare_types location t1 t2
  | t1, t2 -> t1 = t2
;;

(** Checks that either a function parameter or a variable declaration is well typed.
    @param structs The current symbol table of structs
    @param location The location of the declaration in the source code
    @param t The type of the variable *)
let rec check_type_declaration structs location t =
  match t with
  | TypA (TypV, _) -> raise @@ Semantic_error (location, "Cannot declare a void array")
  | TypA (_, Some i) when i < 1 ->
    raise @@ Semantic_error (location, "Cannot declare an array with size less than one")
  | TypA (t, _) when not (is_complete_type t) ->
    raise @@ Semantic_error (location, "Cannot declare undefined-size array")
  | TypP TypV -> raise @@ Semantic_error (location, "Cannot declare a void pointer")
  | TypP t -> check_type_declaration structs location t
  | TypS s ->
    (match Symbol_table.lookup s structs with
     | Some _ -> ()
     | None -> raise @@ Semantic_error (location, "Undefined struct " ^ s))
  | _ -> ()
;;

(** Infers the type of a binary-operator expression
    @param location The location of the expression in the source code
    @param op The binary operator
    @param et1 The type of the first operand
    @param et2 The type of the second operand
    @return The type of the expression *)
let binaryexp_type location op et1 et2 =
  match op, et1, et2 with
  | (Add | Sub | Mult | Div | Mod | Comma), TypI, TypI -> TypI
  | (Add | Sub | Mult | Div | Mod | Comma), TypF, TypF -> TypF
  | (Equal | Neq | Less | Leq | Greater | Geq), TypI, TypI -> TypB
  | (Equal | Neq | Less | Leq | Greater | Geq), TypF, TypF -> TypB
  | (Equal | Neq), TypC, TypC ->
    TypB (*For simplicity only equality checks are allowed on characters *)
  | (Equal | Neq), TypP _, TypNull -> TypB
  | (Equal | Neq), TypNull, TypP _ -> TypB
  | (Equal | Neq), TypP t1, TypP t2 when compare_types location t1 t2 -> TypB
  | (And | Or | Equal | Neq), TypB, TypB -> TypB
  | (BOr | BAnd | BXor | LShift | RShift), TypI, TypI -> TypI
  | _ ->
    raise
    @@ Semantic_error
         ( location
         , "Operator "
           ^ string_of_binop op
           ^ " is not defined when the operands have type "
           ^ string_of_type et1
           ^ " and "
           ^ string_of_type et2 )
;;

(** Infers the type of an unary-operator expression
    @param location The location of the expression in the source code
    @param u The unary operator
    @param et The type of the operand
    @return The type of the expression *)
let unaryexp_type location unop exp_typ =
  match unop, exp_typ with
  | Neg, TypI -> TypI
  | Neg, TypF -> TypF
  | Not, TypB -> TypB
  | BNot, TypI -> TypI
  | (PreIncr | PreDecr | PostIncr | PostDecr), (TypI | TypF) -> exp_typ
  | (PreIncr | PreDecr | PostIncr | PostDecr), _ | Neg, _ | Not, _ | BNot, _ ->
    raise
    @@ Semantic_error
         ( location
         , "Operator "
           ^ string_of_uop unop
           ^ " not defined for type "
           ^ string_of_type exp_typ )
;;

(** Infers the type of an expression
    @param symbols The current symbol table
    @param expr The expression to be inferred
    @return The type of the expression *)
let rec expr_type symbols expr =
  match expr.node with
  | Access a -> access_type symbols a
  | Assign (a, e) ->
    let at = access_type symbols a in
    (match at with
     | TypA (_, _) -> raise @@ Semantic_error (e.loc, "array cannot be reassigned")
     | _ ->
       let et = expr_type symbols e in
       if compare_types e.loc at et
       then at
       else
         raise
         @@ Semantic_error
              ( e.loc
              , "Cannot assign a value of type "
                ^ string_of_type et
                ^ " to a variable of type "
                ^ string_of_type at ))
  | ShortAssign (a, op, e) ->
    (* Desugar a += e to a = a+e, to simplify checks *)
    let a_expr = { loc = e.loc; node = Access a; id = e.id } in
    let bin_expr = { loc = e.loc; node = BinaryOp (op, a_expr, e); id = e.id } in
    expr_type symbols { loc = e.loc; node = Assign (a, bin_expr); id = e.id }
  | Addr a ->
    let at = access_type symbols a in
    TypP at
  | ILiteral _ -> TypI
  | CLiteral _ -> TypC
  | FLiteral _ -> TypF
  | BLiteral _ -> TypB
  | String s -> TypA (TypC, Some (String.length s + 1))
  | Null -> TypNull
  | UnaryOp (u, e1) ->
    let et = expr_type symbols e1 in
    unaryexp_type expr.loc u et
  | BinaryOp (op, e1, e2) ->
    let et1 = expr_type symbols e1 in
    let et2 = expr_type symbols e2 in
    binaryexp_type expr.loc op et1 et2
  | SizeOf e ->
    expr_type symbols e |> ignore;
    TypI
  | Call (id, params) ->
    let params_types = List.map (expr_type symbols) params in
    (match Symbol_table.lookup id symbols.fun_symbols with
     | Some (_, f) ->
       let formals_types = List.map (fun (t, _) -> t) f.formals in
       (match List.length params_types, List.length formals_types with
        | l1, l2 when l1 < l2 ->
          raise @@ Semantic_error (expr.loc, "too few arguments to function " ^ f.fname)
        | l1, l2 when l1 > l2 ->
          raise @@ Semantic_error (expr.loc, "Too many arguments to function " ^ f.fname)
        | _ ->
          List.iter2
            (fun ft pt ->
              if compare_types expr.loc ft pt
              then ()
              else
                raise
                @@ Semantic_error
                     ( expr.loc
                     , "Function "
                       ^ f.fname
                       ^ " expects a parameter with type "
                       ^ string_of_type ft
                       ^ " but an expression with type "
                       ^ string_of_type pt
                       ^ " was passed" ))
            formals_types
            params_types;
          f.typ)
     | None -> raise @@ Semantic_error (expr.loc, "Function " ^ id ^ "not defined"))

and access_type symbols a =
  match a.node with
  | AccVar v ->
    (match Symbol_table.lookup v symbols.var_symbols with
     | Some (_, t) -> t
     | None ->
       raise @@ Semantic_error (a.loc, "Variable " ^ v ^ " not defined in current scope"))
  | AccDeref expr ->
    (match expr_type symbols expr with
     | TypP t -> t
     | _ -> raise @@ Semantic_error (a.loc, "Trying to dereference a non pointer"))
  | AccIndex (a, expr) ->
    (match expr_type symbols expr with
     | TypI ->
       (match access_type symbols a with
        | TypA (t, _) -> t
        | _ -> raise @@ Semantic_error (a.loc, "Cannot access index of non-array"))
     | _ -> raise @@ Semantic_error (a.loc, "Index of array must be an integer"))
  | AccStructField (s, field) ->
    (match access_type symbols s with
     | TypS s ->
       (match Symbol_table.lookup s symbols.struct_symbols with
        | Some (_, s) ->
          (match List.find_opt (fun (_, i) -> i = field) s.fields with
           | Some (t, _) -> t
           | None ->
             raise
             @@ Semantic_error
                  (a.loc, "Field " ^ field ^ " does not exists in structure " ^ s.sname))
        | None -> raise @@ Semantic_error (a.loc, "Structure " ^ s ^ " does not exists"))
     | _ ->
       raise @@ Semantic_error (a.loc, "Trying to access field of non structure variable"))
;;

(** Checks if a variable declaration is well typed
    @param symbols The current symbol table
    @param location The location of the declaration in the source code
    @param t The type of the variable
    @param i The name of the variable *)
let var_decl_type_check symbols location (t, i) =
  (match t with
   | TypV -> raise @@ Semantic_error (location, "Cannot declare a void variable")
   | TypA (_, None) ->
     raise @@ Semantic_error (location, "Size missing for array declaration")
   | _ -> check_type_declaration symbols.struct_symbols location t);
  try Symbol_table.add_entry i (location, t) symbols.var_symbols |> ignore with
  | DuplicateEntry i -> raise @@ Semantic_error (location, "error: redefinition of " ^ i)
;;

(**
   Checks that a statement is well typed
   @param symbols The current symbol table
   @param ftype The return type of the function
   @param statement The statement to be checked
*)
let rec stmt_type_check symbols ftype statement =
  match statement.node with
  | If (cond, then_block, else_block) ->
    if expr_type symbols cond <> TypB
    then
      raise
      @@ Semantic_error (statement.loc, "Expected boolean expression in if condition")
    else (
      let _ = stmt_type_check symbols ftype then_block in
      let _ = stmt_type_check symbols ftype else_block in
      true)
  | DoWhile (cond, body) | While (cond, body) ->
    if expr_type symbols cond <> TypB
    then
      raise @@ Semantic_error (body.loc, "Expected boolean expression in while condition")
    else (
      let _ = stmt_type_check symbols ftype body in
      true)
  | Expr expr ->
    expr_type symbols expr |> ignore;
    true
  | Return (Some expr) ->
    if expr_type symbols expr <> ftype
    then
      raise
      @@ Semantic_error (statement.loc, "Return type does not match function signature")
    else false
  | Return None ->
    if ftype <> TypV
    then raise @@ Semantic_error (statement.loc, "missing return value")
    else false
  | Block stmts ->
    let new_scope =
      { symbols with var_symbols = Symbol_table.begin_block symbols.var_symbols }
    in
    let result =
      List.fold_left
        (fun acc stmtordec ->
          if not acc
          then raise @@ Semantic_error (stmtordec.loc, "instruction after return found")
          else acc && stmtordec_type_check new_scope ftype stmtordec)
        true
        stmts
    in
    Symbol_table.end_block new_scope.var_symbols |> ignore;
    result

(** Checks that statements and declarations are well typed
    @param symbols The current symbol table
    @param ftype The return type of the function
    @param sordec The statement or declaration to be checked *)
and stmtordec_type_check symbols ftype sordec =
  match sordec.node with
  | DecList l ->
    let check_var loc x =
      match x with
      | t, id, None -> var_decl_type_check symbols loc (t, id)
      | t, id, Some e ->
        (match t, e.node with
         | TypA (TypC, None), String str -> init_string loc symbols.var_symbols 0 id str
         | TypA (TypC, Some v), String str -> init_string loc symbols.var_symbols v id str
         | _ ->
           var_decl_type_check symbols loc (t, id);
           let et = expr_type symbols e in
           (match et with
            | TypA (_, _) ->
              (* Array initializers are disallowed *)
              raise @@ Semantic_error (loc, "Array is not a valid value initializer")
            | _ ->
              if compare_types loc t et
              then ()
              else raise @@ Semantic_error (loc, "Value of different type")))
    in
    List.iter (check_var sordec.loc) l;
    true
  | Stmt s -> stmt_type_check symbols ftype s
;;

(** Checks that a function parameter is well typed.
    @param symbols The current symbol table
    @param location The location of the parameter in the source code.
    @param typ The type of the parameter
    @param id The name of the parameter *)
let parameter_type_check symbols location (typ, id) =
  (* Function parameters are treated slightly different from normal variables. We only forbid void variables, but unsized arrays are allowed *)
  match typ with
  | TypV -> raise @@ Semantic_error (location, "Illegal void parameter " ^ id)
  | _ ->
    check_type_declaration symbols.struct_symbols location typ;
    (try Symbol_table.add_entry id (location, typ) symbols.var_symbols |> ignore with
     | DuplicateEntry i ->
       raise
       @@ Semantic_error
            (location, "Parameter " ^ i ^ " already defined in current symbols"))
;;

(** Checks that a function is well typed.
    @param func The function to be checked
    @param symbols The current symbol table
    @param location The location of the function in the source code. *)
let func_type_check func symbols location =
  match func.typ with
  | TypA (_, _) | TypP _ | TypNull ->
    raise @@ Semantic_error (location, "Illegal function type " ^ string_of_type func.typ)
  | _ ->
    let new_scope =
      { symbols with var_symbols = Symbol_table.begin_block symbols.var_symbols }
    in
    List.iter (parameter_type_check new_scope location) func.formals;
    (match func.body.node with
     (*
        The Block case is intercepted to avoid the stmt_type_check function to forget the injection of parameters in
        the function scope.
     *)
     | Block statements ->
       List.fold_left
         (fun acc stmtordec ->
           if not acc
           then raise @@ Semantic_error (stmtordec.loc, "instruction after return found")
           else acc && stmtordec_type_check new_scope func.typ stmtordec)
         true
         statements
       |> ignore
     | _ -> stmt_type_check new_scope func.typ func.body |> ignore)
;;

let rec global_expr_type symbols location expr =
  (*checks that a global variable is initialized with a constant value *)
  match expr.node with
  | ILiteral _ | CLiteral _ | BLiteral _ | FLiteral _ | String _ | Null ->
    expr_type symbols expr
  | UnaryOp (u, e) ->
    let et = global_expr_type symbols location e in
    unaryexp_type location u et
  | BinaryOp (o, e1, e2) ->
    let et1 = global_expr_type symbols location e1 in
    let et2 = global_expr_type symbols location e2 in
    binaryexp_type location o et1 et2
  | _ ->
    raise
    @@ Semantic_error
         ( location
         , "Cannot assign non-constant value at compile time to a global variable" )
;;

let topdecl_type_check symbols node =
  match node.node with
  | Fundecl f -> func_type_check f symbols node.loc
  | VarDecList l ->
    let check_var loc x =
      match x with
      | t, i, None -> var_decl_type_check symbols loc (t, i)
      | t, i, Some expr ->
        (match t, expr.node with
         | TypA (TypC, None), String str -> init_string loc symbols.var_symbols 0 i str
         | TypA (TypC, Some v), String str -> init_string loc symbols.var_symbols v i str
         | _ ->
           var_decl_type_check symbols loc (t, i);
           let et = global_expr_type symbols loc expr in
           if compare_types loc t et
           then
             ()
             (* since global expression cannot be arrays we can directly use match_types *)
           else raise @@ Semantic_error (loc, "Value of different type"))
    in
    List.iter (check_var node.loc) l
  | Structdecl s ->
    (try
       let struct_scope =
         { symbols with var_symbols = Symbol_table.begin_block symbols.var_symbols }
       in
       (*checks that fields are properly declared  and adds them to the inner symbols*)
       List.iter
         (fun f ->
           match f with
           | TypS f, id when f = s.sname ->
             raise @@ Semantic_error (node.loc, "Field " ^ id ^ " has incomplete type")
           | _ -> var_decl_type_check struct_scope node.loc f)
         s.fields;
       Symbol_table.end_block struct_scope.var_symbols |> ignore
     with
     | DuplicateEntry d ->
       raise @@ Semantic_error (node.loc, "Structure " ^ d ^ " already defined"))
;;

let type_check (Prog topdecls) =
  (* Add runtime functions to the current scope *)
  let runtime_functions = Symbol_table.empty_table () in
  List.iter
    (fun (name, f) -> Symbol_table.add_entry name f runtime_functions |> ignore)
    Rt_support.rt_functions;
  let context =
    { fun_symbols = runtime_functions
    ; var_symbols = Symbol_table.empty_table ()
    ; struct_symbols = Symbol_table.empty_table ()
    }
  in
  List.iter (add_fun_struct_sign context) topdecls;
  List.iter (topdecl_type_check context) topdecls;
  check_main_signature context |> ignore;
  Prog topdecls
;;
