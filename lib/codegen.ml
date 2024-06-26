exception Codegen_error of string

open Ast
module L = Llvm

let llcontext = L.global_context ()
let int_lltype = L.i32_type llcontext
let float_lltype = L.float_type llcontext
let bool_lltype = L.i1_type llcontext
let char_lltype = L.i8_type llcontext
let void_lltype = L.void_type llcontext
let llvm_zero = L.const_int int_lltype 0
let llvm_true = L.const_int bool_lltype 1
let llvm_false = L.const_int bool_lltype 0

type environment =
  { fun_symbols : L.llvalue Symbol_table.t
  ; var_symbols : L.llvalue Symbol_table.t
  ; struct_symbols : (L.lltype * string list) Symbol_table.t
  }

let rec lltype_of_typ structs = function
  | TypI -> int_lltype
  | TypF -> float_lltype
  | TypC -> char_lltype
  | TypB -> bool_lltype
  | TypV -> void_lltype
  | TypA (t, None) | TypP t -> L.pointer_type (lltype_of_typ structs t)
  | TypA (t, Some v) -> L.array_type (lltype_of_typ structs t) v
  | TypNull -> L.pointer_type void_lltype
  | TypS n ->
    (match Symbol_table.lookup n structs with
     | Some (t, _) -> t
     | None -> raise @@ Codegen_error ("structure " ^ n ^ " not defined"))
;;

let add_rt_support llmodule symbols =
  let params_to_array params =
    params
    |> List.map fst
    |> List.map (lltype_of_typ symbols.struct_symbols)
    |> Array.of_list
  in
  let fun_type f =
    L.function_type
      (lltype_of_typ symbols.struct_symbols f.typ)
      (params_to_array f.formals)
  in
  Rt_support.rt_functions
  |> List.map (fun (n, (_, f)) -> n, fun_type f)
  |> List.iter (fun (n, t) ->
    Symbol_table.add_entry n (L.declare_function n t llmodule) symbols.fun_symbols
    |> ignore)
;;

let normalize_expr e t = if L.is_undef e then L.const_pointer_null t else e

let add_terminator builder after =
  let terminator = L.block_terminator (L.insertion_block builder) in
  if Option.is_none terminator then after builder |> ignore else ()
;;

let codegen_unop = function
  | t, Neg when t = int_lltype -> L.build_neg
  | t, BNot when t = int_lltype -> L.build_not
  | t, Neg when t = float_lltype -> L.build_fneg
  | t, Not when t = bool_lltype -> L.build_not
  | _ -> raise @@ Codegen_error "Invald unary operator for global variable"
;;

let codegen_binop = function
  | t1, t2, Add when t1 = int_lltype && t2 = int_lltype -> L.build_add
  | t1, t2, Sub when t1 = int_lltype && t2 = int_lltype -> L.build_sub
  | t1, t2, Div when t1 = int_lltype && t2 = int_lltype -> L.build_sdiv
  | t1, t2, Mult when t1 = int_lltype && t2 = int_lltype -> L.build_mul
  | t1, t2, Mod when t1 = int_lltype && t2 = int_lltype -> L.build_srem
  | t1, t2, BAnd when t1 = int_lltype && t2 = int_lltype -> L.build_and
  | t1, t2, BOr when t1 = int_lltype && t2 = int_lltype -> L.build_or
  | t1, t2, BXor when t1 = int_lltype && t2 = int_lltype -> L.build_xor
  | t1, t2, LShift when t1 = int_lltype && t2 = int_lltype -> L.build_shl
  | t1, t2, RShift when t1 = int_lltype && t2 = int_lltype -> L.build_lshr
  | t1, t2, Less when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_lltype && t2 = int_lltype -> L.build_icmp L.Icmp.Ne
  | t1, t2, And when t1 = bool_lltype && t2 = bool_lltype -> L.build_and
  | t1, t2, Or when t1 = bool_lltype && t2 = bool_lltype -> L.build_or
  | t1, t2, Equal when t1 = bool_lltype && t2 = bool_lltype -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_lltype && t2 = bool_lltype -> L.build_icmp L.Icmp.Ne
  | t1, t2, Equal when t1 = char_lltype && t2 = char_lltype -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = char_lltype && t2 = char_lltype -> L.build_icmp L.Icmp.Ne
  | t1, t2, Add when t1 = float_lltype && t2 = float_lltype -> L.build_fadd
  | t1, t2, Sub when t1 = float_lltype && t2 = float_lltype -> L.build_fsub
  | t1, t2, Mult when t1 = float_lltype && t2 = float_lltype -> L.build_fmul
  | t1, t2, Div when t1 = float_lltype && t2 = float_lltype -> L.build_fdiv
  | t1, t2, Less when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_lltype && t2 = float_lltype -> L.build_fcmp L.Fcmp.One
  | t1, t2, Equal
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer -> L.build_icmp L.Icmp.Ne
  | _ -> raise @@ Codegen_error "Type mismatch between operands"
;;

let codegen_const_binop = function
  | t1, t2, Add when t1 = int_lltype && t2 = int_lltype -> L.const_add
  | t1, t2, Sub when t1 = int_lltype && t2 = int_lltype -> L.const_sub
  | t1, t2, Mult when t1 = int_lltype && t2 = int_lltype -> L.const_mul
  | t1, t2, Div when t1 = int_lltype && t2 = int_lltype -> L.const_sdiv
  | t1, t2, Mod when t1 = int_lltype && t2 = int_lltype -> L.const_srem
  | t1, t2, Less when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_lltype && t2 = int_lltype -> L.const_icmp L.Icmp.Ne
  | t1, t2, Add when t1 = float_lltype && t2 = float_lltype -> L.const_fadd
  | t1, t2, Sub when t1 = float_lltype && t2 = float_lltype -> L.const_fsub
  | t1, t2, Div when t1 = float_lltype && t2 = float_lltype -> L.const_fdiv
  | t1, t2, Mult when t1 = float_lltype && t2 = float_lltype -> L.const_fmul
  | t1, t2, Less when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_lltype && t2 = float_lltype -> L.const_fcmp L.Fcmp.One
  | t1, t2, And when t1 = bool_lltype && t2 = bool_lltype -> L.const_and
  | t1, t2, Or when t1 = bool_lltype && t2 = bool_lltype -> L.const_or
  | t1, t2, Equal when t1 = bool_lltype && t2 = bool_lltype -> L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_lltype && t2 = bool_lltype -> L.const_icmp L.Icmp.Ne
  | _ ->
    raise @@ Codegen_error "Mismatch between type of global variable and initial value"
;;

(* This function builds the LLVM instructions for pre-increment, post-increment,
   pre-decrement, and post-decrement operations. *)
let build_pre_post_op builder op value =
  (* Constants for incrementing and decrementing integer and floating point values. *)
  let llvm_one = L.const_int int_lltype 1 in
  let llvm_onef = L.const_float float_lltype 1.0 in
  let inc_op = function
    | (PreIncr | PostIncr), t when t = int_lltype -> L.build_add llvm_one
    | (PreIncr | PostIncr), t when t = float_lltype -> L.build_fadd llvm_one
    (* for decrement we have to flip the arguments to allow inc_op to be single
       argument function*)
    | (PreDecr | PostDecr), t when t = int_lltype -> Fun.flip L.build_sub llvm_one
    | (PreDecr | PostDecr), t when t = float_lltype -> Fun.flip L.build_fsub llvm_onef
    | _ -> raise @@ Codegen_error ("Invalid argument for operator " ^ show_uop op)
  in
  (* Get the correct LLVM build function for the operation and type. *)
  let apply_op = inc_op (op, L.element_type (L.type_of value)) in
  (* Save the value before the operation, to return in case of a post operator. *)
  let before = L.build_load value "" builder in
  (* Apply the operation and store the result. *)
  let after = apply_op before "" builder in
  L.build_store after value builder |> ignore;
  (* Return the value before the operation for post operators, and the value after the operation for pre operators. *)
  if op = PreIncr || op = PreDecr then after else before
;;

let rec codegen_expr symbols builder expr =
  match expr.node with
  | Null -> L.undef (int_lltype |> L.pointer_type)
  | ILiteral i -> L.const_int int_lltype i
  | FLiteral f -> L.const_float float_lltype f
  | BLiteral b -> if b then llvm_true else llvm_false
  | CLiteral c -> L.const_int char_lltype (Char.code c)
  | String s -> L.build_global_string (s ^ "\000") "" builder
  | Access a ->
    let a_val = codegen_access symbols builder a in
    L.build_load a_val "" builder
  | Assign (a, e) ->
    let acc_var = codegen_access symbols builder a in
    let expr_val = codegen_expr symbols builder e in
    let expr_act_val = normalize_expr expr_val (L.type_of acc_var |> L.element_type) in
    L.build_store expr_act_val acc_var builder |> ignore;
    expr_act_val
  | Addr a -> codegen_access symbols builder a
  | ShortAssign (a, op, e) ->
    let acc = codegen_access symbols builder a in
    let a_val = L.build_load acc "" builder in
    let e_val = codegen_expr symbols builder e in
    let value =
      codegen_binop (L.type_of a_val, L.type_of e_val, op) a_val e_val "" builder
    in
    L.build_store value acc builder |> ignore;
    value
  | SizeOf expr ->
    let t = codegen_expr symbols builder expr |> L.type_of in
    let size =
      match L.classify_type t with
      | L.TypeKind.Pointer -> L.size_of (L.element_type t)
      | _ -> L.size_of t
    in
    (* Truncate the size to an integer since L.size_of returns a i64_type*)
    L.build_trunc size int_lltype "" builder
  | UnaryOp (((PreIncr | PostIncr | PreDecr | PostDecr) as op), expr) ->
    let access_e =
      match expr.node with
      | Access a -> a
      | _ -> raise @@ Codegen_error "Invalid argument for abbreviated operator"
    in
    let e_val = codegen_access symbols builder access_e in
    build_pre_post_op builder op e_val
  | UnaryOp (u, e) ->
    let e_val = codegen_expr symbols builder e in
    (*codegen_bin_op returns a partial function; then we apply the operand*)
    codegen_unop (L.type_of e_val, u) e_val "" builder
  | BinaryOp (b, e1, e2) ->
    let e1_val, e2_val =
      let v1, v2 = codegen_expr symbols builder e1, codegen_expr symbols builder e2 in
      let t1, t2 = L.type_of v1, L.type_of v2 in
      match v1, v2 with
      (* handling null *)
      | e1v, e2v when L.is_undef e1v && not (L.is_undef e2v) ->
        L.const_pointer_null (L.pointer_type (L.element_type t2)), v2
      | e1v, e2v when (not (L.is_undef e1v)) && L.is_undef e2v ->
        v1, L.const_pointer_null (L.pointer_type (L.element_type t1))
      | e1v, e2v -> e1v, e2v
    in
    (*codegen_bin_op returns a partial function; then we apply the two operands*)
    codegen_binop (L.type_of e1_val, L.type_of e2_val, b) e1_val e2_val "" builder
  | Call (func, params) ->
    let actual_f =
      match Symbol_table.lookup func symbols.fun_symbols with
      | Some n -> n
      | None -> raise @@ Codegen_error ("Undefined  function  " ^ func)
    in
    let codegen_call_expr symbols builder p e =
      (*array function parameter is a pointer, therefore it is converted to a pointer to the first element*)
      match e.node with
      | Access a ->
        let a_val = codegen_access symbols builder a in
        let pt = p |> L.type_of |> L.classify_type in
        let a_type = a_val |> L.type_of |> L.element_type |> L.classify_type in
        (match pt, a_type with
         | L.TypeKind.Pointer, L.TypeKind.Array ->
           L.build_gep a_val [| llvm_zero; llvm_zero |] "" builder
         | _, _ -> L.build_load a_val "" builder)
      | _ ->
        let e_val = codegen_expr symbols builder e in
        (match L.type_of e_val |> L.element_type |> L.classify_type with
         | L.TypeKind.Array -> L.build_gep e_val [| llvm_zero; llvm_zero |] "" builder
         | _ -> e_val)
    in
    let fparams = L.params actual_f |> Array.to_list in
    let llvm_params =
      params
      |> List.map2 (codegen_call_expr symbols builder) fparams
      |> List.map2 (fun p e -> normalize_expr e (L.type_of p)) fparams
    in
    L.build_call actual_f (Array.of_list llvm_params) "" builder

and codegen_access symbols builder a =
  match a.node with
  | AccVar id ->
    (match Symbol_table.lookup id symbols.var_symbols with
     | Some v -> v
     | None -> raise @@ Codegen_error ("Variable " ^ id ^ " not defined"))
  | AccDeref expr -> codegen_expr symbols builder expr
  | AccIndex (a, index) ->
    let a_val = codegen_access symbols builder a in
    let ind = codegen_expr symbols builder index in
    let at = a_val |> L.type_of in
    (match at |> L.classify_type with
     | L.TypeKind.Pointer ->
       (match at |> L.element_type |> L.classify_type with
        (*multi-dimensional array*)
        | L.TypeKind.Array -> L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder
        (*single dimension array*)
        | _ ->
          let load_val = Llvm.build_load a_val "" builder in
          Llvm.build_in_bounds_gep load_val [| ind |] "" builder)
     (* non-param array*)
     | _ -> L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder)
  | AccStruct (a, f) ->
    let a_val = codegen_access symbols builder a in
    let sname = L.type_of a_val |> L.element_type |> L.struct_name in
    (match Symbol_table.lookup (Option.get sname) symbols.struct_symbols with
     | Some (_, fields) ->
       let to_index = List.mapi (fun i m -> m, i) fields in
       let field_pos =
         match List.assoc_opt f to_index with
         | Some index -> index
         | None -> raise @@ Codegen_error ("Undefined struct field" ^ f)
       in
       L.build_struct_gep a_val field_pos "" builder
     | None -> raise @@ Codegen_error ("Undefined struct " ^ Option.get sname))
;;

let rec codegen_stmt fdef symbols builder stmt =
  let build_while choose_block condition body =
    let cond_block = L.append_block llcontext "test" fdef in
    let body_block = L.append_block llcontext "while_body" fdef in
    let cont_block = L.append_block llcontext "cont" fdef in
    let cond_builder = L.builder_at_end llcontext cond_block in
    let body_builder = L.builder_at_end llcontext body_block in
    choose_block (cond_block, body_block)
    |> L.build_br
    |> add_terminator builder
    |> ignore;
    let e_val = codegen_expr symbols cond_builder condition in
    L.build_cond_br e_val body_block cont_block cond_builder |> ignore;
    codegen_stmt fdef symbols body_builder body |> ignore;
    L.build_br cond_block |> add_terminator body_builder;
    L.position_at_end cont_block builder
  in
  match stmt.node with
  | If (cond, then_block, else_block) ->
    let blockt = L.append_block llcontext "then" fdef in
    let blockelse = L.append_block llcontext "else" fdef in
    let blockcont = L.append_block llcontext "cont" fdef in
    let then_builder = L.builder_at_end llcontext blockt in
    let else_builder = L.builder_at_end llcontext blockelse in
    let e1 = codegen_expr symbols builder cond in
    L.build_cond_br e1 blockt blockelse builder |> ignore;
    codegen_stmt fdef symbols then_builder then_block |> ignore;
    add_terminator then_builder (L.build_br blockcont);
    codegen_stmt fdef symbols else_builder else_block |> ignore;
    add_terminator else_builder (L.build_br blockcont);
    L.position_at_end blockcont builder
  | Expr e -> codegen_expr symbols builder e |> ignore
  | Block b ->
    let new_scope =
      { symbols with var_symbols = Symbol_table.begin_block symbols.var_symbols }
    in
    List.iter (codegen_stmtordec fdef new_scope builder) b
  | Return e ->
    if Option.is_none e
    then L.build_ret_void |> add_terminator builder
    else (
      let e_val = Option.get e |> codegen_expr symbols builder in
      let v = normalize_expr e_val (L.type_of fdef |> L.pointer_type) in
      v |> L.build_ret |> add_terminator builder)
  | While (e, s) -> build_while fst e s
  | DoWhile (e, s) -> build_while snd e s

and codegen_stmtordec fdef symbols builder st =
  match st.node with
  | DecList l ->
    let codegen_decl symbols builder (t, i, init) =
      let var_v = L.build_alloca (lltype_of_typ symbols.struct_symbols t) i builder in
      let get_init_val e =
        let e_val = codegen_expr symbols builder e in
        if L.type_of e_val
           |> L.element_type
           |> L.classify_type
           = L.TypeKind.Array (*it's a string *)
        then e_val
        else (
          let value = normalize_expr e_val (lltype_of_typ symbols.struct_symbols t) in
          L.build_store value var_v builder |> ignore;
          var_v)
      in
      let actual_value = Option.fold ~none:var_v ~some:get_init_val init in
      Symbol_table.add_entry i actual_value symbols.var_symbols |> ignore;
      ()
    in
    List.iter (codegen_decl symbols builder) l
  | Stmt s -> codegen_stmt fdef symbols builder s
;;

let codegen_param symbols builder (t, id) param =
  let tp =
    match t with
    | TypA (t1, _) -> lltype_of_typ symbols.struct_symbols t1 |> L.pointer_type
    | _ -> lltype_of_typ symbols.struct_symbols t
  in
  let l = L.build_alloca tp "" builder in
  (*store function parameters *)
  Symbol_table.add_entry id l symbols.var_symbols |> ignore;
  L.build_store param l builder |> ignore
;;

let codegen_fundecl symbols func =
  let local_scope =
    { symbols with var_symbols = Symbol_table.begin_block symbols.var_symbols }
  in
  (* find the signature generated in the first scan *)
  let f = Symbol_table.lookup func.fname symbols.fun_symbols |> Option.get in
  let ret_type = lltype_of_typ symbols.struct_symbols func.typ in
  let f_builder = L.entry_block f |> L.builder_at_end llcontext in
  List.iter2
    (codegen_param local_scope f_builder)
    func.formals
    (L.params f |> Array.to_list);
  codegen_stmt f local_scope f_builder func.body |> ignore;
  match func.typ with
  | TypV -> add_terminator f_builder L.build_ret_void
  | _ -> add_terminator f_builder (ret_type |> L.undef |> L.build_ret)
;;

(* used for global variables *)
let codegen_const_op = function
  | t, Neg when t = int_lltype -> L.const_neg
  | t, Neg when t = float_lltype -> L.const_fneg
  | t, Not when t = bool_lltype -> L.const_not
  | _ -> raise @@ Codegen_error "Invald unary operator for global variable"
;;

let rec codegen_global_expr structs t expr =
  match expr.node with
  | ILiteral i -> L.const_int int_lltype i
  | FLiteral f -> L.const_float float_lltype f
  | CLiteral c -> Char.code c |> L.const_int char_lltype
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.const_stringz llcontext s
  | Null -> lltype_of_typ structs t |> L.const_pointer_null
  | UnaryOp (uop, e1) ->
    let a = codegen_global_expr structs t e1 in
    let t1 = L.type_of a in
    codegen_const_op (t1, uop) a
  | BinaryOp (binop, e1, e2) ->
    let a = codegen_global_expr structs t e1 in
    let b = codegen_global_expr structs t e2 in
    let t1, t2 = L.type_of a, L.type_of b in
    codegen_const_binop (t1, t2, binop) a b
  | _ -> raise @@ Codegen_error "Invalid initial expression for global variable"
;;

let codegen_global_variable llmodule symbols (t, i) init =
  let var_init =
    Option.fold
      ~none:(L.undef (lltype_of_typ symbols.struct_symbols t))
        (*undefined default initial value *)
      ~some:(codegen_global_expr symbols.struct_symbols t)
      init
  in
  let var = L.define_global i var_init llmodule in
  Symbol_table.add_entry i var symbols.var_symbols |> ignore
;;

let codegen_topdecl llmodule symbols n =
  match n.node with
  | Fundecl f -> codegen_fundecl symbols f |> ignore
  | VarDecList l ->
    let var_gen llmodule symbols (t, i, init) =
      codegen_global_variable llmodule symbols (t, i) init
    in
    List.iter (var_gen llmodule symbols) l
  | Structdecl _ -> ()
;;

let codegen_fun_sign llmodule symbols node =
  match node.node with
  | Fundecl func ->
    let ret_type = lltype_of_typ symbols.struct_symbols func.typ in
    let formals_types =
      func.formals
      |> List.map fst
      |> List.map (fun t ->
        match t with
        | TypA (t1, _) ->
          (*transform array into a pointer*)
          lltype_of_typ symbols.struct_symbols t1 |> L.pointer_type
        | _ -> lltype_of_typ symbols.struct_symbols t)
    in
    let f_type = L.function_type ret_type (Array.of_list formals_types) in
    let f = L.define_function func.fname f_type llmodule in
    Symbol_table.add_entry func.fname f symbols.fun_symbols |> ignore
  | _ -> ()
;;

let codegen_struct_sign symbols node =
  match node.node with
  | Structdecl s ->
    let named_struct = L.named_struct_type llcontext s.sname in
    Symbol_table.add_entry
      s.sname
      (named_struct, s.fields |> List.map snd)
      symbols.struct_symbols
    |> ignore
  | _ -> ()
;;

let codegen_struct symbols node =
  match node.node with
  | Structdecl s ->
    let named_struct =
      Symbol_table.lookup s.sname symbols.struct_symbols |> Option.get |> fst
    in
    let fields_t =
      s.fields |> List.map (fun (t, _) -> lltype_of_typ symbols.struct_symbols t)
    in
    L.struct_set_body named_struct (Array.of_list fields_t) false |> ignore
  | _ -> ()
;;

let to_llvm_module (Prog topdecls) =
  let module_name = "microc_module" in
  let llmodule = L.create_module llcontext module_name in
  let init_context =
    { fun_symbols = Symbol_table.empty_table ()
    ; var_symbols = Symbol_table.empty_table ()
    ; struct_symbols = Symbol_table.empty_table ()
    }
  in
  (* generate all the structs *)
  List.iter (codegen_struct_sign init_context) topdecls;
  List.iter (codegen_struct init_context) topdecls;
  (* then generate all functions signatures *)
  List.iter (codegen_fun_sign llmodule init_context) topdecls;
  add_rt_support llmodule init_context;
  List.iter (codegen_topdecl llmodule init_context) topdecls;
  llmodule
;;
