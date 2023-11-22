exception Codegen_error of string

open Ast

module L = Llvm

let llcontext = L.global_context ()

let int_type = L.i32_type llcontext

let float_type = L.float_type llcontext

let bool_type = L.i1_type llcontext

let char_type = L.i8_type llcontext

let void_type = L.void_type llcontext

let llvm_one = L.const_int int_type 1

let llvm_zero = L.const_int int_type 0

let llvm_onef = L.const_float float_type 1.0

let llvm_true = L.const_int bool_type 1

let llvm_false = L.const_int bool_type 0

type symbols = {
  fun_symbols : L.llvalue Symbol_table.t;
  var_symbols : L.llvalue Symbol_table.t;
  struct_symbols : (L.lltype * string list) Symbol_table.t;
}

let rec build_llvm_type structs = function
  | TypI -> int_type
  | TypF -> float_type
  | TypC -> char_type
  | TypB -> bool_type
  | TypA (t, Some v) -> L.array_type (build_llvm_type structs t) v
  | TypA (t, None) | TypP t ->
      L.pointer_type (build_llvm_type structs t)
      (*Unsized arrays are directly converted to pointers *)
  | TypV -> void_type
  | TypNull -> L.pointer_type void_type
  | TypS n -> (
      match Symbol_table.lookup n structs with
      | Some (t, _) -> t
      | None -> raise @@ Codegen_error("Undefined structure " ^ n))

(*
  For binary and unary  operators, we check that the operands are of the correct type and return the correct instruction builder.
  We exploit partial functions to return a new function that takes the generate subexpressions and build the final one.
*)
let unop = function
  | t, Neg when t = int_type -> L.build_neg
  | t, Neg when t = float_type -> L.build_fneg
  | t, Not when t = bool_type -> L.build_not
  | t, BNot when t = int_type -> L.build_not
  | _ -> raise @@ Codegen_error "Invald unary operator for global variable"

let bin_op = function
  | t1, t2, Add when t1 = int_type && t2 = int_type -> L.build_add
  | t1, t2, Sub when t1 = int_type && t2 = int_type -> L.build_sub
  | t1, t2, Div when t1 = int_type && t2 = int_type -> L.build_sdiv
  | t1, t2, Mult when t1 = int_type && t2 = int_type -> L.build_mul
  | t1, t2, Mod when t1 = int_type && t2 = int_type -> L.build_srem
  | t1, t2, Less when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_type && t2 = int_type ->
      L.build_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Ne
  | t1, t2, BAnd when t1 = int_type && t2 = int_type -> L.build_and
  | t1, t2, BOr when t1 = int_type && t2 = int_type -> L.build_or
  | t1, t2, BXor when t1 = int_type && t2 = int_type -> L.build_xor
  | t1, t2, LShift when t1 = int_type && t2 = int_type -> L.build_shl
  | t1, t2, RShift when t1 = int_type && t2 = int_type -> L.build_lshr
  | t1, t2, Add when t1 = float_type && t2 = float_type -> L.build_fadd
  | t1, t2, Sub when t1 = float_type && t2 = float_type -> L.build_fsub
  | t1, t2, Div when t1 = float_type && t2 = float_type -> L.build_fdiv
  | t1, t2, Mult when t1 = float_type && t2 = float_type -> L.build_fmul
  | t1, t2, Less when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.One
  | t1, t2, Equal
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Ne
  | t1, t2, Equal when t1 = char_type && t2 = char_type ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = char_type && t2 = char_type -> L.build_icmp L.Icmp.Ne
  | t1, t2, And when t1 = bool_type && t2 = bool_type -> L.build_and
  | t1, t2, Or when t1 = bool_type && t2 = bool_type -> L.build_or
  | t1, t2, Equal when t1 = bool_type && t2 = bool_type ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_type && t2 = bool_type -> L.build_icmp L.Icmp.Ne
  | _ -> raise @@ Codegen_error "Type mismatch between operands"

(* Uses const instructions when dealing with global expressions *)
let const_op = function
  | t, Neg when t = int_type -> L.const_neg
  | t, Neg when t = float_type -> L.const_fneg
  | t, Not when t = bool_type -> L.const_not
  | _ -> raise @@ Codegen_error "Invald unary operator for global variable"

let const_bin_op = function
  | t1, t2, Add when t1 = int_type && t2 = int_type -> L.const_add
  | t1, t2, Sub when t1 = int_type && t2 = int_type -> L.const_sub
  | t1, t2, Div when t1 = int_type && t2 = int_type -> L.const_sdiv
  | t1, t2, Mult when t1 = int_type && t2 = int_type -> L.const_mul
  | t1, t2, Mod when t1 = int_type && t2 = int_type -> L.const_srem
  | t1, t2, Less when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_type && t2 = int_type ->
      L.const_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Ne
  | t1, t2, Add when t1 = float_type && t2 = float_type -> L.const_fadd
  | t1, t2, Sub when t1 = float_type && t2 = float_type -> L.const_fsub
  | t1, t2, Div when t1 = float_type && t2 = float_type -> L.const_fdiv
  | t1, t2, Mult when t1 = float_type && t2 = float_type -> L.const_fmul
  | t1, t2, Less when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.One
  | t1, t2, And when t1 = bool_type && t2 = bool_type -> L.const_and
  | t1, t2, Or when t1 = bool_type && t2 = bool_type -> L.const_or
  | t1, t2, Equal when t1 = bool_type && t2 = bool_type ->
      L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_type && t2 = bool_type -> L.const_icmp L.Icmp.Ne
  | _ ->
      raise @@ Codegen_error "Mismatch between type of global variable and initial value"

(* Applies the increment or decrement and return the value before the operation if it's a++ (a--) or the value returned by the expression
  if it is ++a (--a)
 *)
let build_unary_incr_or_decr builder op value =
  let inc_op = function
    | (PreInc | PostInc), t when t = int_type -> L.build_add llvm_one
    | (PreInc | PostInc), t when t = float_type -> L.build_fadd llvm_one
    | (PreDec | PostDec), t when t = int_type -> Fun.flip L.build_sub llvm_one
    | (PreDec | PostDec), t when t = float_type ->
        Fun.flip L.build_fsub llvm_onef
        (*Flip needed so a-- becomes a - 1 and not 1 - a *)
    | _ ->
        raise  @@ Codegen_error ("Invalid type for operator " ^ show_uop op)
  in
  let apply_op = inc_op (op, L.element_type (L.type_of value)) in
  let before = L.build_load value "" builder in
  let after = apply_op before "" builder in
  L.build_store after value builder |> ignore;
  if op = PreInc || op = PreDec then after else before

let normalize_expr e t = if L.is_undef e then L.const_pointer_null t else e (*helper function to handle situations where null appears *)

let rec codegen_expr scope builder e =
  match e.node with
  | ILiteral i -> L.const_int int_type i
  | FLiteral f -> L.const_float float_type f
  | CLiteral c -> L.const_int char_type (Char.code c)
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.build_global_string (s ^ "\000") "" builder
  | Null -> L.undef (int_type |> L.pointer_type)
  | Addr a -> codegen_access scope builder a
  | Access a ->
      (*access always returns an address so we load the value *)
      let a_val = codegen_access scope builder a in
      L.build_load a_val "" builder
  | Assign (a, e) ->
      let acc_var = codegen_access scope builder a in
      let expr_val = codegen_expr scope builder e in
      let expr_act_val =
        normalize_expr expr_val (L.type_of acc_var |> L.element_type)
      in
      L.build_store expr_act_val acc_var builder |> ignore;
      expr_act_val
  | ShortAssign (a, op, e) ->
      let acc = codegen_access scope builder a in
      let a_val = L.build_load acc "" builder in
      let e_val = codegen_expr scope builder e in
      let value =
        bin_op (L.type_of a_val, L.type_of e_val, op) a_val e_val "" builder
      in
      L.build_store value acc builder |> ignore;
      value
  | UnaryOp (((PreInc | PostInc | PreDec | PostDec) as op), e) ->
      let access_e =
        match e.node with
        | Access a ->
            a
            (*we defer loading the value, to correctly use the abbreviated operator *)
        | _ -> assert false (*Abbreviated increments are only defined with access expression *)
      in
      let e_val = codegen_access scope builder access_e in
      build_unary_incr_or_decr builder op e_val
  | UnaryOp (u, e) ->
      let e_val = codegen_expr scope builder e in
      (*gets the correct binary expression and builds the actual instruction *)
      unop (L.type_of e_val, u) e_val "" builder
  | BinaryOp (b, e1, e2) ->
      let e1_val, e2_val =
        let v1, v2 =
          (codegen_expr scope builder e1, codegen_expr scope builder e2)
        in
        let t1, t2 = (L.type_of v1, L.type_of v2) in
        match (v1, v2) with
        (* handling null *)
        | e1v, e2v when L.is_undef e1v && not (L.is_undef e2v) ->
            (L.const_pointer_null (L.pointer_type (L.element_type t2)), v2)
        | e1v, e2v when (not (L.is_undef e1v)) && L.is_undef e2v ->
            (v1, L.const_pointer_null (L.pointer_type (L.element_type t1)))
        | e1v, e2v -> (e1v, e2v)
      in
      (*gets the correct binary expression and builds the actual instruction *)
      bin_op (L.type_of e1_val, L.type_of e2_val, b) e1_val e2_val "" builder
  | Call (f, params) ->
      let actual_f =
        match Symbol_table.lookup f scope.fun_symbols with
        | Some n -> n
        | None -> raise @@ Codegen_error ("Undefined  function  " ^ f)
      in

      let codegen_call_expr scope builder p e =
        (*Since array function parameters are all pointers, converts arrays  to pointers to the first element *)
        match e.node with
        | Access a -> (
            let a_val = codegen_access scope builder a in
            let pt = p |> L.type_of |> L.classify_type in
            match pt with
            | L.TypeKind.Pointer ->
                if
                  a_val |> L.type_of |> L.element_type |> L.classify_type
                  = L.TypeKind.Array
                then L.build_gep a_val [| llvm_zero; llvm_zero |] "" builder
                else if
                  a_val |> L.type_of |> L.element_type |> L.classify_type
                  = L.TypeKind.Pointer
                then L.build_load a_val "" builder
                else a_val
            | _ -> L.build_load a_val "" builder)
        | _ ->
            let e_val = codegen_expr scope builder e in
            if
              L.type_of e_val |> L.element_type |> L.classify_type
              = L.TypeKind.Array
            then
              (* string literal *)
              L.build_gep e_val [| llvm_zero; llvm_zero |] "" builder
            else e_val
      in
      let fparams = L.params actual_f |> Array.to_list in
      let llvm_params =
        params
        |> List.map2 (codegen_call_expr scope builder) fparams 
        |> List.map2 (fun p e -> normalize_expr e (L.type_of p)) fparams
      in
      L.build_call actual_f (Array.of_list llvm_params) "" builder

and codegen_access scope builder a =
  match a.node with
  | AccVar i -> (
      match Symbol_table.lookup i scope.var_symbols with
      | Some v -> v
      | None -> raise @@ Codegen_error ("Variable " ^ i ^ " not defined"))
  | AccDeref e -> codegen_expr scope builder e
  | AccIndex (a, i) -> (
      (*Because unsized arrays are pointers, index access can be used with pointers, and
      we must decide the correct gep instruction to build. *)
      let a_val = codegen_access scope builder a in
      let ind = codegen_expr scope builder i in
      let at = a_val |> L.type_of in
      match at |> L.classify_type with
      | L.TypeKind.Pointer -> (
          match at |> L.element_type |> L.classify_type with
          | L.TypeKind.Array ->
              L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder
          | _ ->
              let load_val = Llvm.build_load a_val "" builder in
              Llvm.build_in_bounds_gep load_val [| ind |] "" builder)
      | _ -> L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder)
  | AccField (a, f) -> (
      let a_val = codegen_access scope builder a in
      let sname = L.type_of a_val |> L.element_type |> L.struct_name in

      match Symbol_table.lookup (Option.get sname) scope.struct_symbols with
      | Some (_, fields) ->
          (*checks that the given field exists and gets its index *)
          let to_index = List.mapi (fun i m -> (m, i)) fields in
          let field_pos =
            match List.assoc_opt f to_index with
            | Some index -> index
            | None -> raise @@ Codegen_error ("Undefined struct member " ^ f)
          in
          L.build_struct_gep a_val field_pos "" builder
      | None ->
          raise @@ Codegen_error ("Undefined struct " ^ Option.get sname))

let add_terminator builder after =
  let terminator = L.block_terminator (L.insertion_block builder) in
  if Option.is_none terminator then after builder |> ignore else ()

let rec codegen_stmt fdef scope builder stmt =
  let build_while choose_block condition body =
    (*helper functions that builds while loops and doWhile.
      Choose_block gets the first block that must be executed between the loop body and the loop condition check *)
    let cond_block = L.append_block llcontext "test" fdef in
    let body_block = L.append_block llcontext "while_body" fdef in
    let cont_block = L.append_block llcontext "cont" fdef in
    let cond_builder = L.builder_at_end llcontext cond_block in
    let body_builder = L.builder_at_end llcontext body_block in
    choose_block (cond_block, body_block)
    |> L.build_br |> add_terminator builder |> ignore;
    let e_val = codegen_expr scope cond_builder condition in
    L.build_cond_br e_val body_block cont_block cond_builder |> ignore;
    codegen_stmt fdef scope body_builder body |> ignore;
    L.build_br cond_block |> add_terminator body_builder;
    L.position_at_end cont_block builder
  in

  match stmt.node with
  | If (e, st1, st2) ->
      let blockt = L.append_block llcontext "then" fdef in
      let blockelse = L.append_block llcontext "else" fdef in
      let blockcont = L.append_block llcontext "cont" fdef in
      let then_builder = L.builder_at_end llcontext blockt in
      let else_builder = L.builder_at_end llcontext blockelse in
      let e1 = codegen_expr scope builder e in
      L.build_cond_br e1 blockt blockelse builder |> ignore;

      codegen_stmt fdef scope then_builder st1 |> ignore;
      add_terminator then_builder (L.build_br blockcont);

      codegen_stmt fdef scope else_builder st2 |> ignore;
      add_terminator else_builder (L.build_br blockcont);
      L.position_at_end blockcont builder;
  | Expr e ->
      codegen_expr scope builder e |> ignore;
  | Block b ->
      let new_scope =
        { scope with var_symbols = Symbol_table.begin_block scope.var_symbols }
      in
      (*
      As soon as codegen_stmt returns false, stops generating instructions.
      Needed to handle instructions after an always reached return statement
      *)
      List.iter( codegen_stmtordec fdef new_scope builder) b
  | Return e ->
      (if Option.is_none e then L.build_ret_void |> add_terminator builder
      else
        let e_val = Option.get e |> codegen_expr scope builder in
        let v = normalize_expr e_val (L.type_of fdef |> L.pointer_type) in

        v |> L.build_ret |> add_terminator builder);
  | While (e, s) ->
      build_while fst e s; (*condition is the first instruction *)
  | DoWhile (e, s) ->
      build_while snd e s; (*body is the first instruction *)

and codegen_stmtordec fdef scope builder st =
  match st.node with
  | DecList l ->
        let var_gen scope builder (t, i, init) =
        let var_v =
          L.build_alloca (build_llvm_type scope.struct_symbols t) i builder
        in
        let get_init_val e =
          let e_val = codegen_expr scope builder e in
          if
            L.type_of e_val |> L.element_type |> L.classify_type
            = L.TypeKind.Array (*string literal *)
          then e_val
          else
            let value =
              normalize_expr e_val (build_llvm_type scope.struct_symbols t)
            in

            L.build_store value var_v builder |> ignore;
            var_v
        in
        let actual_value = Option.fold ~none:var_v ~some:get_init_val init in
        Symbol_table.add_entry i actual_value scope.var_symbols |> ignore;
        ()
      in
      List.iter (var_gen scope builder) l;
  | Stmt s -> codegen_stmt fdef scope builder s

(*
  - Generate parameters
  - Generate function body
  - Add terminator when  no return is present
   *)
let codegen_func  scope func =
  let local_scope =
    {
      scope with
      var_symbols = Symbol_table.begin_block scope.var_symbols;
    }
  in
  let f = Symbol_table.lookup func.fname scope.fun_symbols |> Option.get in
  let ret_type = build_llvm_type scope.struct_symbols func.typ in
  let f_builder = L.entry_block f |> L.builder_at_end llcontext in
  let build_param scope builder (t, i) p =
    let tp =
      match t with
      | TypA (t1, _) ->
          build_llvm_type scope.struct_symbols t1 |> L.pointer_type
      | _ -> build_llvm_type scope.struct_symbols t
    in
    let l = L.build_alloca tp "" builder in (*store function parameters *)
    Symbol_table.add_entry i l scope.var_symbols |> ignore;
    L.build_store p l builder |> ignore
  in

  List.iter2
    (build_param local_scope f_builder)
    func.formals
    (L.params f |> Array.to_list);
  codegen_stmt f local_scope f_builder func.body |> ignore;
  match func.typ with
  | TypV -> add_terminator f_builder L.build_ret_void
  | _ -> add_terminator f_builder (ret_type |> L.undef |> L.build_ret) (*when no return is present, the function returns an undefined value *)

let rec codegen_global_expr structs t e =
  match e.node with
  | ILiteral i -> L.const_int int_type i
  | FLiteral f -> L.const_float float_type f
  | CLiteral c -> Char.code c |> L.const_int char_type
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.const_stringz llcontext s
  | Null -> build_llvm_type structs t |> L.const_pointer_null
  | UnaryOp (uop, e1) ->
      let a = codegen_global_expr structs t e1 in
      let t1 = L.type_of a in
      const_op (t1, uop) a
  | BinaryOp (binop, e1, e2) ->
      let a = codegen_global_expr structs t e1 in
      let b = codegen_global_expr structs t e2 in
      let t1, t2 = (L.type_of a, L.type_of b) in
      const_bin_op (t1, t2, binop) a b
  | _ ->
      raise @@ Codegen_error ( "Invalid initial expression for global variable")

let codegen_global_variable llmodule scope (t, i) init =
  let var_init =
    Option.fold
      ~none:(L.undef (build_llvm_type scope.struct_symbols t)) (*undefined default initial value *)
      ~some:(codegen_global_expr scope.struct_symbols t)
      init
  in
  let var = L.define_global i var_init llmodule in
  Symbol_table.add_entry i var scope.var_symbols |> ignore

let codegen_topdecl llmodule scope n =
  match n.node with
  | Fundecl f -> codegen_func  scope f |> ignore
  | VarDecList l ->
      let var_gen llmodule scope (t, i, init) =
        codegen_global_variable llmodule scope (t, i) init
      in
    List.iter (var_gen llmodule scope) l
  | Structdecl s ->
      let named_s = L.named_struct_type llcontext s.sname in
      Symbol_table.add_entry s.sname
        (named_s, s.fields |> List.map snd)
        scope.struct_symbols
      |> ignore;
      let fields_t =
        s.fields
        |> List.map (fun (t, _) -> build_llvm_type scope.struct_symbols t)
      in
      L.struct_set_body named_s (Array.of_list fields_t) false

let add_rt_support llmodule scope =
(*declares function prototypes *)
  let params_to_array params =
    params |> List.map fst
    |> List.map (build_llvm_type scope.struct_symbols)
    |> Array.of_list
  in
  let fun_type f =
    L.function_type
      (build_llvm_type scope.struct_symbols f.typ)
      (params_to_array f.formals)
  in
  Rt_support.rt_functions
  |> List.map (fun (n, (_, f)) -> (n, fun_type f))
  |> List.iter (fun (n, t) ->
         Symbol_table.add_entry n
           (L.declare_function n t llmodule)
           scope.fun_symbols
         |> ignore)

let add_function llmodule scope node =
  match node.node with
  | Fundecl func ->
  let ret_type = build_llvm_type scope.struct_symbols func.typ in
  let formals_types =
    func.formals |> List.map fst
    |> List.map (fun t ->
           match t with
           | TypA (t1, _) -> (*all arrays become pointers *)
               build_llvm_type scope.struct_symbols t1 |> L.pointer_type
           | _ -> build_llvm_type scope.struct_symbols t)
  in
  let f_type = L.function_type ret_type (Array.of_list formals_types) in
  let f = L.define_function func.fname f_type llmodule in
    {
      scope with
      fun_symbols = Symbol_table.add_entry func.fname f scope.fun_symbols;
    }
  | _ -> scope

let to_llvm_module (Prog topdecls) =
  let module_name = "microc_module" in
  let llmodule = L.create_module llcontext module_name in
  let init_scope =
    {
      fun_symbols = Symbol_table.empty_table ();
      var_symbols = Symbol_table.empty_table ();
      struct_symbols = Symbol_table.empty_table ();
    }
  in
  let function_scope = List.fold_left (add_function llmodule) init_scope topdecls in
  add_rt_support llmodule function_scope;
  List.iter (codegen_topdecl llmodule function_scope) topdecls;
  llmodule
