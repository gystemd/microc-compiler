type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Comma
[@@deriving show]

type uop = Neg | Not | PreInc | PreDec | PostInc | PostDec   [@@deriving show]



type identifier = string [@@deriving show]


let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

type 'a annotated_node = { loc : Location.code_pos; [@opaque] node : 'a; id : int }
[@@deriving show]

type typ =
  | TypI (* Type int                    *)
  | TypB (* Type bool                   *)
  | TypC (* Type char                   *)
  | TypF
  | TypA of typ * int option (* Array type                  *)
  | TypP of typ (* Pointer type                *)
  | TypV (* Type void                   *)
  | TypS of identifier
  | TypNull (*bottom type for null value*)
[@@deriving show]

and expr = expr_node annotated_node

and expr_node =
  | Access of access (* x    or  *p    or  a[e]     *)
  | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
  | ShortAssign of access * binop * expr
  | Addr of access (* &x   or  &*p   or  &a[e]    *)
  | ILiteral of int (* Integer literal             *)
  | FLiteral of float 
  | CLiteral of char (* Char literal                *)
  | BLiteral of bool (* Bool literal                *)
  | String of string
  | Null
  | UnaryOp of uop * expr (* Unary primitive operator    *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator   *)
  | Call of identifier * expr list (* Function call f(...)        *)
[@@deriving show]

and access = access_node annotated_node

and access_node =
  | AccVar of identifier (* Variable access        x    *)
  | AccDeref of expr (* Pointer dereferencing  *p   *)
  | AccIndex of access * expr (* Array indexing         a[e] *)
  | AccField of access * identifier
[@@deriving show]

and stmt = stmt_node annotated_node

and stmt_node =
  | If of expr * stmt * stmt (* Conditional                 *)
  | While of expr * stmt (* While loop                  *)
  | DoWhile of expr*stmt
  | Expr of expr (* Expression statement   e;   *)
  | Return of expr option (* Return statement            *)
  | Block of stmtordec list (* Block: grouping and scope   *)
[@@deriving show]

and stmtordec = stmtordec_node annotated_node

and stmtordec_node =
  | Dec of typ * identifier * expr option (* Local variable declaration  *)
  | Stmt of stmt (* A statement                 *)
[@@deriving show]

and function_block_node = FunctionBlock of stmtordec list [@@deriving show]

and function_block = function_block_node annotated_node
type fun_decl = {
  typ : typ;
  fname : string;
  formals : (typ * identifier) list;
  body : function_block;
}
[@@deriving show]

type struct_decl = {
  sname : string;
  fields :  (typ*identifier) list;
}
[@@deriving show]
type topdecl = topdecl_node annotated_node

and topdecl_node = Fundecl of fun_decl | Vardec of typ * identifier * expr option | Structdecl of struct_decl
[@@deriving show]

type program = Prog of topdecl list [@@deriving show]