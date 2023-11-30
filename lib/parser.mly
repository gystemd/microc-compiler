%{
        open Ast


        (* build a new annotated node*)
        let ann_node nd loc = {loc = (Location.to_code_position loc); node = nd; id=0}

        let build_for_init e loc =
              match e with
              | Some(x) -> ann_node(Stmt(ann_node(Expr(x)) loc)) loc
              | None -> ann_node(Stmt(ann_node(Block([])) loc)) loc


        let build_for_condition e loc =
              match e with
              | Some(x) -> x
              | None -> ann_node(BLiteral(true)) loc

        let build_for_iter e loc =
              match e with
              | Some(x) -> ann_node(Stmt(ann_node(Expr(x)) loc)) loc
              | None -> ann_node(Stmt( ann_node(Block([])) loc)) loc

%}

/* Tokens declarations */
%token IF RETURN ELSE FOR WHILE DO INT CHAR VOID NULL BOOL FLOAT STRUCT
%token PLUS MINUS TIMES DIVIDE MOD DOT
%token SIZEOF
%token AND OR EQ NEQ NOT GT LT GEQ LEQ BOR BNOT BXOR LSHIFT RSHIFT
%token ADDRESS ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token INCREMENT DECREMENT
%token COMMA SEMI
%token SHORTADD SHORTDIV SHORTMIN SHORTMUL SHORTMOD
%token <string>ID
%token <int>INTEGER
%token <float> FLOATLIT
%token <char>CHARLIT
%token <string>STRING
%token TRUE FALSE
%token EOF

/* Precedence and associativity specification */
%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN SHORTADD SHORTDIV SHORTMIN SHORTMUL SHORTMOD
%left BXOR BNOT LSHIFT RSHIFT BOR
%left OR
%left AND
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc NOT ADDRESS
%left DOT
%nonassoc LBRACKET


/* Starting symbol */

%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */

program:
  | p = list(topdec) EOF     {Prog p}
;

topdec:
| vl = varlist SEMI {ann_node (VarDecList(vl)) $loc}
| t = typ i = ID LPAREN fs=separated_list(COMMA, genericparam) RPAREN b=block
  {ann_node (Fundecl({typ=t; fname=i; formals=fs; body=b})) $loc}
| STRUCT i = ID LBRACE l=list(terminated(genericparam,SEMI)) RBRACE SEMI
  {ann_node (Structdecl({sname=i; fields=l})) $loc}
;

typ:
  | INT {TypI}
  | FLOAT {TypF}
  | CHAR {TypC}
  | BOOL {TypB}
  | VOID {TypV}
  | STRUCT i = ID {TypS(i)}
;

genericparam:
| t = typ v=vardesc {((fst v) t, snd v)}
varlist:
/* Unwrap the type declarations, applying the final function to t */
| t = typ v = separated_list(COMMA, vardecl) {List.map (fun (a,b,c) -> (a t, b, c)) v}
;

vardecl:
| v = vardesc e = option(preceded(ASSIGN,expr)) {((fst v) , snd v, e)}
;
vardesc:
/* The 'vardesc' rule does not have information about the variable type. Hence, we generate a function
  that accepts the type as an input parameter. This function will wrap the necessary layers (Pointers and Arrays)
  around the input to produce the correct final type, e.g TypP(TypP(TypP(TypI))). */
| i = ID  {((fun t -> t), i)} 
| TIMES v = vardesc %prec ADDRESS {((fun t->fst v (TypP(t))) , snd v )}
| LPAREN v = vardesc RPAREN {v}
| v = vardesc LBRACKET n = option(INTEGER) RBRACKET {((fun t -> fst v (TypA(t,n))), snd v) }
;

block:
| LBRACE c=list(stmtordec) RBRACE { ann_node (Block(c)) $loc}
;

stmtordec:
| s = statement {ann_node (Stmt(s)) $loc}
| v = varlist SEMI {ann_node (DecList(v)) $loc}
;
statement: 
| RETURN e = option(expr) SEMI {ann_node (Return(e)) $loc}
| e = expr SEMI {ann_node (Expr(e)) $loc}
| b = block {b}
| DO s = statement WHILE LPAREN e = expr RPAREN SEMI {ann_node (DoWhile(e, s)) $loc}
| WHILE LPAREN e = expr RPAREN s=statement {ann_node (While(e, s)) $loc}
| FOR LPAREN init = option(expr) SEMI ext_cond = option(expr) SEMI incr=option(expr) RPAREN s=statement
/* Transform the foor loop into equivalent while loop*/
{

    ann_node (Block([build_for_init init $loc;
              ann_node (Stmt(
                  ann_node (While(build_for_condition ext_cond $loc,
                    ann_node (Block([ann_node(Stmt(s)) $loc;build_for_iter incr $loc]))
                    $loc))
                  $loc)) 
              $loc;
              ])) 
    $loc
}
| IF LPAREN cond=expr RPAREN s=statement e=elseblock
  {ann_node (If(cond,s,e)) $loc}
;

elseblock:
  | %prec NOELSE{ann_node (Block([])) $loc} /* precedence to if without else */
  | ELSE st=statement {st}
;

expr:
| r = rexpr {r}
| l = lexpr {ann_node (Access(l)) $loc}
;

lexpr:
| i = ID {ann_node (AccVar(i)) $loc}
| LPAREN l = lexpr RPAREN {l}
| TIMES ADDRESS l=lexpr {ann_node (AccDeref(ann_node (Addr(l)) $loc)) $loc}
| TIMES l = lexpr {ann_node (AccDeref(ann_node (Access(l)) $loc)) $loc}
| l=lexpr LBRACKET e = expr RBRACKET { ann_node (AccIndex(l,e)) $loc}
| l = lexpr DOT f=ID {ann_node (AccStruct(l,f)) $loc}
;

rexpr:
| a = aexpr {a}
| SIZEOF LPAREN e=expr RPAREN{ann_node (SizeOf(e)) $loc}
| i = ID LPAREN p=separated_list(COMMA,expr) RPAREN
  {ann_node (Call(i,p)) $loc}
| l = lexpr ASSIGN e = expr {ann_node(Assign(l, e)) $loc}
| u=unaryOp e=expr {ann_node (UnaryOp(u, e)) $loc}
| e=expr b=binOp e2=expr  {ann_node (BinaryOp(b,e,e2)) $loc}
| l=lexpr s=shortOp e = expr {ann_node (ShortAssign(l,s, e )) $loc}
| INCREMENT l = lexpr {ann_node(UnaryOp(PreIncr,ann_node(Access(l)) $loc )) $loc}
| DECREMENT l = lexpr {ann_node (UnaryOp(PreDecr,ann_node(Access(l)) $loc )) $loc}
| l  = lexpr INCREMENT {ann_node (UnaryOp(PostIncr,ann_node(Access(l)) $loc )) $loc}
| l  = lexpr DECREMENT {ann_node(UnaryOp(PostDecr,ann_node(Access(l)) $loc )) $loc}
;

(*inline BinOp to remove shift-reduce conflicts*)
%inline binOp:
| PLUS  {Add}
| MINUS   {Sub}
| TIMES   {Mult}
| MOD   {Mod}
| DIVIDE  {Div}
| AND   {And}
| OR  {Or}
| LT {Less}
| GT {Greater}
| LEQ {Leq}
| GEQ {Geq}
| EQ {Equal}
| NEQ {Neq}
| BOR {BOr}
| ADDRESS {BAnd}
| BXOR {BXor}
| LSHIFT {LShift}
| RSHIFT {RShift}
;

%inline unaryOp:
| NOT {Not}
| MINUS {Neg}
| BNOT {BNot}

%inline shortOp:
|SHORTADD {Add}
|SHORTDIV {Div}
|SHORTMIN {Sub}
|SHORTMUL {Mult}
|SHORTMOD {Mod}
;
aexpr:
| i=INTEGER
  {ann_node(ILiteral(i)) $loc}
| c=CHARLIT
  {ann_node(CLiteral(c)) $loc}
| f=FLOATLIT
  {ann_node(FLiteral(f)) $loc}
| s=STRING
  {ann_node(String(s)) $loc}
| TRUE
  {ann_node(BLiteral(true)) $loc}
| FALSE
  {ann_node(BLiteral(false)) $loc}
| NULL
  {ann_node(Null) $loc}
| LPAREN r=rexpr RPAREN
  {r}
| ADDRESS l=lexpr
  {ann_node(Addr(l)) $loc}


