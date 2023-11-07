exception DuplicateEntry of Ast.identifier

type 'a t = ((string, 'a) Hashtbl.t) list

let empty_table () = [Hashtbl.create 0]

(* create a new scope by adding a new hash table to the front of the
   list of hash tables. *)
let begin_block table = Hashtbl.create 0 :: table


let end_block table = List.tl table

(*  recursively lookup the variable starting from the current scope; *)
let rec lookup symbol table =
    match table with
        | []       -> None
        | (hd::tl) ->
            match Hashtbl.find_opt hd symbol with
                | None    -> lookup symbol tl
                | Some(v) -> Some(v)

(* add the variable information in the current scope*)
let add_entry symbol info table =
    if lookup symbol table <> None then
        raise (DuplicateEntry symbol);
    let current = List.hd table in
    match Hashtbl.find_opt current symbol with
        | None    -> Hashtbl.add current symbol info; table
        | Some(_) -> raise (DuplicateEntry symbol)
