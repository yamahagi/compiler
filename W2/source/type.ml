type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec print_type_list t =
    match t with
    | [] -> print_string " "
    | x :: xs -> print_type x; print_string ", "; print_type_list xs

and print_type t =
    match t with
    | Unit -> print_string "UNIT"
    | Bool -> print_string "BOOL"
    | Int  -> print_string "INT"
    | Float-> print_string "FLOAT"
    | Fun (a, b) -> print_string "FUN "; print_type_list a; print_string "-> "; print_type b
    | Tuple l -> print_string "TUPLE "; print_type_list l
    | Array a -> print_string "ARRAY "; print_type a
    | Var x -> (match !x with
               | Some a -> print_string "VAR "; print_type a
               | None -> print_string "VAR NONE ")
