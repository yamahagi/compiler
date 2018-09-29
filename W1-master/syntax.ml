type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_list_pair l =
    match l with
    | [] -> print_string " "
    | (a, b) :: xs -> (print_string a;
                       print_string " ";
                       Type.print_type b;
                       print_string ", ";
                       print_list_pair xs)
and print_list l =
    match l with
    | [] -> print_string " "
    | x :: xs -> (print_expr x;
                  print_string ", ";
                  print_list xs)

and print_expr e =
    match e with
    | Unit  -> print_string "UNIT"
    | Bool b -> (print_string "BOOL ";
                      print_string (string_of_bool b))
    | Int i -> (print_string "INT ";
                     print_int i)
    | Float f -> (print_string "FLOAT ";
                       print_string (string_of_float f))
    | Not n -> (print_string "NOT (";
                print_expr n;
                print_string ")")
    | Neg n -> (print_string "NEG (";
                print_expr n;
                print_string ")")
    | Add (a, b) -> (print_string "ADD (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | Sub (a, b) -> (print_string "SUB (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | FNeg n -> (print_string "FNEG (";
                print_expr n;
                print_string ")")
    | FAdd(a, b) -> (print_string "FADD (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | FSub(a, b) -> (print_string "FSUB (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | FMul(a, b) -> (print_string "FMUL (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | FDiv(a, b) -> (print_string "FDIV (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | Eq (a, b) -> (print_string "EQ (";
                    print_expr a;
                    print_string ", ";
                    print_expr b;
                    print_string ")")
    | LE (a, b) -> (print_string "LE (";
                    print_expr a;
                    print_string ", ";
                    print_expr b;
                    print_string ")")
    | If (a, b, c) ->
            (print_string "IF (";
                       print_expr a;
                       print_string ", ";
                       print_expr b;
                       print_string ")")
    | Let ((a, b), c, d) ->
            (print_string "LET (";
             print_string a;
             print_string ", ";
             Type.print_type b;
             print_string ", ";
             print_expr c;
             print_string ", ";
             print_expr d;
             print_string ")")
    | Var v -> (print_string "VAR ";
                     print_string v)
    | LetRec (f, t) -> (print_string "LETREC (";
                        print_fundef f;
                        print_string  " ";
                        print_expr t;
                        print_string ")")
    | App (a, b) -> (print_string "APP (";
                     print_expr a;
                     print_string ", ";
                     print_list b;
                     print_string ")")
    | Tuple a -> (print_string "TUPLE (";
                  print_list a;
                  print_string ")")
    | LetTuple (l, a, b) -> (print_string "LETTUPLE (";
                             print_string ", ";
                             print_list_pair l;
                             print_string ", ";
                             print_expr a;
                             print_string ", ";
                             print_expr b;
                             print_string ")")
    | Array (a, b) -> (print_string "ARRAY (";
                       print_expr a;
                       print_string ", ";
                       print_expr b;
                       print_string ")")
    | Get (a, b) -> (print_string "GET (";
                     print_expr a;
                     print_string ", ";
                     print_expr b;
                     print_string ")")
    | Put (a, b, c) -> (print_string "PUT (";
                        print_expr a;
                        print_string ", ";
                        print_expr b;
                        print_string ", ";
                        print_expr c;
                        print_string ")")
and print_fundef f =
    let ((a, b), c) = f.name in (print_string a; print_string " "; Type.print_type b);
    print_string " ";
    print_list_pair f.args;
    print_expr f.body
