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

(*

parserの結果を出力

*)

let rec print_list_pair l =
    match l with
    | [] -> print_string " "
    | (a, b) :: xs -> (print_string a;
			print_string " ";
                       Type.print_type b;
                       print_list_pair xs)
and print_list l =
    match l with
    | [] -> print_string ""
    | x :: xs -> (print_expr x;
                  print_list xs)

and print_expr e =
    match e with
    | Unit  -> print_string "UNIT";
		print_newline()
    | Bool b -> (print_string "BOOL ";
                print_string (string_of_bool b);
		print_newline())
    | Int i -> (print_string "INT ";
                print_int i;
		print_newline()
		)
    | Float f -> (print_string "FLOAT ";
                  print_string (string_of_float f);
		  print_newline())
    | Not n -> (print_string "NOT ";
                print_expr n;
		)
    | Neg n -> (print_string "NEG ";
                print_expr n;
		)
    | Add (a, b) -> (print_string "ADD ";
		     print_newline();
                     print_expr a;
                     print_expr b;
			)
    | Sub (a, b) -> (print_string "SUB ";
		     print_newline();
                     print_expr a;
                     print_expr b;
		 	)
    | FNeg n -> (print_string "FNEG ";
                print_expr n;
		)
    | FAdd(a, b) -> (print_string "FADD ";
		     print_newline();
                     print_expr a;
                     print_expr b;
			)
    | FSub(a, b) -> (print_string "FSUB ";
		     print_newline();
                     print_expr a;
                     print_expr b;
			)
    | FMul(a, b) -> (print_string "FMUL ";
		     print_newline();
                     print_expr a;
                     print_expr b;
			)
    | FDiv(a, b) -> (print_string "FDIV ";
		     print_newline();
                     print_expr a;
                     print_expr b;
			)
    | Eq (a, b) -> (print_string "EQ ";
		     print_newline();
                    print_expr a;
                    print_expr b;
			)
    | LE (a, b) -> (print_string "LE ";
		     print_newline();
                    print_expr a;
                    print_expr b;
			)
    | If (a, b, c) ->
            (print_string "IF ";
		       print_newline();
                       print_expr a;
		       print_string "then ";
                       print_expr b;
		       print_string "else ";
		       print_newline();
			print_expr c;
			)
    | Let ((a, b), c, d) ->
            (print_string "LET ";
		print_newline();
             print_string a;
             Type.print_type b;
             print_expr c;
             print_expr d;
		)
    | Var v -> (print_string "VAR ";
                     print_string v;
		print_newline()
			)
    | LetRec (f, t) -> (print_string "LETREC ";
			print_newline();
                        print_fundef f;
			print_string("in");
			print_newline();
                        print_expr t;
			)
    | App (a, b) -> (print_string "APP ";
                     print_expr a;
                     print_list b;
			)
    | Tuple a -> (print_string "TUPLE ";
		     print_newline();
                  print_list a;
		     print_newline()
			)
    | LetTuple (l, a, b) -> (print_string "LETTUPLE ";
                             print_list_pair l;
                             print_expr a;
                             print_expr b;
				)
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
    let (a, b) = f.name in (print_string a; print_string " "; Type.print_type b);
    print_string " ";
    print_list_pair f.args;
    print_newline();
    print_expr f.body
