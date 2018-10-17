(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x

let rec g env = function (* α変換ルーチン本体 (caml2html: alpha_g) *)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Float(d) -> Float(d)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = Id.genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2)
  | Var(x) -> Var(find x env)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      LetRec({ name = (find x env, t);
               args = List.map (fun (y, t) -> (find y env', t)) yts;
               body = g env' e1 },
             g env e2)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> (* LetTupleのα変換 (caml2html: alpha_lettuple) *)
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
               find y env,
               g env' e)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)



let rec print_list l =
    match l with
    | [] -> print_string " "
    | x :: xs -> (print_string x; print_string " "; (print_list xs))

and print_list_pair l =
     match l with
    | [] -> print_string " "
    | (a, b) :: xs -> (print_string a;
                       print_string " ";
                       Type.print_type b;
                       print_string " ";
                       print_list_pair xs)
and  print_alpha e =  (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  match e with
  | Unit -> print_string "Unit "
  | Int(i) ->    print_string "Int ";
                        print_int i;
                        print_string " "
  | Float(d) ->
                        print_string "Float ";
                        print_string (string_of_float d);
                        print_string " "
  | Neg(e) ->
                        print_string "NEG ";
                        print_string e;
                        print_string " "
  | Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
                        print_string "Add ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | Sub(e1, e2) ->
                        print_string "SUB ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | FNeg(e) ->
                        print_string "FNeg ";
                        print_string e;
                        print_newline()
| FAdd(e1, e2) ->
                        print_string "FAdd ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | FSub(e1, e2) ->
                        print_string "FSub ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | FMul(e1, e2) ->
                        print_string "FMul ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | FDiv(e1, e2) ->
                        print_string "FDiv ";
                        print_string e1;
                        print_string " ";
                        print_string e2;
                        print_newline()
  | IfEq(e1, e2, e3, e4) ->
        print_string "IFEQ ";
             print_string e1;
                print_string " ";
             print_string e2;
                print_string " ";
             print_alpha e3;
                print_string "\nIN\n";
             print_alpha e4
  | IfLE(e1, e2, e3, e4) ->
        print_string "IFLE ";
             print_string e1;
                print_string " ";
             print_string e2;
                print_string " ";
             print_alpha e3;
                print_string "\nIN\n";
             print_alpha e4
  | Let((x, t), e1, e2) ->
        print_string "LET ";
             print_string x;
             print_string " ";
             Type.print_type t;
             print_string " = ";
             print_alpha e1;
             print_string "IN  \n";
             print_alpha e2
  | Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
                print_string "VAR ";
                print_string x
| LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
                print_string "LETREC ";
             print_string x;
                print_string " ";
                Type.print_type t;
             print_string " ";
                print_list_pair yts;
             print_string " = \n";
             print_alpha e1;
             print_string " RECIN\n ";
             print_alpha e2
  | App(e1, e2s) ->
                        print_string "APP ";
                      print_string e1;
                      print_string " ";
                      print_list e2s
  | Tuple(es) ->
                                print_string "TUPLE ";
                                print_list es
  | LetTuple(xts, e1, e2) ->
                             print_string "LETTUPLE ";
                              print_list_pair xts;
                              print_string " ";
                              print_string e1;
                              print_string " ";
                              print_alpha e2
  | Get(e1, e2) ->
                print_string "GET ";
                       print_string e1;
                       print_string " ";
                       print_string e2
  | Put(e1, e2, e3) ->
             print_string "PUT ";
             print_string e1;
             print_string " ";
             print_string e2;
             print_string " ";
             print_string e3
  |  ExtArray id ->  print_string "EXTARREY ";
                      print_string id
  | ExtFunApp (id, l) ->  print_string "EXTFUNAPP \n";
                            print_string id;
                            print_string " ";
                            print_list l;
                                print_newline()

let f = 
g M.empty
