(* this file has all variants defined for Q12-16; trim out the ones not used *)

datatype binOp = OpPlus | OpTimes | OpCons

datatype unOp = OpIsZero | OpIsEmpty | OpHead | OpTail

datatype myType = T_Int | T_Bool | T_Fun of myType * myType 
                | T_Var of int | T_List of myType

datatype expr = Int of int
              | Bool of bool
              | If of expr * expr * expr
              | BinOp of binOp * expr * expr 
              | UnOp of unOp * expr 
              | Var of string
              | Fun of string * expr
              | App of expr * expr
              | Let of string * expr * expr
              | Letrec of string * expr * expr
              | Empty

exception Fail of string

(* permitted mutation *)

val count = ref 0

fun freshTVar() = T_Var (count := !count+1; !count)

fun resetTVar() = count := 0

(* end of permitted mutation *)

(* define the functions below *)

fun check_expect test x y = if x = y then () else print ("Wrong value, test " ^
  (Int.toString test) ^ "\n")

fun typeToString T_Int = "T_Int"
  | typeToString T_Bool = "T_Bool"
  | typeToString (T_Fun(x,y)) = "T_Fun ("^ typeToString x ^", "^ typeToString y ^")"
  | typeToString (T_List(x)) = "T_List ("^ typeToString x ^")"
  | typeToString (T_Var(x)) = "T_Var ("^(Int.toString x)^")"

fun envToString [] = "=============================\n"
  | envToString ((c1, c2)::cs) = "(" ^ c1 ^ "," ^ (typeToString c2) ^ ") \n" ^ (envToString cs)

fun subsToString [] = "=============================\n"
  | subsToString ((c1,c2)::cs) = "(" ^ (typeToString c1) ^ "," ^ (typeToString
  c2) ^ ") \n" ^ (subsToString cs)

fun printType t = print ((typeToString t) ^ "\n")
fun printEnv c = print ("Environment: \n" ^ (envToString c))
fun printSubs s = print ("Substitutions: \n" ^ (subsToString s))

(* Apply set of substitutions to a type expression *)
fun subExp e [] = e
  | subExp T_Int _ = T_Int
  | subExp T_Bool _ = T_Bool
  | subExp (T_Fun(x,y)) ts = T_Fun((subExp x ts), (subExp y ts))
  | subExp (T_List(x)) ts = T_List(subExp x ts)
  | subExp x ((t1,t2)::ts) = if x=t1 then t2 else subExp x ts

(* Apply a set of substitutions to a type environment *)
fun subEnv c [] = c
  | subEnv [] _ = []
  | subEnv ((c1,(c2, c3))::cs) s = ((c1, (c2, subExp c3 s))::(subEnv cs s))

(* Check if an environment contains a specific type variable *)
fun contains x [] = false
  | contains x ((y1,y2)::ys) = if x=y1 then true else contains x ys

fun containsVar x [] = false
  | containsVar x ((y1, (y2, y3))::ys) = if x=y3 then true else containsVar x ys

fun quantify (T_Fun(x,y)) c =
    let
      val (a,b) = quantify x c
      val (c,d) = quantify y c
    in
      (a @ c, T_Fun(b,d))
  end
  | quantify (T_List(x)) c = 
    let
      val (a,b) = quantify x c
    in
      (a, T_List(b))
    end
  | quantify (T_Var(x)) c = if containsVar (T_Var(x)) c then ([], T_Var(x)) else ([T_Var(x)], T_Var(x))
  | quantify x c = ([], x)

fun instantiate (q, x) = subExp x (map (fn (t) => (t, freshTVar())) q)

(* Environment is now ('foldr', (quantified types, expr) *)
(* Get the type of a typevar from a type environment *)
fun typeOf x []  = raise Fail "unbound"
  | typeOf x ((y1,y2)::ys) = if x=y1 then instantiate y2 else typeOf x ys

fun filter p [] = []
  | filter p (x::xs) = if p x then x::(filter p xs) else filter p xs

fun op o(s1,s2) = 
  (map (fn (x,y) => (x, subExp y s1)) s2) @ (filter (fn (x,y) => not(contains x s2)) s1)
infix o;

(* Test a if a certain type variable occurs in a type expression *)
fun occurs x T_Bool = false
  | occurs x T_Int = false
  | occurs x (T_Fun(y,z)) = occurs x y orelse occurs x z
  | occurs x (T_Var(y)) = x=y
  | occurs x (T_List(y)) = occurs x y

fun unify T_Int T_Int = []
  | unify T_Bool T_Bool = []
  | unify (T_List(x)) (T_List(y)) = unify x y
  | unify (T_Fun(t1, t2)) (T_Fun(t3, t4)) = 
    let
      val s1 = unify t1 t3
      val s2 = unify (subExp t2 s1) (subExp t4 s1)
    in
      s2 o s1
    end
  | unify (T_Var(x)) (T_Var(y)) = if x=y then [] else [(T_Var(x), T_Var(y))] 
  | unify (T_Var(x)) y =
    if occurs x y then raise Fail "circularity"
    else [(T_Var(x),y)]
  | unify x (T_Var(y)) =
    if occurs y x then raise Fail "circularity"
    else [(T_Var(y),x)]
  | unify x (T_Fun(_, _)) = raise Fail "not function"
  | unify _ _ = raise Fail "mismatch"

fun wUnOp c OpIsZero x = 
    let
      val (s,t) = w c x
      val s' = (unify t T_Int);
    in
      (s' o s, T_Bool) (* OpIsZero returns a boolean *)
    end
  | wUnOp c OpIsEmpty x = 
    let
      val (s,t) = w c x
      val temp = T_List(freshTVar());
      val s' = unify t temp
    in
      (s' o s, T_Bool) (* OpIsEmpty returns a boolean *)
    end
  | wUnOp c OpHead x =
    let
      val (s,t) = w c x
      val temp = freshTVar();
      val s' = unify t (T_List(temp))
    in
      (s' o s, subExp temp (s' o s)) (* OpHead returns a type *)
    end
  | wUnOp c OpTail x =
    let
      val (s,t) = w c x
      val temp = T_List(freshTVar())
      val s' = unify t temp
    in
      (s' o s, subExp temp (s' o s)) (* OpTail returns a list *)
    end
and
    wBinOp c OpCons y z =
      let
        val (s1,t1) = w c y
        val (s2,t2) = w (subEnv c s1) z
        val s3 = unify t2 (T_List(t1))
      in
        (s3 o s2 o s1, T_List(subExp t1 (s3 o s2 o s1)))  (*OpCons returns a list *)
      end
  | wBinOp c _ y z =
      let
        val (s1,t1) = w c y
        val (s2,t2) = w (subEnv c s1) z
        val s3 = unify t1 T_Int
        val s4 = unify t2 T_Int
      in
        (s4 o s3 o s2 o s1, subExp t1 (s4 o s3 o s2 o s1)) 
      end
and
    w c (Int(_)) = ([], T_Int)
  | w c (Bool(_)) = ([], T_Bool)
  | w c (Var(x)) = ([], typeOf x c)
  | w c (Fun(x,y)) =
    let
      val temp = freshTVar()
      val (s,t) = w ((x,([],temp))::c) y
    in
      (s, T_Fun(subExp temp s, t))
    end
  | w c (App(x,y)) = 
    let
      val (s1,t1) = w c x
      val (s2,t2) = w (subEnv c s1) y
      val temp = freshTVar() 
      val s3 = unify (subExp t1 s2) (T_Fun(t2, temp))
    in
      (s3 o s2 o s1, subExp temp (s3 o s2 o s1))
    end
  | w c (If(x,y,z)) = 
    let 
      val (s,t) = (w c x)
      val s1 = (unify t T_Bool) o s
      val (s2,t2) = w (subEnv c s1) y
      val (s3,t3) = w (subEnv c (s2 o s1)) z
      val s4 = unify (subExp t2 s3) t3
    in
      (s4 o s3 o s2 o s1, (subExp t3 (s4 o s3 o s2 o s1)))
    end
  | w c (UnOp(x,y)) = wUnOp c x y
  | w c (BinOp(x,y,z)) = wBinOp c x y z
  | w c (Let(x,y,z)) =
    let
      val (s1,t1) = w c y
      val c1 = (subEnv c s1)
      val q1 = quantify t1 c1
      val (s2,t2) = w ((x,q1)::c1) z
    in
      (s2 o s1, t2)
    end
  | w c (Letrec(x,y,z)) =
    let
        val temp = freshTVar()
        val (s1,t1) = w ((x,([], temp))::c) y
        val s2 = unify t1 (subExp temp s1)
        val t2 = subExp t1 s2
        val c1 = subEnv c (s2 o s1)
        val q2 = quantify t2 c1
        val (s3, t3) = w ((x,q2)::c) z
    in
        (s3 o s2 o s1, t3)
    end
  | w c Empty = ([], T_List(freshTVar()))

fun typeExpr e = 
  let 
    val (s,t) = w [] e
  in
    t
  end

fun subs (T_Var(x)) (T_Var(y)) = if x=y then [] else [(T_Var(x),T_Var(y))]
  | subs (T_Fun(a,b)) (T_Fun(c,d)) = (subs a c) @ (subs b d)
  | subs (T_List(a)) (T_List(b)) = (subs a b)
  | subs _ _ = []

fun typeEquiv e1 e2 = 
    let
      val s1 = subs e1 e2
      val s2 = subs e2 e1 
      val e1' = subExp e1 s1
      val e2' = subExp e2 s2
    in
      e1'=e2 andalso e1=e2'
    end;

Control.Print.printDepth := 100;

check_expect 1 (typeExpr (Int(1))) T_Int;
check_expect 2 (typeExpr (Bool(true))) T_Bool;
check_expect 3 (typeExpr (Fun("x", Int(1)))) (T_Fun(T_Var(1), T_Int));
resetTVar();
check_expect 4 (typeExpr (Fun("x", Var("x")))) (T_Fun(T_Var(1), T_Var(1)));
resetTVar();
check_expect 5 (typeExpr (App((Fun("x", Var("x"))), Int(1)))) T_Int;
resetTVar();
check_expect 6 (typeExpr (App((Fun("x", Var("x"))), Bool(true)))) T_Bool;

check_expect 7 (typeEquiv T_Int T_Bool) false;
check_expect 8 (typeEquiv T_Int T_Int) true;
check_expect 9 (typeEquiv (T_Fun(T_Var(1), T_Var(2))) (T_Fun(T_Var(1), T_Var(2)))) true;
check_expect 10 (typeEquiv (T_Fun(T_Var(1), T_Var(2))) (T_Fun(T_Var(3), T_Var(4)))) true;
check_expect 11 (typeEquiv (T_Fun(T_Int, T_Var(2))) (T_Fun(T_Bool, T_Var(3)))) false;
check_expect 12 (typeEquiv (T_Fun(T_Int, T_Var(2))) (T_Fun(T_Int, T_Var(2)))) true;
check_expect 13 (typeEquiv (T_Fun(T_Int, T_Var(2))) T_Int) false;
check_expect 14 (typeEquiv (T_Fun(T_Int, T_Int)) (T_Var(1))) false;
check_expect 15 (typeEquiv (T_Var(1)) (T_Fun(T_Int, T_Int))) false;
check_expect 16 (typeEquiv (T_Var(1)) T_Int) false;
check_expect 17 (typeEquiv T_Int (T_Var(1))) false;
check_expect 18 (typeEquiv (T_Fun(T_Var(1), T_Var(1))) (T_Fun(T_Var(2), T_Var(1)))) false;
check_expect 19 (typeEquiv (T_Fun(T_Var(1), T_Var(2))) (T_Fun(T_Var(2), T_Var(1)))) true;

(* If Tests *)
resetTVar();
check_expect 20 (typeExpr (If(Bool(true), Int(1), Int(2)))) T_Int;
resetTVar();
check_expect 21 (typeExpr (Fun("x", If(Var("x"), Var("x"), Var("x"))))) (T_Fun(T_Bool, T_Bool));
resetTVar();
check_expect 26 (typeExpr (Fun("y", Fun("x", If(Var("x"), Var("x"), Var("y")))))) (T_Fun(T_Bool,(T_Fun(T_Bool, T_Bool))));
resetTVar();

(* OpIsZero *)
check_expect 22 (typeExpr (UnOp(OpIsZero, Int(1)))) T_Bool;
resetTVar();
check_expect 23 (typeExpr (Fun("x", UnOp(OpIsZero, Var("x"))))) (T_Fun(T_Int,T_Bool));
resetTVar();
resetTVar();

(* BinOp *)
check_expect 24 (typeExpr (BinOp(OpTimes, Int(1), Int(2)))) T_Int;
resetTVar();
check_expect 25 (typeExpr (Fun("x", BinOp(OpTimes, Int(1), Var("x"))))) (T_Fun(T_Int,T_Int));
resetTVar();

(* List Tests *)
check_expect 27 (typeEquiv (typeExpr Empty) (T_List(T_Var(1)))) true;
check_expect 28 (typeExpr (BinOp(OpCons, Int(1), Empty))) (T_List(T_Int));
check_expect 29 (typeExpr (Fun("x", BinOp(OpCons, Int(1), Var("x")))))
                (T_Fun(T_List(T_Int), T_List(T_Int)));

val sml_foldr = (Letrec("foldr", Fun("p", Fun("e", Fun("l", 
    If(UnOp(OpIsEmpty, Var("l")),
    Var("e"), 
    App(App(Var("p"), UnOp(OpHead, Var("l"))),
        App(App(App(Var("foldr"),Var("p")), Var("e")), Var("l"))))))),
  Var("foldr")));

check_expect 30 (typeEquiv (typeExpr sml_foldr)
    (T_Fun (T_Fun (T_Var 6,T_Fun (T_Var 10,T_Var 10)), T_Fun (T_Var 10,T_Fun
    (T_List (T_Var 6),T_Var 10))))) true;

val sml_filter = 
  (Letrec("filter", Fun("f", Fun("l", 
    If(UnOp(OpIsEmpty, Var("l")),
    Empty, 
    If(App(Var("f"), UnOp(OpHead, Var("l"))),
        BinOp(OpCons, UnOp(OpHead, Var("l")), App(App(Var("filter"), Var("f")), Var("l"))), 
        App(App(Var("filter"),Var("f")), Var("l")))))),
  Var("filter")));
val sml_map =
  (Letrec("map", Fun("f", Fun("l", 
    If(UnOp(OpIsEmpty, Var("l")),
        Empty, 
        BinOp(OpCons,
                App(Var("f"), UnOp(OpHead, Var("l"))), App(App(Var("map"),
                Var("f")), UnOp(OpTail, Var("l"))))))),
  Var("map")));

(* Foldr  Tests *)
val sml_sum = Fun("x", Fun("y", BinOp(OpPlus, Var("x"), Var("y"))));
check_expect 31 (typeEquiv (typeExpr sml_sum)
    (T_Fun(T_Int , T_Fun(T_Int, T_Int)))) true;
check_expect 33 (typeEquiv (typeExpr (App(sml_foldr, sml_sum))) 
    (T_Fun (T_Int,T_Fun (T_List (T_Int),T_Int)))) true;

(* Filter Tests*)
check_expect 31 (typeEquiv (typeExpr sml_filter)
  (T_Fun (T_Fun (T_Var 1,T_Bool),T_Fun (T_List (T_Var 1),T_List (T_Var 1)))))
  true; 

(* Map Tests *)
check_expect 32 (typeEquiv (typeExpr sml_map)
  (T_Fun (T_Fun (T_Var 1, T_Var 2),T_Fun (T_List (T_Var 1),T_List (T_Var 2)))))
  true; 

val sml_split =
  (Letrec("split", (Fun("a", 
  If(UnOp(OpIsEmpty, Var("a")), 
    BinOp(OpCons, Empty, BinOp(OpCons,Empty, Empty)),
    If(UnOp(OpIsEmpty, UnOp(OpTail, Var("a"))),
        BinOp(OpCons, Var("a"), BinOp(OpCons, Empty, Empty)),
        Let
          ("b", App(Var("split"), UnOp(OpTail, UnOp(OpTail, Var("a")))),
            BinOp(OpCons, 
                BinOp(OpCons, UnOp(OpHead, Var("a")), UnOp(OpHead, Var("b"))), 
                BinOp(OpCons, BinOp(OpCons, UnOp(OpHead, UnOp(OpTail,Var("a"))), UnOp(OpHead, UnOp(OpTail, Var("b")))), Empty))))))),
            Var("split")));

(* Split *)
check_expect 33 (typeEquiv (typeExpr sml_split)
    (T_Fun (T_List(T_Var 1), T_List(T_List(T_Var 1))))) true;

val sml_length =
  Letrec("len", Fun("a",
    If(UnOp(OpIsEmpty, Var("a")),
        Int(0),
        BinOp(OpPlus, Int(1), App(Var("len"), UnOp(OpTail, UnOp(OpHead, Var("a"))))))),
    Var("len"));    

val sml_length =
  Letrec("len", Fun("a",
    If(UnOp(OpIsEmpty, Var("a")),
        Int(0),
        BinOp(OpPlus, Int(1), App(Var("len"), UnOp(OpTail, Var("a")))))),
    Var("len"));    
check_expect 34 (typeEquiv (typeExpr sml_length)
    (T_Fun (T_List(T_Var 1), T_Int))) true;

val sml_longer =
  Letrec("longer", Fun("a", Fun("b",
    If(UnOp(OpIsEmpty, Var("b")),
        Bool(true),
        If(UnOp(OpIsEmpty, Var("a")),
            Bool(false),
            App(App(Var("longer"),  UnOp(OpTail, Var("a"))), UnOp(OpTail,Var("b"))))))),
    Var("longer"));
check_expect 35 (typeEquiv (typeExpr sml_longer)
     (T_Fun (T_List (T_Var 104),T_Fun (T_List (T_Var 106),T_Bool)))) true;

val sml_merge = 
  (Letrec("merge", Fun("a",Fun("b",
  If(UnOp(OpIsEmpty, Var("a")),
    Var("b"),
    If(UnOp(OpIsEmpty, Var("b")),
        Var("a"),
        (Let("n", UnOp(OpHead, Var("a")),
        (Let("ns", UnOp(OpTail, Var("a")),
        (Let("m", UnOp(OpHead, Var("b")),
        (Let("ms", UnOp(OpTail, Var("b")),
            If(UnOp(OpIsZero, BinOp(OpPlus, Var("n"), Var("m"))),
                BinOp(OpCons, Var("n"), App(App(Var("merge"), Var("ns")), Var("b"))),
                BinOp(OpCons, Var("m"), App(App(Var("merge"), Var("ms")),
                Var("a")))))))))))))))), Var("merge")));
check_expect 36 (typeEquiv (typeExpr sml_merge)
    (T_Fun (T_List T_Int, T_Fun(T_List T_Int, T_List T_Int)))) true;

val sml_msort =
  (Letrec("msort", Fun("l",
    Let("merge", sml_merge,
    Let("split", sml_split,
    If(UnOp(OpIsEmpty, Var("l")),
        Empty,
        If(UnOp(OpIsEmpty, UnOp(OpTail, Var("l"))),
            Var("l"),
            Let("ms", UnOp(OpHead, App(Var("split"),Var("l"))),
            Let("ns", UnOp(OpHead, UnOp(OpTail, App(Var("split"),Var("l")))),
            Let("ms'", App(Var("msort"), Var("ms")),
            Let("ns'", App(Var("msort"), Var("ns")),
                App(App(Var("merge"), Var("ms'")), Var("ns'"))))))))))),
    Var("msort")));
check_expect 37 (typeEquiv (typeExpr sml_msort)
    (T_Fun (T_List T_Int, T_List T_Int))) true;

fun safeTypeExpr x = print (Bool.toString((typeExpr x) = T_Int)) handle (Fail x) => print ("Wrong value, test " ^
  x ^ "\n")
