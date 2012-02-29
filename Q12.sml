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

(* Get the type of a typevar from a type environment *)
fun typeOf x []  = raise Fail "unbound"
  | typeOf x ((y1,y2)::ys) = if x=y1 then y2 else typeOf x ys

(* Apply set of substitutions to a type expression *)
fun subExp e [] = e
  | subExp T_Int _ = T_Int
  | subExp T_Bool _ = T_Bool
  | subExp (T_Fun(x,y)) ts = T_Fun((subExp x ts), (subExp y ts))
  | subExp (T_List(x)) ts = T_List(subExp x ts)
  | subExp x ((t1,t2)::ts) = if x=t1 then t2 else subExp x ts

(*Apply a set of substitutions to a type environment *)
fun subEnv c [] = c
  | subEnv [] _ = []
  | subEnv ((c1,c2)::cs) s = ((c1, subExp c2 s)::(subEnv cs s))

fun contains x [] = false
  | contains x ((y1,y2)::ys) = if x=y1 then true else contains x ys

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
  | unify _ (T_Fun(_, _)) = raise Fail "not function"
  | unify (T_Fun(_, _)) _ = raise Fail "not function"
  | unify _ _ = raise Fail "mismatch"

fun w c (Int(_)) = ([], T_Int)
  | w c (Bool(_)) = ([], T_Bool)
  | w c (Var(x)) = ([], typeOf x c)
  | w c (Fun(x,y)) = 
    let
      val temp = freshTVar()
      val (s,t) = w ((x,temp)::c) y
    in
      (s, T_Fun(subExp temp s, t))
    end
  | w c (App(x,y)) = 
    let
      val (s1, t1) = w c x
      val (s2, t2) = w (subEnv c s1) y
      val temp = freshTVar()
      val s3 = unify (subExp t1 s2) (T_Fun(t2, temp))
    in
      (s3 o s2 o s1, subExp temp s3)
    end
    
fun typeExpr e = 
  let 
    val (s,t) = w [] e
  in
    t
  end

fun subs (T_Var(x)) (T_Var(y)) = if x=y then [] else [(T_Var(x),T_Var(y))]
  | subs (T_Fun(a,b)) (T_Fun(c,d)) = (subs a c) @ (subs b d)
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
