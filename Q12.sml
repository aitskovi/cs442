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

(* Get the type of a typevar from a type environment *)
fun typeOf x []  = raise Fail "unbound"
  | typeOf x ((y1,y2)::ys) = if x=y1 then y2 else typeOf x ys

(* Apply set of substitutions to a type expression *)
fun subExp e [] = e
  | subExp T_Int _ = T_Int
  | subExp T_Bool _ = T_Bool
  | subExp (T_Fun(x,y)) ts = T_Fun((subExp x ts), (subExp y ts))
  | subExp (T_Var(x)) ((t1,t2)::ts) = if x=t1 then t2 else subExp (T_Var(x)) ts
  | subExp (T_List(x)) ts = T_List(subExp x ts)

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
  | unify (T_Var(x)) (T_Var(y)) = if x=y then [] else [(x, T_Var(y))] 
  | unify (T_Var(x)) y =
    if occurs x y then raise Fail "circularity"
    else [(x,y)]
  | unify x (T_Var(y)) =
    if occurs y x then raise Fail "circularity"
    else [(y,x)]
  | unify _ (T_Fun(_, _)) = raise Fail "not function"
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
    
fun typeExpr e = w [] e

fun typeEquiv e1 e2 = 
  let
    val s = unify e1 e2
  in
    (subExp e1 s) = (subExp e2 s)
  end
(* (expect (typeExpr (Int(1))) ([], T_Int)) *)
(* (expect (typeExpr (Bool(true))) ([], T_Bool)) *)
(* (expect (typeExpr (Fun("x", Int(1)))) ([], T_Fun(T_Var(1), T_Int)) *)
(* (expect (typeExpr (Fun("x", Var("x")))) ([], T_Fun(T_Var(1), T_Var(1))) *)
(* (expect (App((Fun("x", Var("x")))), Int(1))) ([(T_Var(1), T_Int)], T_Fun(T_Int, T_Int))) *)
