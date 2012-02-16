datatype binOp = OpPlus | OpTimes

datatype unOp = OpIsZero

datatype myType = T_Int | T_Bool | T_Fun of myType * myType

datatype expr = Int of int
              | Bool of bool
              | If of expr * expr * expr
              | BinOp of binOp * expr * expr
              | UnOp of unOp * expr
              | Var of string
              | Fun of (string * myType) * expr
              | App of expr * expr

exception Fail of string

fun expect test x y = if x = y then () else print ("Wrong value, test " ^
  (Int.toString test) ^ "\n")

(* Environment is defined as a list of name-value tries *)
fun find_t x [] = raise Fail "not bound"
  | find_t x ((y1,y2)::ys) = if x=y1 then y2 else find_t x ys

(* (expect 1 (find_t Var("a") [(Var("a"), Bool(true))]) SOME(Bool(true))) *)
(* (expect 2 (find_t Var("b") [(Var("b"), Bool(true))]) NONE) *)
(* (expect 3 (find_t Var("c") [(Var("c"), Bool(true))] *)

fun typeApp (T_Fun(x,y)) z env = if x=(typeML z env) then y else raise Fail "mismatch"
  | typeApp x y env = raise Fail "not function"
and
    typeML (Int(_)) _ = T_Int
  | typeML (Bool(_)) _ = T_Bool
  | typeML (If(x,y,z)) env =
        if
          (typeML x env)=T_Bool
        then 
          let
            val ty = typeML y env
            val tz = typeML z env
          in
            if ty = tz then ty else raise Fail "mismatch"
          end
        else
          raise Fail "not Boolean"
  | typeML (BinOp(_, x,y)) env =
        let
          val tx = typeML x env
          val ty = typeML y env
        in
          if tx = ty then tx else raise Fail "mismatch"
        end
  | typeML (UnOp(_, x)) env =
        let
          val tx = typeML x env
        in
          if tx = T_Int then T_Bool else raise Fail "mismatch"
        end
  | typeML (Var(x)) env = find_t x env
  | typeML (Fun((x,y),z)) env = T_Fun(y, typeML z ((x,y)::env))
  | typeML (App(x,y)) env = typeApp (typeML x env) y env

fun typeExpr expr = typeML expr []

(* (expect 4 (typeExpr (Bool(true))) T_Bool) *)
(* (expect 5 (typeExpr (Int(3))) T_Int) *)
(* (expect 6 (typeExpr (If(Bool(true), Int(3), Int(3))) T_Int)) *)
(* (expect 7 (typeExpr (If(Bool(true), Bool(true), Int(3)))) #error) *)
(* (expect 7 (typeExpr (If(Int(3), Int(3), Int(3)))) #error) *)
(* (expect 8 (typeExpr (BinOp(OpPlus, Int(3), Int(3)))) T_Int) *)
(* (expect 9 (typeExpr (Fun(("v", T_Int), Var("v")))) T_Fun(T_Int, T_Int)) *)
