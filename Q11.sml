datatype binOp = OpPlus | OpTimes | OpCons

datatype unOp = OpIsZero | OpHead | OpTail | OpIsEmpty

datatype myType = T_Int | T_Bool | T_Fun of myType * myType | T_List of myType

datatype expr = Int of int
              | Bool of bool
              | If of expr * expr * expr
              | BinOp of binOp * expr * expr
              | UnOp of unOp * expr
              | Var of string
              | Fun of (string * myType) * expr
              | App of expr * expr
              | Let of (string * expr) * expr
              | Letrec of ((string * myType) * expr) * expr
              | Empty of myType

exception Fail of string

fun expect test x y = if x = y then () else print ("Wrong value, test " ^
  (Int.toString test) ^ "\n")

(* Environment is defined as a list of name-value tries *)
fun find_t x [] = raise Fail "not bound"
  | find_t x ((y1,y2)::ys) = if x=y1 then y2 else find_t x ys

(* (expect 1 (find_t Var("a") [(Var("a"), Bool(true))]) SOME(Bool(true))) *)
(* (expect 2 (find_t Var("b") [(Var("b"), Bool(true))]) NONE) *)
(* (expect 3 (find_t Var("c") [(Var("c"), Bool(true))] *)

fun typeUnOp OpIsEmpty (T_List(_)) = T_Bool
  | typeUnOp OpIsZero T_Int = T_Bool
  | typeUnOp OpHead (T_List(x)) = x
  | typeUnOp OpTail (T_List(x)) = T_List(x)
  | typeUnOp _ _ = raise Fail "mismatch"

fun typeBinOp OpPlus x y = if x = y then x else raise Fail "mismatch"
  | typeBinOp OpTimes x y = if x = y then x else raise Fail "mismatch"
  | typeBinOp OpCons x (T_List(y)) = if x = y then T_List(x) else raise Fail "mismatch"
  | typeBinOp _ _ _ = raise Fail "mismatch"

fun typeApp (T_Fun(x,y)) z env = if x=(typeML z env) then y else raise Fail "mismatch"
  | typeApp x y env = raise Fail "not function"
and
    typeML (Int(_)) _ = T_Int
  | typeML (Bool(_)) _ = T_Bool
  | typeML (Empty(x)) _ = T_List(x)
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
  | typeML (BinOp(x, y, z)) env = typeBinOp x (typeML y env) (typeML z env)
  | typeML (UnOp(x, y)) env = typeUnOp x (typeML y env)
  | typeML (Var(x)) env = find_t x env
  | typeML (Fun((x,y),z)) env = T_Fun(y, typeML z ((x,y)::env))
  | typeML (App(x,y)) env = typeApp (typeML x env) y env
  | typeML (Let((x,y),z)) env = typeML z ((x, typeML y env)::env)
  | typeML (Letrec(((w,x),y),z)) env =
        let
          val ty = typeML y ((w,x)::env)
        in
          if ty = x then typeML z ((w,x)::env) else raise Fail "mismatch"
        end

fun typeExpr expr = typeML expr []

(* (expect 4 (typeExpr (Bool(true))) T_Bool) *)
(* (expect 5 (typeExpr (Int(3))) T_Int) *)
(* (expect 6 (typeExpr (If(Bool(true), Int(3), Int(3))) T_Int)) *)
(* (expect 7 (typeExpr (If(Bool(true), Bool(true), Int(3)))) #error) *)
(* (expect 7 (typeExpr (If(Int(3), Int(3), Int(3)))) #error) *)
(* (expect 8 (typeExpr (BinOp(OpPlus, Int(3), Int(3)))) T_Int) *)
(* (expect 9 (typeExpr (Fun(("v", T_Int), Var("v")))) T_Fun(T_Int, T_Int)) *)
(* (expect 10 (typeExpr (BinOp(OpCons, Int(1), Int(2)))) #error) *)
(* (expect 11 (typeExpr (BinOp(OpCons, Int(1), Empty(T_Int)))) T_List T_Int) *)
(* (expect 12 (typeExpr (BinOp(OpCons, Int(1), Empty(T_Bool)))) #error) *)
(* (expect 13 (typeExpr (BinOp(OpCons, Int(1), (BinOp(OpCons, Int(1), Empty(T_Int)))))) T_List T_Int) *)
(* (expect 14 (typeExpr (UnOp(OpHead, (BinOp(OpCons, Int(1), Empty(T_Int))))) T_List T_Int) *)
(* (expect 15 (typeExpr (UnOp(OpHead, (BinOp(OpCons, Int(1), Empty(T_Int))))) T_List T_Int) *)
