
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

(* Parser Combinators *)

(* Concatentation *)
fun (p1 -- p2) cl =
    let val (x, cl') = p1 cl
        val (y, cl'') = p2 cl'
    in ((x, y), cl'') end

(* Alternation *)
fun (p1 || p2) cl = p1 cl handle Fail _ => p2 cl

fun (p >> f) cl = let val (x, cl') = p cl in (f x, cl') end
fun (p1 --:: p2) = (p1 -- p2) >> op ::
fun star p cl = (p --:: start p || empty) cl
fun plus p = p --:: star p

(* Takes the next character and runs a test on it *)
fun tc t [] = raise Fail "Unexpected End"
 |  tc t (x::xs)  = if t x then (x,xs) else raise Fail "Invalid Character"

(* Check if a character is a digit *)
val digit = tc Char.isDigit

(* Check if a character is alphabetic *)
val alpha = tc Char.isAlpha

val integer = plus digit >> (valOf o Int.fromString o implode)
val var = alpha -- (star (digit || alpha))

fun parseML cl = raise Fail "undefined"
