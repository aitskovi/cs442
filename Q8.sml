
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
fun --(p1,p2) cl =
    let val (x, cl') = p1 cl
        val (y, cl'') = p2 cl'
    in ((x, y), cl'') end
infix --

(* Alternation *)
fun ||(p1,p2) cl = p1 cl handle Fail _ => p2 cl
infix ||

fun >>(p,f) cl = let val (x, cl') = p cl in (f x, cl') end
infix >>

fun --::(p1,p2) = (p1 -- p2) >> op ::
infix --::

fun --$(p1,p2) = (p1 -- p2) >> #1
infix --$;
fun $--(p1,p2) = (p1 -- p2) >> #2
infix $--;

fun empty cl = ([], cl)
fun star p cl = (p --:: star p || empty) cl
fun plus p = p --:: star p

(* Takes the next character and runs a test on it *)
fun tc t [] = raise Fail "Unexpected End"
 |  tc t (x::xs)  = if t x then (x,xs) else raise Fail "Invalid Character"

fun ch (c: char) = tc (fn x=> (x=c))

(* Check if a character is a digit *)
val digit = tc Char.isDigit

(* Check if a character is alphabetic *)
val alpha = tc Char.isAlpha

fun parens p = ch #"(" $-- p --$ ch #")"
val w = star (tc Char.isSpace) >> implode
fun ws p = w $-- p --$ w

  (* Type Variables *)
val int_type = (ch #"I") $-- (ch #"n") $-- (ch #"t") >> (fn _=> T_Int)
val bool_type = (ch #"B") $-- (ch #"o") $-- (ch #"o") $-- (ch #"l") >> (fn _=> T_Bool)

fun parseType cl =
    let
      val val_type = ws (int_type || bool_type || parens parseType)
      val fun_type = (val_type --$ ws (ch #"-")) -- ((ch #">") $-- parseType) >> (fn (x,y)=> T_Fun(x,y))
    in
      ws (fun_type || val_type) cl
    end

(* Expression Variables *)
val true_expr = ch #"t" $-- ch #"r" $-- ch #"u" $-- ch #"e" >> (fn _=> true)
val false_expr = ch #"f" $-- ch #"a" $-- ch #"l" $-- ch #"s" $-- ch #"e" >> (fn _=> false)
val integer = plus digit >> (valOf o Int.fromString o implode)
val negative_integer = ch #"~" $-- integer >> (fn x=> ~x)
val var = alpha -- (star (digit || alpha))

fun parseML cl = raise Fail "undefined";

Control.Print.printDepth := 100;
