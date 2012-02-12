datatype arith = Number of int
               | Plus of arith * arith
               | Times of arith * arith
               | Exp of arith * arith

datatype ops = OpPlus | OpTimes | OpExp
exception Fail of string

fun rightAssoc OpExp = true
  | rightAssoc _     = false

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
(* fun app p cl = (p --## app p || empty) cl *)

(* Takes the next character and runs a test on it *)
fun tc t [] = raise Fail "Unexpected End"
 |  tc t (x::xs)  = if t x then (x,xs) else raise Fail "Invalid Character"

fun ch (c: char) = tc (fn x=> (x=c))
fun st (s: string) = foldr (fn (fst, rst) => (ch fst --:: rst)) empty  (explode s)

(* Check if a character is a digit *)
val digit = tc Char.isDigit

(* Check if a character is alphabetic *)
val alpha = tc Char.isAlpha

fun parens p = ch #"(" $-- p --$ ch #")"
val w = star (tc Char.isSpace) >> implode
fun ws p = w $-- p --$ w

(* Expression Variables *)
val integer = plus digit >> (valOf o Int.fromString o implode)
val negative_integer = ch #"~" $-- integer >> (fn x=> ~x)
val number = integer || negative_integer >> (fn x=> Number(x))

fun precOf opr = 
  case opr of
     OpPlus => 1
     | OpTimes => 2
     | OpExp => 3

fun apply opr x y =
  case opr of
       OpPlus => Plus(x, y)
     | OpTimes =>Times(x, y)
     | OpExp => Exp(x,y)

val pop = (ws ((ch #"+" >> (fn _ => OpPlus)) || 
               (ch #"*" >> (fn _ => OpTimes)) ||
               (ch #"^" >> (fn _ => OpExp))))

fun infixes (pv, pop, precOf, rightAssoc, apply) =
    let fun level k cl = next k (pv cl)
        and next k (x, cs) =
            let val (opr, cs') = pop cs
                val q = if rightAssoc opr then precOf opr else 1+(precOf opr)
            in
              if precOf opr >= k 
              then next k ((level q >> apply opr x) cs')
              else (x, cs)
            end
                handle Fail _ => (x,cs)
    in level 0
    end

fun
  parseExpr sl = (ws (number || parens parseArith)) sl
and
  parseArith sl = infixes (parseExpr, pop, precOf, rightAssoc, apply)  sl;

parseArith (explode "2 + 3 * 4^5^6");
Control.Print.printDepth := 100;
