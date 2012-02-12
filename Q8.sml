
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
(* fun app p cl = (p --## app p || empty) cl *)

(* Takes the next character and runs a test on it *)
fun tc t [] = raise Fail "undefined: Unexpected End"
 |  tc t (x::xs)  = if t x then (x,xs) else raise Fail "undefined: Invalid Character"

fun ch (c: char) = tc (fn x=> (x=c))
fun st (s: string) = foldr (fn (fst, rst) => (ch fst --:: rst)) empty  (explode s)

(* Check if a character is a digit *)
val digit = tc Char.isDigit

(* Check if a character is alphabetic *)
val alpha = tc Char.isAlpha

fun parens p = ch #"(" $-- p --$ ch #")"
val w = star (tc Char.isSpace) >> implode
fun ws p = w $-- p --$ w

  (* Type Variables *)
val int_type = st "Int" >> (fn _=> T_Int)
val bool_type = st "Bool" >> (fn _=> T_Bool)

fun parseType cl =
    let
      val val_type = ws (int_type || bool_type || parens parseType)
      val fun_type = (val_type --$ ws (st "->") -- parseType) >> (fn (x,y)=> T_Fun(x,y))
    in
      ws (fun_type || val_type) cl
    end

(* Expression Variables *)
val true_expr = (st "true") >> (fn _=> Bool(true))
val false_expr = (st "false")  >> (fn _=> Bool(false))
val integer = plus digit >> (valOf o Int.fromString o implode)
val negative_integer = ch #"~" $-- integer >> (fn x=> ~x)
val number = integer || negative_integer >> (fn x=> Int(x))
val var_string = alpha --:: (star (digit || alpha)) >> implode
val var = var_string >> (fn (x)=> Var(x))

fun precOf opr =
  case opr of
     OpPlus => 1
     | OpTimes => 2

fun apply opr x y =
  case opr of
       OpPlus => BinOp(OpPlus, x, y)
     | OpTimes => BinOp(OpTimes, x, y)

fun rightAssoc _ = false

val pop = (ws (ch #"+" >> (fn _ => OpPlus) ||
              (ch #"*" >> (fn _ => OpTimes))))

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

(* fun makeApp x [] = [x]
  | makeApp x [y] = [App(x,y)] *)

(* fun compress l = foldr (fn ((fst : expr), (rst: expr list)) => makeApp fst
* rst) empty l *)

fun parseExpr cl =
  let
    val if_expr = st "if" $-- parseArith --$ ws (st "then") -- parseArith --$ ws (st "else") -- parseArith >> (fn ((x,y),z) => If(x,y,z))
    val fn_expr = st "fn" $-- ws var_string --$  ws (ch #":") -- parseType --$ ws (st "=>") -- parseArith >> (fn ((x,y),z) => Fun((x,y),z))
    val iszero_expr = st "iszero" $-- parseArith >> (fn x => UnOp(OpIsZero, x))
    val non_recur  = iszero_expr || if_expr || fn_expr || number || true_expr || false_expr || var || parens parseArith
  in
    (ws non_recur) cl
  end
and
    parseArith cl = infixes(parseExpr, pop, precOf, rightAssoc, apply) cl

fun parseML cl = parseArith cl;

Control.Print.printDepth := 100;
