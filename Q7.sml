
datatype 'a regexp = 
    Empty
  | Epsilon
  | Symbol of 'a
  | Concat of 'a regexp * 'a regexp
  | Alt of 'a regexp * 'a regexp
  | Repeat of 'a regexp;

(* Define the following functions:
   concat, alt, repeat with types matching
   Concat, Alt, Repeat;
   containsEps, deriv, matches *)

(* Smart constructor for the concat regex operator.
 * e.g. a + b *)
fun concat x Empty = Empty
  | concat Empty y = Empty
  | concat x Epsilon = x
  | concat Epsilon y = y
  | concat x y = Concat(x,y);

(* Smart constructor for the alternate regex operator.
 * e.g. a | b *)
fun alt x Empty = x
  | alt Empty y = y
  | alt x y = Alt(x,y);

(* Smart constructor for the repeat regex operator.
 * e.g. a* *)
fun repeat Empty = Epsilon
  | repeat Epsilon = Epsilon
  | repeat x = Repeat(x);

(* Checks if the provided regex contains epsilon *)
fun containsEps Empty = false 
  | containsEps Epsilon = true 
  | containsEps (Symbol(_)) = false
  | containsEps (Concat(x,y)) = containsEps x andalso containsEps y
  | containsEps (Alt(x,y)) = containsEps x orelse containsEps y
  | containsEps (Repeat(_)) = true;

fun deriv x Empty = Empty
  | deriv x Epsilon = Empty
  | deriv x (Symbol(y)) = if x=y then Epsilon else Empty
  | deriv x (Alt(y,z)) = Alt(deriv x y, deriv x z)
  | deriv x (Concat(y,z)) = if containsEps y then Alt(Concat(deriv x y, z), deriv x z)
                       else Concat(deriv x y, z)
  | deriv x (Repeat(y)) = Concat(deriv x y, Repeat(y));

fun matches Empty [] = true
  | matches Empty (y::ys) = false
  | matches Epsilon [] = true
  | matches Epsilon (y::ys) = false
  | matches x [] = containsEps x
  | matches x (y::ys) = matches (deriv y x) ys;

(* Testing Framework *)
exception Fail of string 

fun testPos testFunc (num, texp, res) =
  (if res = testFunc texp then () else print("Wrong value, test " ^ (Int.toString num) ^ "\n"))
  handle (Fail s) => print("Exception on test " ^ (Int.toString num) ^ ": " ^ s ^ "\n");

(* Tests *)

(* containsEps tests *)
fun tE() = app (testPos containsEps)
  [
   (1, (Symbol(#"a")), false),
   (2, (Repeat(Symbol(#"a"))), true),
   (3, (Alt( Repeat(Symbol(#"a")), Repeat(Symbol(#"a")))), true),
   (4, (Alt( Repeat(Symbol(#"a")), Symbol(#"b"))), true),
   (5, (Concat ( Repeat(Symbol(#"a")), Repeat(Symbol(#"a")))), true),
   (6, (Concat ( Repeat(Symbol(#"a")), Symbol(#"b"))), false),
   (7, Empty, false),
   (8, Epsilon, true)
  ];

(* matches tests *)
(*fun tM() = app (testPos matches)
  [
   (1, (Symbol(#"a") [#"a"]), true)
   (*(2, Symbol(#"a"), explode("b"), false),
   (3, Symbol(#"a"), explode("ab"), false)*)
  ];
  *)



(* The SML interpreter limits the depth of
   displayed expressions. You may find this frustrating.
   The following line sets the truncation depth high
   enough for testing purposes. *)

Control.Print.printDepth := 100;
