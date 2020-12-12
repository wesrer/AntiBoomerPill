(* Representation of VScheme, with some utility functions *)

(* You'll need to understand what's going on and how it's used. *)

structure VScheme = struct
  type name = string
  datatype exp = LITERAL of value
               | VAR     of name
               | SET     of name * exp
               | IFX     of exp * exp * exp
               | WHILEX  of exp * exp
               | BEGIN   of exp list
               | APPLY   of exp * exp list
               | LETX    of let_kind * (name * exp) list * exp
               | LAMBDA  of lambda
  and let_kind = LET | LETREC
  and    value = SYM       of name
               | NUM       of int
               | BOOLV     of bool   
               | PAIR      of value * value
               | EMPTYLIST
    withtype lambda = name list * exp
(*

case of ( + x y) => x = 2 y = 3
| (+ (+ x y) z)

(+ 2 3)
(+ (+ 2 3) 5)

case of (+ 2 3)
  (+ x y) =>

  case ( + 2  3) of 
    5 =>
    8 =>

    case foo of
      X.BOOL 


*)
  datatype def  = VAL    of name * exp
                | DEFINE of name * lambda
                | EXP    of exp
                | CHECK_EXPECT of exp * exp
                | CHECK_ASSERT of exp
                | CHECK_ERROR of exp
end

structure UnambiguousVScheme = struct
  type name = string
  type primitive = Primitive.primitive
  datatype let_kind = datatype VScheme.let_kind
  datatype exp = LITERAL   of value
               | LOCAL     of name
               | GLOBAL    of name
               | SETLOCAL  of name * exp
               | SETGLOBAL of name * exp
               | IFX       of exp * exp * exp
               | WHILEX    of exp * exp
               | BEGIN     of exp list
               | FUNCALL   of exp * exp list
               | PRIMCALL  of primitive * exp list
               | LETX      of let_kind * (name * exp) list * exp
               | LAMBDA    of lambda
  and let_kind = LET | LETREC
  and    value = SYM       of name
               | NUM       of int
               | BOOLV     of bool   
               | EMPTYLIST
    withtype lambda = name list * exp

  datatype def  = VAL    of name * exp
                | DEFINE of name * lambda
                | EXP    of exp
                | CHECK_EXPECT of string * exp * string * exp
                | CHECK_ASSERT of string * exp
                | CHECK_ERROR of string * exp


  fun valToString (SYM x) = x
    | valToString (NUM n) = Int.toString n
    | valToString (BOOLV b) = Bool.toString b
    | valToString (EMPTYLIST) = "'()"

  fun whatIs (LITERAL v)        = "LITERAL " ^ valToString v
    | whatIs (LOCAL x)          = "LOCAL " ^ x
    | whatIs (GLOBAL x)         = "GLOBAL " ^ x
    | whatIs (SETLOCAL (x, e))  = "SETLOCAL " ^ x
    | whatIs (SETGLOBAL (x, e)) = "SETGLOBAL " ^ x
    | whatIs (IFX (e1, e2, e3)) = "IFX"
    | whatIs (WHILEX (e1, e2))  = "WHILEX"
    | whatIs (BEGIN es)         = "BEGIN"
    | whatIs (FUNCALL (e, es))  = "FUNCALL"
    | whatIs (PRIMCALL (x, es)) = "PRIMCALL " ^ Primitive.name x
    | whatIs (LETX (LET, bs, body))           = "LET"
    | whatIs (LETX (LETREC, bindings, body))  = "LETREC"
    | whatIs (LAMBDA (xs, e))                 = "LAMBDA"

end

structure VSchemeTests : sig
  val delay : VScheme.def list -> VScheme.def list  (* put tests at end *)
end
  =
struct
  structure S = VScheme
  fun isTest (S.CHECK_ASSERT _) = true
    | isTest (S.CHECK_ERROR _) = true
    | isTest (S.CHECK_EXPECT _) = true
    | isTest (S.VAL _) = false
    | isTest (S.DEFINE _) = false
    | isTest (S.EXP _) = false

  fun delay ds =
    let val (tests, other) = List.partition isTest ds
    in  other @ tests
    end
end

structure VSchemeUtils : sig
  type exp = VScheme.exp
  val car : exp -> exp
  val cdr : exp -> exp
  val cons : exp -> exp -> exp
  val list : exp list -> exp
  val nth : int -> exp -> exp
  val setnth : exp -> int -> exp -> exp

  val setcar : exp -> exp -> exp
end
  =
struct
  structure S = VScheme

  type exp = VScheme.exp

  fun car e = S.APPLY (S.VAR "car", [e])
  fun cdr e = S.APPLY (S.VAR "cdr", [e])
  fun cons x xs = S.APPLY (S.VAR "cons", [x, xs])
  fun setcar e v = S.APPLY (S.VAR "set-car!", [e, v])

  fun nth 0 e = car e
    | nth k e = nth (k-1) (cdr e)

  fun list [] = S.LITERAL S.EMPTYLIST
    | list (e::es) = cons e (list es)

  fun setnth e 0 v = S.APPLY (S.VAR "set-car!", [e, v])
    | setnth e k v = setnth (cdr e) (k - 1) v
end
