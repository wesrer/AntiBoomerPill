(* In K-normal form, convert string names to register names *)

(* You'll write this file *)

structure KNRename :> sig
  val regOfName : string -> ObjectCode.reg Error.error
  val mapx : ('a -> 'b Error.error) ->
             ('a KNormalForm.exp -> 'b KNormalForm.exp Error.error)
  val exp :  (('a -> 'b Error.error) * 'a KNormalForm.exp -> 'b KNormalForm.exp Error.error)
end
  = 
struct
  structure K = KNormalForm

  infix 3 <*>   val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>

  val succeed = Error.succeed
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun curry_tup f x y z = f ((x, y), z)

  (* AsmLex.registerNum takes a string starting with "r" followed by a number n
     such that 0 <= n < 256, and returns n *)
  val regOfName = AsmLex.registerNum


  fun exp (f, K.LITERAL v) = K.LITERAL <$> succeed v
    | exp (f, K.NAME n) = K.NAME <$> f n
    | exp (f, K.VMOP (vmop, xs)) = curry K.VMOP vmop <$> errorList (map f xs)
    | exp (f, K.VMOP_LIT (vmop, xs, v)) = curry3 K.VMOP_LIT vmop <$> errorList (map f xs) <*> succeed v
    | exp (f, K.FUNCALL (x, xs)) = curry K.FUNCALL <$> f x <*> errorList (map f xs)
    | exp (f, K.IF_EXP (x, e1, e2)) = curry3 K.IF_EXP <$> f x <*> exp (f, e1) <*> exp (f, e2)
    | exp (f, K.LET (x, e1, e2)) = curry3 K.LET <$> f x <*> exp (f, e1) <*> exp (f, e2)
    | exp (f, K.SEQ (e1, e2)) = curry K.SEQ <$> exp (f, e1) <*> exp (f, e2)
    | exp (f, K.ASSIGN (x, e)) = curry K.ASSIGN <$> f x <*> exp (f, e)
    | exp (f, K.WHILE (x, e1, e2)) = curry3 K.WHILE <$> f x <*> exp (f, e1) <*> exp (f, e2)
    | exp (f, K.FUNCODE (xs, e)) = curry K.FUNCODE <$> errorList (map f xs) <*> exp (f, e)
    | exp (f, K.CAPTURED i) = K.CAPTURED <$> succeed i
    | exp (f, K.CLOSURE ((xs, e), captured)) = curry_tup K.CLOSURE <$> errorList (map f xs) <*> exp (f, e) <*> errorList (map f xs)
    | exp (f, K.LETREC (closure_names, e)) = raise Impossible.impossible "letrec in knrename"

  fun mapx f = (fn (x) => exp (f, x))
end
