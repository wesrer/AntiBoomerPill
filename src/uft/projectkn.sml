(* Project disambiguated VScheme into KNormal representation. 
    Note that this can fail if the disambiguated VScheme is not already 
    written in KNormal-form. *)

(* You'll complete this file *)

structure ProjectKN :> sig
  val value : UnambiguousVScheme.value -> KNormalForm.literal
  val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error
end 
  = 
struct
  structure K  = KNormalForm
  structure KU = KNormalUtil
  structure P  = Primitive
  structure X  = UnambiguousVScheme

  infix  3 <*>  val op <*> = Error.<*>
  infixr 4 <$>  val op <$> = Error.<$>
  val succeed = Error.succeed
  val error = Error.ERROR
  val errorList = Error.list

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)

  fun checky p = P.name p = "check" orelse P.name p = "expect"

  (* val exp   : UnambiguousVScheme.exp -> string KNormalForm.exp Error.error *)

  val asName : X.exp -> X.name Error.error
         (* project into a name; used where KNF expects a name *)
    = fn X.LOCAL x => succeed x 
       | e => error ("expected a local variable but instead got " ^ (X.whatIs e))

  fun value (X.SYM s) = K.STRING s
    | value (X.NUM n) = K.INT n
    | value (X.BOOLV b) = K.BOOL b
    | value X.EMPTYLIST = K.EMPTYLIST

    (* TODO: return error in whilex *)

  fun exp (X.LITERAL l) = succeed (K.LITERAL (value l))
      | exp (X.LOCAL x) = succeed (K.NAME x)
      | exp (X.GLOBAL x) = succeed (KU.getglobal x)
      | exp (X.IFX (e1, e2, e3)) = curry3 K.IF_EXP <$> asName e1 <*> exp e2 <*> exp e3
      | exp (X.LETX (X.LET, [], e)) = exp e
      | exp (X.LETX (_, [(x, e)], e')) = (curry3 K.LET) x <$> exp e <*> exp e'
      | exp (X.WHILEX (X.LETX (_, [(x, e)], (X.LOCAL y)), body)) = 
                                                          (case (x = y) of 
                                                            true => (curry3 K.WHILE) x <$> exp e <*> exp body
                                                            | false => raise Impossible.impossible "while condition var different")
      | exp (X.BEGIN [e1, e2]) = curry K.SEQ <$> exp e1 <*> exp e2
      | exp (X.SETLOCAL (x, e)) = curry K.ASSIGN x <$> exp e
      | exp (X.SETGLOBAL (x, x')) = curry KU.setglobal x <$> asName x'
      | exp (X.PRIMCALL (p, [e1, X.LITERAL v])) = if checky p then (curry3 K.VMOP_LIT p <$>  errorList (map asName [e1]) <*> succeed (value v)) else (curry K.VMOP p <$> errorList (map asName [e1, X.LITERAL v]))
      | exp (X.PRIMCALL (p, es)) = curry K.VMOP p <$> errorList (map asName es)
      | exp (X.FUNCALL (e, es)) = curry K.FUNCALL <$> asName e <*> errorList (map asName es)
      | exp _ = error "project VScheme exp into KNF"

fun fundef f xs e = K.LET ("t", (K.FUNCODE (xs, e)), KU.setglobal (f, "t"))

  fun def (X.DEFINE (f, (xs, e))) = fundef f xs <$> exp e
      | def (X.EXP e) =  exp e
      | def _ = Impossible.exercise "project VScheme exp into KNF"

end


