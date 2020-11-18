(* Embeds KNormal-form Scheme into VScheme. This cannot fail. *)

(* You'll complete this file *)

structure EmbedKN :> sig
  val value : KNormalForm.literal -> VScheme.value
  val def   : VScheme.name KNormalForm.exp -> VScheme.def
end 
  = 
struct
  structure K  = KNormalForm
  structure S  = VScheme
  structure SU = VSchemeUtils
  structure P  = Primitive

  fun let' x e' e = S.LETX (S.LET, [(x, e')], e)   (* useful helper *)

  fun value (K.REAL r) = S.NUM (Real.round r)
      | value (K.STRING s) = S.SYM s
      | value (K.BOOL b) = S.BOOLV b
      | value (K.NIL) = S.BOOLV false
      | value (K.EMPTYLIST) = S.EMPTYLIST
      | value (K.INT n) = S.NUM n


  (* NOTE: error handling here? *)

  fun nameFrom (K.STRING s) = s
     | nameFrom _ = raise Impossible.impossible "setglobal or getglobal used with non-string literal"

  fun exp (K.LITERAL v) = S.LITERAL (value v)
    | exp (K.NAME x) = S.VAR x
    | exp (K.VMOP (vmop, xs)) = S.APPLY (S.VAR (P.name vmop), map S.VAR xs)
    | exp (K.VMOP_LIT (vmop, [], v)) =  
          (case (P.name vmop) of 
            "getglobal" => S.VAR (nameFrom v)
            | _ => S.APPLY (S.VAR (P.name vmop), [S.LITERAL (value v)]))                                
    | exp (K.VMOP_LIT (vmop, r::rs, v)) = 
        (case (P.name vmop) of 
            "setglobal" => S.SET (nameFrom v, S.VAR r)
            | name => S.APPLY (S.VAR name, (map S.VAR (r::rs))@ [S.LITERAL (value v)]))
    | exp (K.SEQ (e1, e2)) =  S.BEGIN (exp e1 :: [exp e2])  
    | exp (K.ASSIGN (x, e)) = S.SET (x, exp e)
    | exp (K.FUNCODE (xs, e)) = S.LAMBDA (xs, exp e)
    | exp (K.FUNCALL (x, xs)) = S.APPLY ((S.VAR x), map S.VAR xs)                                  
    | exp (K.IF_EXP (reg, e1, e2)) = S.IFX ((S.VAR reg), exp e1, exp e2)
    | exp (K.CAPTURED i) = S.APPLY (S.VAR "CAPTURED-IN", [S.LITERAL (S.NUM i), S.VAR "$closure"])
    | exp (K.CLOSURE ((formals, body), captured)) =         
        let val namelist = "$closure" :: formals
            val lam = S.LAMBDA (namelist,  exp body)
        in
            SU.cons lam (SU.list (map S.VAR captured))
        end
    (* | exp (K.LETREC (bindings, e)) =  let val (n::names, c::closures) = ListPair.unzip bindings
                                                      in 
                                                          S.LETX (S.LETREC, (n, exp c) :: (exp K.LETREC (names, closures)), exp e)   
                                                      end     *)
    | exp (K.WHILE (x, e1, e2)) = S.WHILEX (let' x (exp e1) (S.VAR x), exp e2)
    | exp (K.LET (x, e, K.NAME p)) = (case (x = p) of
                                      true => exp e 
                                      | false => let' x (exp e) (exp (K.NAME p)))
    | exp (K.LET (reg, e1, e2)) = let' reg (exp e1) (exp e2)


   fun def knf_e = S.EXP (exp knf_e)

end
