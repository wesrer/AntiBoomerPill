(* Closure conversion from unambiguous VScheme to Closed Scheme. 
    This is where we handle lambda and captured variables *)

(* You'll write this file *)

structure ClosureConvert :> sig
  val close : UnambiguousVScheme.def -> ClosedScheme.def
end 
= 
struct
  structure X = UnambiguousVScheme
  structure C = ClosedScheme
  structure S = Set

  fun literal (X.SYM x)   = C.STRING x
    | literal (X.NUM i)   = C.INT i
    | literal (X.BOOLV b) = C.BOOL b
    | literal X.EMPTYLIST = C.EMPTYLIST


  fun indexOf x xs = 
    (* returns `SOME i`, where i is the position of `x` in `xs`,
       or if `x` does not appear in `xs`, returns `NONE` *)
    let fun find k []        = NONE 
          | find k (y :: ys) = if x = y then SOME k else find (k + 1) ys
    in  find 0 xs
    end

  fun closeExp captured e =
    (* Given an expression `e` in Unambiguous vScheme, plus a list
       of the free variables of that expression, return the closure-
       converted version of the expression in Closed Scheme *)
    let val _ = closeExp : X.name list -> X.exp -> C.exp

        (* I recommend internal function closure : X.lambda -> C.closure *)
        fun closure (xs, body) = 
              raise Impossible.exercise "closure-conversion of a `lambda`"
        val _ = closure : X.lambda -> C.closure

        (* I recommend internal function exp : X.exp -> C.exp *)
        fun exp _ = Impossible.exercise "close exp over `captured`"
    in  exp e
    end

  fun free (X.LOCAL n) = S.insert (n, S.empty)
    | free (X.LITERAL v) = S.empty
    | free (X.GLOBAL n) = S.empty
    | free (X.SETGLOBAL (n, e)) = free e
    | free (X.SETLOCAL (n, e)) = S.insert (n, free e)
    | free (X.IFX (e, e1, e2)) = S.union' [free e, free e1, free e2]
    | free (X.WHILEX (e1, e2)) = S.union' [free e1, free e1]
    | free (X.BEGIN es) =  S.union' (map free es)
    | free (X.FUNCALL (e, es)) = S.union' (map free (e::es))
    | free (X.PRIMCALL (p, es)) = S.union' (map free es)
    | free (X.LETX (X.LETREC, bindings, e)) = 
      (* let val (names, exps) = ListPair.unzip bindings
          val free_exps = S.union' (map free exps)
          val name_diff = S.diff (free_exps, S.ofList names)
          val free_body = S.diff (free e, S.ofList names)
      in 
        S.union' ([name_diff, free_body])
      end *)
      
    | free (X.LETX (X.LET, bindings, e)) = 
      let val (names, exps) = ListPair.unzip bindings
          val free_exps = map free exps 
          val free_body = S.diff (free e, S.ofList names)
      in 
        S.union' (free_body :: free_exps)
      end
    | free (X.LAMBDA (names, exp)) = free exp

  val _ = free : X.exp -> X.name S.set

  fun close def = Impossible.exercise "close a definition"

  (* f(e1...en) - (x1...xn) *)

end
