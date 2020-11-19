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
(* (define o (f g) (list3 (lambda ($closure x) ((CAPTURED-IN 0 $closure) ((CAPTURED-IN 1 $closure) x)))
                        f
                        g))

  (define o (f g) (lambda (x) (f (g x))))
*)

  fun indexOf x xs =
    (* returns `SOME i`, where i is the position of `x` in `xs`,
       or if `x` does not appear in `xs`, returns `NONE` *)
    let fun find k []        = NONE
          | find k (y :: ys) = if x = y then SOME k else find (k + 1) ys
    in  find 0 xs
    end

  fun unLambda (X.LAMBDA lambda) = lambda
    | unLambda _ = Impossible.impossible "parser failed to insist on a lambda"

  and closeExp captured e =
    (* Given an expression `e` in Unambiguous vScheme, plus a list
       of the free variables of that expression, return the closure-
       converted version of the expression in Closed Scheme *)
    let val _ = closeExp : X.name list -> X.exp -> C.exp

        (* I recommend internal function closure : X.lambda -> C.closure *)
        fun closure (xs, body) =
          let
            val free_names = S.diff (free body, S.ofList xs)
            val free_names_list = S.elems (free_names)
            (* val _ = app print ["Closure converting ",
                      WppScheme.expString (VScheme.LAMBDA (xs, VScheme.VAR "...")),
                      " with free names ", String.concatWith " " free_names_list, "\n"] *)
            val capturedExps = closeExp captured
            val free_expressions = map (capturedExps o X.LOCAL) free_names_list

          in
            ((xs, closeExp free_names_list body), free_expressions)
          end

        val _ = closure : X.lambda -> C.closure

        (* I recommend internal function exp : X.exp -> C.exp *)
        fun exp (X.LITERAL v) = C.LITERAL (literal v)
          | exp (X.LOCAL n) =
            (case (indexOf n captured) of
                NONE => C.LOCAL n
              | SOME i => C.CAPTURED i)
          | exp (X.GLOBAL n) = C.GLOBAL n
          | exp (X.IFX (e1, e2, e3)) = C.IFX (exp e1, exp e2, exp e3)
          | exp (X.WHILEX (e1, e2)) = C.WHILEX (exp e1, exp e2)
          | exp (X.BEGIN es) = C.BEGIN (map exp es)
          | exp (X.SETLOCAL (n, e)) =
            (case (indexOf n captured) of
                NONE => C.SETLOCAL(n, exp e)
              | SOME i => Impossible.impossible "Attemping to write a captured variable. Stop.")
          | exp (X.SETGLOBAL (n, e)) = C.SETGLOBAL (n, exp e)
          | exp (X.FUNCALL (e, es)) = C.FUNCALL (exp e, map exp es)
          | exp (X.PRIMCALL (p, es)) = C.PRIMCALL (p, map exp es)
          | exp (X.LAMBDA (xs, e)) = C.CLOSURE (closure (xs, e))
          | exp (X.LETX (X.LET, bindings, e)) =
                    let val (names, exps) = ListPair.unzip bindings
                    in
                      C.LET (ListPair.zip (names, map exp exps), exp e)
                    end
          | exp (X.LETX (X.LETREC, bindings, e)) = C.LETREC ((map (fn (n, l) => (n, closure(unLambda l))) bindings), exp e)
        in  exp e
    end

  and free (X.LOCAL n) = S.insert (n, S.empty)
    | free (X.LITERAL v) = S.empty
    | free (X.GLOBAL n) = S.empty
    | free (X.SETGLOBAL (n, e)) = free e
    | free (X.SETLOCAL (n, e)) = S.insert (n, free e)
    | free (X.IFX (e, e1, e2)) = S.union' [free e, free e1, free e2]
    | free (X.WHILEX (e1, e2)) = S.union' [free e1, free e1]
    | free (X.BEGIN es) =  S.union' (map free es)
    | free (X.FUNCALL (e, es)) = S.union' (map free (e::es))
    | free (X.PRIMCALL (p, es)) = S.union' (map free es)
    | free (X.LETX (X.LET, bindings, e)) =
      let val (names, exps) = ListPair.unzip bindings
          val free_exps = map free exps
          val free_body = S.diff (free e, S.ofList names)
      in
        S.union' (free_body :: free_exps)
      end
    | free (X.LETX (X.LETREC, bindings, e)) =
      let val (names, exps) = ListPair.unzip bindings
          val free_exps = map free exps
          val name_diff = S.diff (S.union' free_exps, S.ofList names)
          val free_body = S.diff (free e, S.ofList names)
      in
        S.union' [free_body, name_diff]
      end
    | free (X.LAMBDA (names, exp)) = S.union' [S.diff (free exp, S.ofList names)]

  val _ = free : X.exp -> X.name S.set

  fun close (X.VAL (n, e)) = C.VAL (n, closeExp [] e)
    | close (X.EXP e) = C.EXP (closeExp [] e)
    | close (X.DEFINE (n, (xs, e))) = C.DEFINE (n, (xs, closeExp [] e))
    | close (X.CHECK_EXPECT (s1, e1, s2, e2)) = C.CHECK_EXPECT (s1, closeExp [] e1, s2, closeExp [] e2)
    | close (X.CHECK_ASSERT (s, e)) = C.CHECK_ASSERT (s, closeExp [] e)

end
