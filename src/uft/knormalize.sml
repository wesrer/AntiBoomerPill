(* KNormalizer from First-Order Scheme to KNormal-form Scheme. 
    This is where register allocation happens! *)

(* You'll fill out the missing code in this file *)

structure KNormalize :> sig
  type reg = int  (* register *)
  type exp = reg KNormalForm.exp
  type regset     (* set of registers *)
  val regname : reg -> string
  val exp : reg Env.env -> regset -> ClosedScheme.exp -> reg KNormalForm.exp
  val def :                          ClosedScheme.def -> reg KNormalForm.exp
  val smallest : regset -> reg
  val -- : regset * reg -> regset  
  val bindAnyReg : regset -> exp -> (reg -> exp) -> exp 
  type 'a normalizer = regset -> 'a -> exp
  (* remove a register *) 
end 
  =
struct 
  structure K  = KNormalForm
  structure F  = ClosedScheme
  structure E  = Env
  structure P  = Primitive

  fun curry  f x y   = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun fst (x, y) = x
  fun member x = List.exists (fn y => x = y)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  (************ Register and regset operations ************)

  type reg = int
  fun regname r = "$r" ^ Int.toString r

  datatype regset = RS of int (* RS n represents { r | r >= n } *)

  (************ K-normalization ************)

  type exp = reg K.exp
  type policy = regset -> exp -> (reg -> exp) -> exp
    (* puts the expression in an register, continues *)

  infix 6 --

  fun (RS n) -- r = if r > n then (RS (r + 1)) else (RS (n + 1))

  type 'a normalizer = regset -> 'a -> exp

  fun nbRegsWith normalize p A [] k = k []
    | nbRegsWith normalize p A (e::es) k = 
    p A (normalize A e) (fn t => nbRegsWith normalize p (A -- t) es (fn (ts) => k (t :: ts)))
    
  val nbRegsWith : 'a normalizer -> policy -> regset -> 'a list -> (reg list -> exp) -> exp
    = nbRegsWith

  fun smallest (RS n) = n

  type policy = regset -> exp -> (reg -> exp) -> exp

  fun bindAnyReg rs e f = 
    (case e of
          K.NAME r => f r
          | _ => (K.LET ((smallest rs), e, (f (smallest rs)))))

  fun bindSmallest A e k = K.LET (smallest A, e, k (smallest A))


  and exp rho A e =
    let val exp : reg Env.env -> regset -> ClosedScheme.exp -> exp = exp
        val nbRegs = nbRegsWith (exp rho)   (* normalize and bind in _this_ environment *)
      fun normalize_begin [] = (K.LITERAL (ObjectCode.BOOL false))
        | normalize_begin [e] = exp rho A e
        | normalize_begin (e::es) = K.SEQ (exp rho A e, normalize_begin es)
    in  
        case e of 
          F.LITERAL v => K.LITERAL v
        | F.PRIMCALL (p, es) => nbRegs bindAnyReg A es (fn (ys) => K.VMOP (p, ys))
        | F.FUNCALL (f, formals) => bindSmallest A (exp rho A f) 
                                  (fn r => nbRegs bindSmallest (A -- r) formals  (fn ys => K.FUNCALL (r, ys))) 
        | F.LOCAL name => K.NAME (Env.find (name, rho))
        | F.SETLOCAL (name, x) => K.ASSIGN (Env.find (name, rho), (exp rho A x))
        | F.GLOBAL name =>  K.VMOP_LIT (P.getglobal, [], K.STRING name)
        | F.SETGLOBAL (name, e) => bindAnyReg A (exp rho A e) (fn (x) => K.VMOP_LIT (P.setglobal, [x], K.STRING name))
        | F.BEGIN exps => normalize_begin exps
        | F.IFX (e1, e2, e3) => bindAnyReg A (exp rho A e1) (fn (x) => K.IF_EXP (x, (exp rho A e2), (exp rho A e3)))
        | F.WHILEX (e1, e2) =>  K.WHILE (smallest A, (exp rho A e1), (exp rho A e2))
        | F.LET (es, e1) => let val (names, exps) = ListPair.unzip es
                                fun removeRegisters reg_set [] = reg_set
                                  | removeRegisters reg_set (x::xs) = removeRegisters (reg_set -- x) xs
                                fun bind_rho rs = ListPair.foldl (fn (n, r, env) => Env.bind (n, r, env)) rho (names, rs)
                              in
                                 nbRegs bindAnyReg A exps (fn rs => let val rho' = bind_rho rs 
                                                            in
                                                              exp rho' (removeRegisters A rs) e1
                                                            end)
                              end    
        (* | F.CLOSURE (lambda, captured) => Impossible.exercise "CLOSURE"
        | F.LETREC (bindings, body) => Impossible.exercise "LETREC"
        | F.CAPTURED i => Impossible.exercise "CAPTURED" *)
    end

  fun helper e p v reg_set env = bindAnyReg reg_set (exp env reg_set e) (fn (x) => K.VMOP_LIT (p, [x], v))

     val rec printList = fn
             nil => () |
             x::xs => (
                 print(Int.toString(x));
                 print("\n"); 
                 printList(xs)
             );

  fun def e = 
      let val env = Env.empty
        val reg_set = (RS 0)
        fun get_consecutive_regs c n = if (c = 0) then [] else c :: (get_consecutive_regs (c - 1) n)

      in 
        case e of
          F.EXP x => exp env reg_set x
        | F.CHECK_EXPECT (s1, e1, s2, e2) => K.SEQ ((helper e1 P.check (ObjectCode.STRING s1) reg_set env), (helper e2 P.expect (ObjectCode.STRING s2) reg_set env))
        | F.CHECK_ASSERT (s, x) => (helper x P.check_assert (ObjectCode.STRING s) reg_set env)
        | F.VAL (name, x) => exp env reg_set (F.SETGLOBAL (name, x))
        | F.DEFINE (fun_name, (formals, x)) =>
          let 
            val (env', reg_set') = List.foldl (fn (x, (bindings, A)) => (Env.bind (x, smallest A, bindings), A -- (smallest A)) ) (Env.bind (fun_name, 0, env), RS 1) formals
            val consec_regs = List.map (fn n => Env.find (n, env')) formals
            val funcode = K.FUNCODE (consec_regs, exp env' reg_set' x)
            val let_exp = K.LET (0, funcode, KNormalUtil.setglobal (fun_name, 0))
          in 
            let_exp
          end
      end 

end

            (* val (_, range_list, fold_env) = fold_acc (fun_name, env, formals) *)

