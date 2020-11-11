(* Generates abstract assembly code from KNormal-form VScheme *)

(* You'll write this file *)

structure Codegen
  :>
sig 
  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr
  val forEffect : reg KNormalForm.exp -> instruction list
end
  =
struct
  structure A = AsmGen
  structure K = KNormalForm
  structure P = Primitive

  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr

  (********* Join lists, John Hughes (1986) style *********)

  type 'a hughes_list = 'a list -> 'a list
    (* append these lists using `o` *)

  (* don't look at these implementations; look at the types below! *)
  fun empty tail = tail
  fun S e  tail = e :: tail
  fun L es tail = es @ tail

  val _ = empty : 'a hughes_list
  val _ = S     : 'a      -> 'a hughes_list   (* singleton *)
  val _ = L     : 'a list -> 'a hughes_list   (* conversion *)

  val hconcat : 'a hughes_list list -> 'a hughes_list
    = fn xs => foldr op o empty xs

  (************** the code generator ******************)

  (* three contexts for code generation: to put into a register,
     to run for side effect, or (in module 8) to return. *) 

  fun toReg' (dest : reg) (e : reg KNormalForm.exp) : instruction hughes_list =
        (case e of K.ASSIGN (x, e) => (toReg' x e) o (S (A.copyreg dest x))
                 | K.NAME x => S (A.copyreg dest x)
                 | K.LITERAL lit => S (A.loadlit dest lit)
                 | K.VMOP (P.HAS_EFFECT b, ns) => S (A.setreg dest (P.HAS_EFFECT b) ns)
                 | K.VMOP (vmop, ns) => S (A.setreg dest vmop ns)
                 | K.VMOP_LIT (P.HAS_EFFECT b, ns, lit) => S (A.effectLit (P.HAS_EFFECT b) ns lit)
                 | K.VMOP_LIT (vmop, ns, lit) => S (A.setregLit dest vmop ns lit)
                 | K.IF_EXP (x, e1, e2) => (let val L = A.newlabel ();
                                                val L' = A.newlabel ();
                                              in
                                               (S (A.ifgoto x L)) o (toReg' dest e2) o (S (A.goto L')) o (S (A.deflabel L)) o (toReg' dest e1) o (S (A.deflabel L'))
                                              end)
                 | K.SEQ (e1, e2) => (forEffect' e1) o (toReg' dest e2)
                 | K.LET (n, e1, e2) => (toReg' n e1) o (toReg' dest e2)
                 | K.WHILE (n, e1, e2) => (forEffect' (K.WHILE (n, e1, e2))) o (S (A.loadlit dest (ObjectCode.BOOL false)))  
                 | K.FUNCALL (funreg, (x::xs)) => 
                    (if (A.areConsecutive (x::xs)) andalso (x = funreg + 1) 
                    then S (A.call dest funreg (List.last (x::xs))) 
                    else (raise Impossible.impossible "registers in funcall not consecutive"))
                 | K.FUNCALL (funreg, []) => S (A.call dest funreg funreg)
                 | K.FUNCODE (xs, e) => S (A.loadfunc dest (List.length xs) ((return e) [])))

  and forEffect' (e: reg KNormalForm.exp) : instruction hughes_list =
        (case e of K.VMOP (vmop, ns) => (case vmop of 
                                            P.HAS_EFFECT b => S (A.effect vmop ns)
                                           | _ => empty)
                 | K.NAME _ => empty
                 | K.LITERAL _ => empty
                 | K.FUNCODE _ => empty
                 | K.VMOP_LIT (vmop, ns, lit) => (case vmop of
                                                   P.HAS_EFFECT b => S (A.effectLit vmop ns lit)
                                                   | _ => empty)
                 | K.IF_EXP (x, e1, e2) => (let val L = A.newlabel ();
                                                val L' = A.newlabel ();
                                              in
                                               (S (A.ifgoto x L)) o (forEffect' e2) o (S (A.goto L')) o (S (A.deflabel L)) o (forEffect' e1) o (S (A.deflabel L'))
                                              end)
                 | K.ASSIGN (x, e) => (toReg' x e)
                 | K.SEQ (e1, e2) => (forEffect' e1) o (forEffect' e2)
                 | K.LET (n, e1, e2) => (toReg' n e1) o (forEffect' e2)
                 | K.WHILE (x, e1, e2) => (let val L = A.newlabel ()
                                                val L' = A.newlabel ()
                                              in
                                               (S (A.goto L)) o
                                                (S (A.deflabel L')) o 
                                                (forEffect' e2) o 
                                                (S (A.deflabel L)) o 
                                                (toReg' x e1) o 
                                                (S (A.ifgoto x L')) 
                                              end) 
                  | K.FUNCALL (funreg, (x::xs)) => 
                    if (A.areConsecutive (x::xs)) andalso (x = funreg + 1) 
                    then S (A.call x funreg (List.last (x::xs))) 
                    else raise Impossible.impossible "registers in funcall not consecutive"
                 | K.FUNCALL (funreg, []) => S (A.call funreg funreg funreg))

  and return (e : reg KNormalForm.exp) : instruction hughes_list =
      (case e of 
                K.NAME x => S (A.return x)
                | K.FUNCALL (funreg, (x::xs)) => 
                  if (A.areConsecutive (x::xs)) andalso (x = funreg + 1) 
                  then (S (A.tailcall funreg (List.last (x::xs))))
                  else (raise Impossible.impossible "registers in tailcall not consecutive")
                | K.IF_EXP (x, e1, e2) => (let val L = A.newlabel ();
                                            in
                                              (S (A.ifgoto x L)) o (return e2) o (S (A.deflabel L)) o (return e1)
                                            end)
                | K.SEQ (e1, e2) => (forEffect' e1) o (return e2)
                | K.LET (n, e1, e2) => (toReg' n e1) o (return e2)
                | x => (toReg' 0 x) o (S (A.return 0)))

  val _ = forEffect' :        reg KNormalForm.exp -> instruction hughes_list
  val _ = toReg'     : reg -> reg KNormalForm.exp -> instruction hughes_list
  val _ = return     : reg KNormalForm.exp -> instruction hughes_list

  fun forEffect e = forEffect' e []

end
