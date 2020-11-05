(* Label elimination: translate assembly code into virtual object code *)

(* You'll complete this file *)

structure Assembler :>
  sig
    val translate : AssemblyCode.instr list -> ObjectCode.instr list Error.error
      (* What can go wrong: An undefined or multiply-defined label *)

  end
  =
struct

  structure A = AssemblyCode
  structure E = Env
  structure O = ObjectCode

  type 'a error = 'a Error.error
  val (succeed, <*>, <$>, >=>) = (Error.succeed, Error.<*>, Error.<$>, Error.>=>)
  infixr 4 <$>
  infix 3  <*>
  infix 2  >=>
  val fail = Error.ERROR

  fun curry f x y = f (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun flip f x y  = f y x
  fun cons x xs = x :: xs

  (* A "translation" that cannot handle any labels.  You will define a better one *)
  fun old_translate instrs =
    let fun cvt (A.OBJECT_CODE instr)       = Error.OK instr
          | cvt (A.LOADFUNC (r, k, instrs)) = curry3 O.LOADFUNC r k <$> old_translate instrs
          | cvt _                           = Error.ERROR "assembler not implemented"
    in  Error.list (map cvt instrs)
    end

  (* In lab, define `fold`, `lift`, `labelEnv`, `labelElim`, and `translate` here *)
  
(* val fold : (AssemblyCode.instr * int * 'c -> 'c) * 'c * AssemblyCode.instr list *)
(* TODO: use size function to determine how many isntructions are taken up *)
(* 
fun fold f a ys = 
  let fun fold_helper i f a [] = a 
  | fold_helper i f a (x::xs) = 
      case (x) of 
            (A.DEFLABEL _)=> fold_helper i f (f (x, i, a)) xs
          | (A.IF_GOTO_LABEL _) => fold_helper (i + 2) f (f (x, i, a)) xs
          | _ => fold_helper (i + 1) f (f (x, i, a)) xs
    in 
      fold_helper 0 f a ys
    end *)

  fun foldhelp func pos acc [] = acc
    | foldhelp func pos acc (x::xs) = 
          case (x) of 
                (A.DEFLABEL _)=> func (x, pos, (foldhelp func pos acc xs))
              | (A.IF_GOTO_LABEL _) => func (x, pos, (foldhelp func (pos + 2) acc xs))
              | _ => func (x, pos, (foldhelp func (pos + 1) acc xs))

  fun fold func acc xs = foldhelp func 0 acc xs          

  (* val lift : ('a * 'b * 'c -> 'c error) -> ('a * 'b * 'c error -> 'c error) *)

  (* lift f (x, y, (OK z)) == f (x, y, z)
  lift f (x, y, (ERROR msg)) ==  ERROR msg *)

  fun lift g = (fn (x, y, z_err) =>  case (z_err) of 
                              (Error.OK z) => g (x, y, z)
                            |  e => e)

  fun labelEnv xs = fold (lift (fn (x, i, a) =>  (case x of 
                                                      (A.DEFLABEL lab) => if (Env.binds (a, lab)) then (fail "label already bound") else (succeed (Env.bind (lab, i, a)))
                                                      | _ => succeed a ))
                          ) (succeed Env.empty) xs


  val labelEnv : AssemblyCode.instr list -> int Env.env error = labelEnv 
  
  (* val labelElim :
  AssemblyCode.instr list -> string Env.env ->
  ObjectCode.instr list Error.error *)

(* 
curry3 O.LOADFUNC r k <$> old_translate instrs

curry3 O.LOADFUNC reg arity

curry3 loadfunc:    'b instr list -> 'b instruction

succeed curry3 loadfunc:  'b instr list error -> 'b instruction error

translate: ('a instr list -> 'b instr list error)

'a -> 'b

'c -> 'a *)

(* TODO: check what i refers to in ifgoto *)
fun labelElim xs env = fold (fn (x, i, a) => 
      (case x of 
              (A.GOTO_LABEL lab) => curry op :: <$> succeed (O.GOTO (Env.find (lab, env) - i)) <*> a
            | (A.IF_GOTO_LABEL (reg, lab)) => curry op @ <$> succeed [O.REGS ("if", [reg]), (O.GOTO (Env.find (lab, env) - i - 1))] <*> a
            | (A.DEFLABEL _) => a
            | (A.OBJECT_CODE y) => curry op :: <$> succeed y <*> a
            | (A.LOADFUNC (reg, arity, instrs)) => curry op :: <$> (curry3 O.LOADFUNC reg arity <$> translate instrs) <*> a)) (succeed [])  xs 

                                  (* curry op :: <$> (O.LOADFUNC (reg, arity, translate instrs)) <$> a *)


(* 
val translate : 
  AssemblyCode.instr list -> ObjectCode.instr list Error.error *)

  and translate xs = (labelEnv >=> labelElim xs) xs 

end


(* val labelElim :
  AssemblyCode.instr list -> string Env.env ->
  ObjectCode.instr list Error.error *)

(* fun labelElim [] env = Error.succeed []
    | labelElim ((A.OBJECT_CODE x) :: xs) env = curry op :: x <$> (labelElim xs env)
    | labelElim ((A.DEFLABEL x) :: xs) env = labelElim xs env
    | labelElim ((A.GOTO_LABEL x) :: xs) env = succeed find (x, env) >=> succeed goto  <      :: (labelElim xs env)
         
         (fn y z => (goto y) :: z) <$> succeed find (x, env) <*> (labelElim xs env)

    
 *)
    




 