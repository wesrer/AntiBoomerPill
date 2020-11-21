(* Properties of primitives (SVM instructions) *)

(* In module 6, you'll need to extend the implementation to
   account for any exciting SVM instructions you may have defined.
   You'll see here NR's allergy to long stretches of repetitive code.
 *)

structure Primitive :> sig
  (* A primitive corresponds to an SVM instruction *)

  type base
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

    (* For SETS_REGISTER, the instruction takes the destination register
       as the first operand, and the actual parameters as remaining operands.

       For HAS_EFFECT, the instruction has no destination register;
       the operands are the arguments.
       
       A SETS_REGISTER primitive _must not_ have a side effect,
       because if one appears in an effectful context, it is discarded.
     *)

  (* these are the primitives that are available to source code *)
  val find : string -> primitive option  (* intended only for primitives that
                                            appear in source code; internal
                                            compiler primitives should be named
                                            using the values below *)
  val exposedNames : string list


  (* observers for properties of primitives *)
  val name  : primitive -> string   (* used in assembly code & object code *)
  val arity : primitive -> int      (* used to make LAMBDAs on demand *)
  val throwsError : primitive -> bool
     (* primitive does not return, so is OK to use in a value context *)

  (* these are the primitives that are used inside the compiler *)
  val cons         : primitive   (* for building quoted lists *)
  val loadliteral  : primitive
  val setglobal    : primitive
  val getglobal    : primitive
  val check        : primitive   (* for converting check-expect to K-normal form *)
  val expect       : primitive   (* for converting check-expect to K-normal form *)
  val check_assert : primitive


end
  =
struct

  (* Pure, register-setting primitives grouped by arity.  You can extend these lists *)
  (* TODO: change so zero is a binary op *)

  val binary  = [ "+", "-", "*", "/", "<", ">", "cons", "=", "idiv", "zero" ]
  val unary   = [ "boolean?", "null?", "number?", "pair?", "function?", "nil?",
                "symbol?", "car", "cdr", "hash"]


  (* Three different groups of side-effecting primitives.  To the compiler,
     `error` looks a lot like `print`, but only `error` throws an error,
     so I feel compelled to separate them. *)

  val side_effecting = [ "print", "printu", "println" ]
  val error          = [ "error" ]
  val checky = [ "check", "expect" ]  (* arity 2 *)



  (* Representation of a primitive, with observers *)

  type base = { name : string, arity : int }
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

  fun base (SETS_REGISTER b) = b
    | base (HAS_EFFECT    b) = b

  val name  = #name  o base
  val arity = #arity o base

  fun throwsError p = #name (base p) = "error"

  (* building and using the list of primitives *)

  fun add arity ty names prims =
    foldl (fn (name, prims) => ty { name = name, arity = arity } :: prims)
          prims names

  val primitives : primitive list =  (* you can also extend this definition *)
    ( add 2 SETS_REGISTER binary
    o add 1 SETS_REGISTER unary
    o add 1 HAS_EFFECT side_effecting
    o add 1 HAS_EFFECT error
    o add 2 HAS_EFFECT checky
    ) []

  val exposedNames = map name primitives
  
  fun find x = List.find (fn p => name p = x) primitives


  (* Primitives used internally *)

  val cons         = SETS_REGISTER { name = "cons",         arity = 2 }
  val setglobal    = HAS_EFFECT    { name = "setglobal",    arity = 2 }
  val getglobal    = SETS_REGISTER { name = "getglobal",    arity = 1 }
  val check        = HAS_EFFECT    { name = "check",        arity = 2 }
  val expect       = HAS_EFFECT    { name = "expect",       arity = 2 }
  val check_assert = HAS_EFFECT    { name = "check-assert", arity = 2 }
  val loadliteral  = SETS_REGISTER { name = "loadliteral",  arity = 1 }
  val plus         = SETS_REGISTER { name = "+",            arity = 2 }
  val subtract     = SETS_REGISTER { name = "-",            arity = 2 }
  val divide        = SETS_REGISTER { name = "/",            arity = 2 }
  val multiply     = SETS_REGISTER { name = "*",            arity = 2 }
  val not         = SETS_REGISTER { name = "!",            arity = 2 }
  val makeconscell = SETS_REGISTER { name = "makeconscell",  arity = 1 }
  val projectbool = SETS_REGISTER { name = "projectbool",  arity = 1 }
  val function_observer = SETS_REGISTER { name = "makeconscell",  arity = 1 }
  val boolean_observer = SETS_REGISTER { name = "boolean?",  arity = 1 }

  val pair_observer = SETS_REGISTER { name = "pair?",  arity = 1 }
  val symbol_observer = SETS_REGISTER { name = "symbol?",  arity = 1 }
  val number_observer = SETS_REGISTER { name = "number?",  arity = 1 }
  val mov = SETS_REGISTER { name = "mov",  arity = 2 }
  val hash = SETS_REGISTER { name = "hash",  arity = 2 }
  val gc = HAS_EFFECT { name = "gc",  arity = 0 }
end



