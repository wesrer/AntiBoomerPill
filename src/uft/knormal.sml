(* Representation of KNormal-form, with some utility functions *)

(* You'll define the representation of `'a exp`, and you'll write
   the utility functions. *)

structure KNormalForm = struct
  
  datatype literal = datatype ObjectCode.literal

  type vmop = Primitive.primitive

  datatype 'a exp      (* type parameter 'a is a _name_, typically
                          instantiated as `string` or `ObjectCode.reg` *)
    = LITERAL of literal
    | NAME of 'a
    | VMOP of vmop * 'a list
    | VMOP_LIT of vmop * 'a list * literal
    | FUNCALL of 'a * 'a list
    | IF_EXP of 'a * 'a exp * 'a exp
    | LET of 'a * 'a exp * 'a exp
    | SEQ of 'a exp * 'a exp
    | ASSIGN of 'a * 'a exp
    | WHILE of 'a * 'a exp * 'a exp
    | FUNCODE of 'a list * 'a exp
end

structure KNormalUtil :> sig
  type name = string
  val setglobal : name * 'a -> 'a KNormalForm.exp
  val getglobal : name -> 'a KNormalForm.exp

   (* create these @(x,...x, v) forms:
         setglobal(name-of-global, register)
         getglobal(name-of-global)
    *)

  (* you could consider adding similar functions for `check`, `expect`,
     and `check-assert` *)
end
  =
struct
  structure K = KNormalForm
  type name = string

  fun setglobal (x, register) = K.VMOP_LIT (Primitive.setglobal, [register], ObjectCode.STRING x)
  fun getglobal x             = K.VMOP_LIT (Primitive.getglobal, [], ObjectCode.STRING x)

end
