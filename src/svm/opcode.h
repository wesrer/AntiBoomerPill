// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented 
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

typedef enum opcode { 
                      Halt, // R0
                      Print, // R1 
                      If,
                      Mov,
                     LoadLiteral, // R1LIT
                      Check, Expect, // R1LIT
                      CheckAssert,
                      Unimp, // stand-in for opcodes not yet implemented
                      Not,
                      Add,
                      Subtract,
                      Multiply,
                      IDiv,
                      FloatDiv,
                      Zero,
                      SetGlobal,
                      GetGlobal,
                      MakeConsCell,
                      ProjectBool,
                      GoTo,
                      TailCall,
                      Function_Observer,   
                      Pair_Observer,
                      Symbol_Observer,
                      Number_Observer,               
                      Boolean_Observer,             
                      Null_Observer,            
                      Nil_Observer,
                      Cdr,
                      Car,
                      MkClosure,
                      GetClSlot,
                      SetClSlot,
                      Cons,
                      Hash,
                      Equal,
                      LessThan,
                      GreaterThan,
                      Error,
                      Printu,
                      Println,
                      Call,
                      Return,
                      GC,
                      And,
                      Or
} Opcode;

int isgetglobal(Opcode code); // update this for your SVM, in instructions.c

#endif
