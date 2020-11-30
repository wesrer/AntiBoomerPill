// List of all opcodes, parsers, and unparsers

// You'll develop this list from module 2 onward.  Eve$ry time
// you add a new instruction, you'll add an ent$ry here.
// You'll also define the opcode in file opcodes.h,
// and you'll add a case to your `vmrun` function.

#include "iformat.h"
#include "name.h"
#include "itable.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

instruction_info instructions[] = {
  { "if", If, parseR1, "if $rX" },
  { "halt", Halt, parseR0, "halt" },
  { "gc", GC, parseR0, "gc" },
  { "print", Print, parseR1, "print $rX" },
  { "loadliteral", LoadLiteral, parseR1LIT, "$rX := LIT" },
  { "check", Check, parseR1LIT, "check LIT, $rX" },
  { "call", Call, parseR3, "rX := call rY (rY+1, ..., rZ)"},
  { "tailcall", TailCall, parseR2, "tailcall rX (rX+1, ..., rY)"},
  { "check-assert", CheckAssert, parseR1LIT, "check-assert LIT, $rX" },
  { "expect", Expect, parseR1LIT, "expect LIT, $rX" },
  { "+", Add, parseR3, "$rX := $rY + $rZ" },
  { "-", Subtract, parseR3, "$rX := $rY - $rZ" },
  { "/", IDiv, parseR3, "$rX := $rY / $rZ" },
  { "//", FloatDiv, parseR3, "$rX := $rY // $rZ" },
  { "*", Multiply, parseR3, "$rX := $rY * $rZ" },
  { "not", Not, parseR1, "not $rX"},
  { "zero", Zero, parseR1, "zero $rX"},
  { "setglobal", SetGlobal, parseR1LIT, "globals[LIT] := $rX" },
  { "getglobal", GetGlobal, parseR1LIT, "getglobal $rX LIT" },
  { "makeconscell", MakeConsCell, parseR1, "makeconscell $rX"},
  { "projectbool", ProjectBool, parseR1, "projectbool $rX"},
  { "goto", GoTo, parseR0I24, "goto LIT"},

  { "function?", Function_Observer, parseR2, "$rX := function? $rY" },
  { "pair?", Pair_Observer, parseR2, "$rX := pair? $rY" },
  { "symbol?", Symbol_Observer, parseR2, "$rX := symbol? $rY" },
  { "number?", Number_Observer, parseR2, "$rX := number? $rY" },
  { "mov", Mov, parseR2, "$rX := $rY" },

  { "boolean?", Boolean_Observer, parseR2, "$rX := boolean? $rY" },
  { "null?", Null_Observer, parseR2, "$rX := null? $rY" },
  { "nil?", Nil_Observer, parseR2, "$rX := nil? $rY" },
  { "cdr", Cdr, parseR2, "$rX := cdr $rY" },
  { "car", Car, parseR2, "$rX := car $rY" },
  { "hash", Hash, parseR2, "$rX := hash $rY" },

  { "cons", Cons, parseR3, "$rX := $rY cons $rZ" },
  { "=", Equal, parseR3, "$rX := $rY = $rZ" },
  { ">", GreaterThan, parseR3, "$rX := $rY > $rZ" },
  { "<", LessThan, parseR3, "$rX := $rY < $rZ" },

  {"mkclosure", MkClosure, parseR2U8, "$rX := closure[$rY,Z]"},
  {"getclslot", GetClSlot, parseR2U8, "$rX := $rY.Z"},
  {"setclslot", SetClSlot, parseR2U8, "$rX.Z := $rY"},

  {"return", Return, parseR1, "return rX"},

  { "error", Error, parseR1, "error $rX" },
  { "printu", Printu, parseR1, "printu $rX" },
  { "println", Println, parseR1, "println $rX" },


};

int isgetglobal(Opcode code) {
  return code == GetGlobal; // change this for your SVM
}

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);
