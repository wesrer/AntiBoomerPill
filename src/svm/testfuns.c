// Builds a few functions for testing.  See description in testfuns.h

// Module 2: These functions could be used as a model for code 
// to run in your loader.

#include "testfuns.h"

#include "iformat.h"
#include "opcode.h"
#include "value.h"
#include "vmheap.h"
#include "vmsizes.h"
#include "vmstate.h"

static struct VMFunction *newfunction(int instruction_count) {
  // play it safe: always tack a Halt instruction into the end
  VMNEW(struct VMFunction *, fun, vmsize_fun(instruction_count + 1));
  fun->arity = 0;
  fun->size  = instruction_count + 1;
  fun->instructions[instruction_count] = eR0(Halt);
  return fun;
}

struct VMFunction *unimp_opcodefun(void) {
  struct VMFunction *fun = newfunction(1);
  fun->instructions[0] = eR1(Unimp, 0);
  return fun;
}


struct VMFunction *haltfun(void) {
  return newfunction(0); // nothing but the Halt instruction
}

struct VMFunction *print0fun(void) {
  struct VMFunction *fun = newfunction(1);
  fun->instructions[0] = eR1(Print, 0);
  return fun;
}

struct VMFunction *ce0fun(VMState vm) {
  VMString r0string = Vmstring_newc("$r0");
  int stringslot = literal_slot(vm, mkStringValue(r0string));
  struct VMFunction *fun = newfunction(2);
  fun->instructions[0] = eR1U16(Check, 0, stringslot);
  fun->instructions[1] = eR1U16(Expect, 0, stringslot);
  return fun;
}

struct VMFunction *nottruefun(VMState vm) {

  struct VMFunction *fun = newfunction(4);
  int boolslot = literal_slot(vm, mkBooleanValue(true));
  fun->instructions[0] = eR1U16(LoadLiteral, 0, boolslot);
  fun->instructions[1] = eR1(Print, 0);
  fun->instructions[2] = eR1(Not, 0);
  fun->instructions[3] = eR1(Print, 0);
  return fun;
}

struct VMFunction *not0fun(VMState vm) {
  //should throw a typeerror
  struct VMFunction *fun = newfunction(4);
  Number_T zero = 0.0;
  int boolslot = literal_slot(vm, mkNumberValue(zero));
  fun->instructions[0] = eR1U16(LoadLiteral, 0, boolslot);
  fun->instructions[1] = eR1(Print, 0);
  fun->instructions[2] = eR1(Not, 0);
  fun->instructions[3] = eR1(Print, 0);
  return fun;
}


struct VMFunction *mov8fun(VMState vm) {
  Number_T eight = 8.0;
  int numberslot = literal_slot(vm, mkNumberValue(eight));
  struct VMFunction *fun = newfunction(2);
  fun->instructions[0] = eR1U16(LoadLiteral, 0, numberslot);
  fun->instructions[1] = eR1(Print, 0);
  return fun;
}

struct VMFunction *add78fun(VMState vm) {

  struct VMFunction *fun = newfunction(4);
  Number_T seven = 7.0;
  Number_T eight = 8.0;
  int num1slot = literal_slot(vm, mkNumberValue(seven));
  int num2slot = literal_slot(vm, mkNumberValue(eight));
  fun->instructions[0] = eR1U16(LoadLiteral, 0, num1slot);
  fun->instructions[1] = eR1U16(LoadLiteral, 1, num2slot);
  fun->instructions[2] = eR3(Add, 2, 0, 1);
  fun->instructions[3] = eR1(Print, 2);
  return fun;
}