// Heart of the VM: runs instructions until told to halt

// You'll write a small `vmrun` function in module 1.  You'll pay
// some attention to performance, but you'll implement only a few 
// instructions.  You'll add other instructions as needed in future modules.

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

#include "check-expect.h"
#include "iformat.h"
#include "value.h"
#include "vmstate.h"
#include "vmrun.h"

#include "print.h"

#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"
#include "vtable.h"
#include "svmdebug.h"
#include "disasm.h"


void vmrun(VMState vm, struct VMFunction *fun) {
  //cached instruction pointer
  int cip = 0;
  //cached register ptr
  const char *dump_decode = svmdebug_value("decode");
  const char *dump_call   = svmdebug_value("call");
  (void) dump_call;  // make it OK not to use `dump_call`
  fun = fun;

  while (true) {
    Value* regs = vm->registers + vm->window;
    Instruction i = fun->instructions[cip];
    Value RX = regs[uX(i)];
    Value RY = regs[uY(i)];
    Value RZ = regs[uZ(i)];
    // vm->highest_reg = fun->nregs + vm->window > vm->highest_reg ? uX(i) vm->window : vm->highest_reg;
    if (dump_decode)
      idump(stderr, vm, cip, i, vm->window, &RX, &RY, &RZ);

    switch(opcode(i)) {
      case If:
      {
        bool b = AS_BOOLEAN(vm, regs[uX(i)]);
          if (!b)
          {
            cip ++;
          }
          break;

      }
      case GoTo:
        {        
          cip += iXYZ(i);
          continue;
        }
      case Print:
        {
          Value v = regs[uX(i)];
          print("%v\n", v);        
        break; 
        }
      case Check:
        {
        Value source = regs[uX(i)];
        Value v = vm->literal_pool[uYZ(i)];
        check(AS_CSTRING(vm, v),  source);
        break;
        }
      case Expect:
        {
        Value source = regs[uX(i)];
        Value v = vm->literal_pool[uYZ(i)];
        expect(AS_CSTRING(vm, v),  source);
        break;
        }
      case Not:
      {
        bool b = AS_BOOLEAN(vm, regs[uX(i)]);
        regs[uX(i)] = mkBooleanValue(!b);
        break;
      }
      case LoadLiteral:
      {
        Value v = vm->literal_pool[uYZ(i)];
        // print("in loadlit: %v\n", v);
        regs[uX(i)] = v;
        break;
      }
      case Add:
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        Value v = mkNumberValue(num1 + num2);
        regs[uX(i)] = v;
        break;
      }
      case Equal:
      {
        bool b = eqvalue(regs[uY(i)], regs[uZ(i)]);
        regs[uX(i)] = mkBooleanValue(b);
        break;
      }      
      case Zero: 
      {
        regs[uX(i)] = mkNumberValue(0);
        break;
      }
      case Subtract: 
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        Value v = mkNumberValue(num1 - num2);
        regs[uX(i)] = v;
        break;
      }
      case Divide: 
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        Value v = mkNumberValue(num1 / num2);
        regs[uX(i)] = v;
        break;
      }
      case Multiply: 
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        Value v = mkNumberValue(num1 * num2);
        regs[uX(i)] = v;
        break;
      }
      case SetGlobal: 
      {
        Value v = vm->literal_pool[uYZ(i)];
        VTable_put(vm->globals, v, regs[uX(i)]);
        break;
      }
      case GetGlobal: 
      {
        Value v = VTable_get(vm->globals, vm->literal_pool[uYZ(i)]);
        regs[uX(i)] = v;
        break;
      }
      case MakeConsCell: 
      { 
        VMNEW(struct VMBlock *, bl, sizeof(*bl) + sizeof(bl->slots[0]));
        bl->nslots = 1;
        bl->slots[0] = regs[uX(i)];
        regs[uX(i)] = mkConsValue(bl);
        break;
      }
      case ProjectBool: 
      { 
        bool b = AS_BOOLEAN(vm, regs[uX(i)]);
        regs[uX(i)] = mkBooleanValue(b);
        break;
      }
      //TODO: check if there are right number of arguments
      case Call:
      {
        int lastarg = uZ(i);
        int funreg = uY(i);
        int destreg = uX(i);
        struct Activation a;
        a.start_window = funreg;
        a.end_window = lastarg;
        a.dest_reg = destreg;
        a.fun = fun;
        a.pc = cip;
        if (vm->callstack_size >= vm->callstack_length) {
          runerror(vm, "Stack overflow!");
        }
        vm->callstack[vm->callstack_size] = a;
        int n = lastarg - funreg;
        vm->callstack_size++; 
        vm->window += funreg;
        Value callee = regs[funreg];
        if (callee.tag == VMFunction)
         fun = callee.f;
        else if (callee.tag == VMClosure)
         fun = callee.hof->f;
        // fun = AS_VMFUNCTION(vm, regs[funreg]);
      
        if (n > fun->arity) 
          runerror(vm, "Function arity and arguments mismatched ");
        if (fun->nregs >= 255)
          runerror(vm, "Register file overflowed");
        cip = 0;
        continue;
      }
      //TODO: check if there are right number of arguments
      // case Call:
      // {
      //   int lastarg = uZ(i);
      //   int funreg = uY(i);
      //   int destreg = uX(i);
      //   struct Activation a;

      //   a.start_window = funreg;
      //   a.end_window = lastarg;
      //   a.dest_reg = destreg;

      //   Value callee = regs[funreg];
      //   if (callee.tag == VMFunction)
      //    a.fun = callee.f;
      //   else if (callee.tag == VMClosure)
      //    a.fun = callee.hof->f;

      //   a.pc = cip;
      //   // fun = a.fun;

      //   if (vm->callstack_size >= vm->callstack_length) {
      //     runerror(vm, "Stack overflow!");
      //   }

      //   vm->callstack[vm->callstack_size] = a;
      //   int n = lastarg - funreg;
      //   vm->callstack_size++; 
      //   vm->window += funreg;
      //   // fun = AS_VMFUNCTION(vm, regs[funreg]);
      //   fun = a.fun;

      //   print("num of function args: %d\n", n);
      //   print("num of arguments: %d\n", fun->arity);

      //   if (n > fun->arity) 
      //     runerror(vm, "Function arity and arguments mismatched ");
      //   if (fun->nregs >= 255)
      //     runerror(vm, "Register file overflowed");
      //   cip = 0;
      //   continue;
      // }
      case Return:
      {
        // printf("previous fun pointer: %p\n", (void *) fun);
        struct Activation a = vm->callstack[vm->callstack_size - 1];
        regs[a.dest_reg - a.start_window] = regs[uX(i)];
        vm->callstack_size--;
        cip = a.pc + 1;
        vm->window -= a.start_window;
        fun = a.fun;
        // printf("reset fun pointer: %p\n", (void *) fun);
        // print("in return\n");
        continue;
      }
      case Mov: 
      {
        regs[uX(i)] = regs[uY(i)];
        break;
      }
      case TailCall:
      {
        int lastarg = uY(i); //this is rn from the semantics
        int funreg = uX(i);  //this is r0 from the semantics
        //check that last arg - funreg is the arity

        // struct Activation last_call = vm->callstack[vm->callstack_size];
        //TODO: delete this line:
        regs[0]  = regs[funreg];

        memmove(regs, regs + funreg, (lastarg-funreg + 1) * sizeof(*regs));
        fun = AS_VMFUNCTION(vm, regs[funreg]);
        //check that there are enough regs for function
        cip = 0;
        continue;
      }
      case Symbol_Observer:
      {
        regs[uX(i)] = mkBooleanValue(isSymbol(regs[uY(i)]));
        break;
      }
      case Number_Observer:
      {
        regs[uX(i)] = mkBooleanValue(isNumber(regs[uY(i)]));
        break;
      }
      case Function_Observer:
      {
        regs[uX(i)] = mkBooleanValue(isFunction(regs[uY(i)]));
        break;
      }
      case Nil_Observer:
      {
        // print("in nil observer\n");
        regs[uX(i)] = mkBooleanValue(eqvalue(regs[uY(i)], nilValue));
        break;

      }
      case Null_Observer:
      {
        // print("in null observer\n");
        regs[uX(i)] = mkBooleanValue(eqvalue(regs[uY(i)], emptylistValue));
        // print("%v", mkBooleanValue(eqvalue(regs[uY(i)], emptylistValue)));
        break;
      }
      case CheckAssert:
      {
        Value v = vm->literal_pool[uYZ(i)];
        regs[uX(i)] = mkBooleanValue(AS_BOOLEAN(vm, v));
        break;      
      }
      case Car:
      {
        Value v = regs[uY(i)];
        if (eqvalue(emptylistValue, v)) 
          runerror(vm, "car of empty list");
        struct VMBlock* bl = AS_BLOCK(vm, regs[uY(i)]);

        regs[uX(i)] = bl->slots[0];
        break;
      }
      case Cdr:
      {
        if (eqvalue(regs[uY(i)], emptylistValue)) {
           regs[uX(i)] = emptylistValue;
        }
        else {
          struct VMBlock* bl = AS_BLOCK(vm, regs[uY(i)]);
          if (bl->nslots == 1)
            regs[uX(i)] = emptylistValue;
          else {
          VMNEW(struct VMBlock*, new_list, sizeof(*new_list) + (new_list->nslots - 1) * sizeof(new_list->slots[0]));
          new_list->nslots = bl->nslots - 1;
          memcpy(new_list->slots, bl->slots + 1, bl->nslots * sizeof(bl->slots[0]));
          regs[uX(i)] = emptylistValue;
          }
        }
        break;
      }
      case MkClosure:
      {
        // Number_T nslots = AS_NUMBER(vm, uZ(i));
        VMNEW(struct VMClosure*, closure, sizeof(*closure) + uZ(i) * sizeof(closure->captured[0]));
        closure->nslots = uZ(i);
        closure->f = AS_VMFUNCTION(vm, regs[uY(i)]);
        regs[uX(i)] = mkClosureValue(closure);
        break;
      }
      case GetClSlot:
      {
        struct VMClosure* closure = AS_CLOSURE(vm, regs[uY(i)]);
        // Number_T slot = AS_NUMBER(vm, uZ(i));
        regs[uX(i)] = closure->captured[uZ(i)];
        break;
      }
      case SetClSlot:
      {
        struct VMClosure* closure = AS_CLOSURE(vm, regs[uX(i)]);
        // Number_T slot = AS_NUMBER(vm, uZ(i));
        closure->captured[uZ(i)] =  regs[uY(i)];
        break;
      }
      case Error:
      {
        runerror(vm, AS_CSTRING(vm, regs[uX(i)]));
        break;
      }
      case Cons:
      {
        Value cons_val = regs[uY(i)];
        if (eqvalue(regs[uZ(i)], emptylistValue)) {
          VMNEW(struct VMBlock*, new_list, sizeof(*new_list) + sizeof(new_list->slots[0]));
          new_list->nslots = 1;
         new_list->slots[0] = cons_val;
          regs[uX(i)] = mkBlockValue(new_list);
        }
        else {
          struct VMBlock* bl = AS_BLOCK(vm, regs[uZ(i)]);
          VMNEW(struct VMBlock*, new_list, sizeof(*new_list) + (bl->nslots + 1) * sizeof(new_list->slots[0]));
          new_list->nslots = bl->nslots + 1;
          memcpy(new_list->slots + 1, bl->slots, bl->nslots * sizeof(bl->slots[0]));
          new_list->slots[0] = cons_val;
        regs[uX(i)] = mkBlockValue(new_list);
        }

        break;
      }
      case Halt:
        vm->ip = cip;
        return;
      default:
        runerror(vm, "Instruction word does not contain valid opcode");
        break;
    }
    cip++;
  }

  // Run code from `fun` until it executes a Halt instruction.
  // Then return.
  return;
}
