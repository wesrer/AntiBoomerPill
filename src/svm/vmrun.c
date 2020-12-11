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
#include "vmsizes.h"
#include "print.h"

#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"
#include "vtable.h"
#include "svmdebug.h"
#include "disasm.h"

#define VMSAVE()  (vm->current_fun = fun, vm->ip = cip)
#define VMLOAD()  (fun = vm->current_fun, cip = vm->ip, i = fun->instructions[cip])
// #define GC() (VMSAVE(), gc(vm), VMLOAD())

#define RX regs[uX(i)]
#define RY regs[uY(i)]
#define RZ regs[uZ(i)]

#define LV() (literal_value(vm, uYZ(i)))

void vmrun(VMState vm, struct VMFunction *fun) {
  int cip = 0;   //cached instruction pointer

  const char *dump_decode = svmdebug_value("decode");
  const char *dump_call   = svmdebug_value("call");
  (void) dump_call;  // make it OK not to use `dump_call`

  vm->current_fun = fun;
  Value* regs = vm->registers;
  Instruction* instrs = fun->instructions;

  while (true) {
    Instruction i = instrs[cip];
    
    if (dump_decode)
      idump(stderr, vm, cip, i, vm->window, &RX, &RY, &RZ);

    switch(opcode(i)) {
      case If:
      {
        bool b = AS_BOOLEAN(vm, regs[uX(i)]);
        if (!b)
          cip ++;
        break;
      }
      case GoTo:
      { 
        int32_t jump = iXYZ(i);
        // if (jump < 0 && gc_needed)
          // GC();
        cip += jump;
        continue;
      }
      case Print:
        print("%v\n", RX);        
        break; 
      case Check:
        check(vm, AS_CSTRING(vm, LV()),  RX);
        break;
      case Expect:
        expect(vm, AS_CSTRING(vm, LV()),  RX);
        break;
      case Not:
        RX = mkBooleanValue(!AS_BOOLEAN(vm, RY));
        break;
      case Or:
        RX = mkBooleanValue(AS_BOOLEAN(vm, RY) || AS_BOOLEAN(vm, RZ));
        break;
      case And:
        RX = mkBooleanValue(AS_BOOLEAN(vm, RY) && AS_BOOLEAN(vm, RZ));
        break;
      case LoadLiteral:
        regs[uX(i)] = LV();
        break;
      case Add:
        RX = mkNumberValue(AS_NUMBER(vm, RY) + AS_NUMBER(vm, RZ));
        break;
      case GreaterThan:
        RX = mkBooleanValue(AS_NUMBER(vm, RY) > AS_NUMBER(vm, RZ));
        break;
      case LessThan:
        RX = mkBooleanValue(AS_NUMBER(vm, RY) < AS_NUMBER(vm, RZ));
        break;
      case GreaterThanEqualTo:
        RX = mkBooleanValue(AS_NUMBER(vm, RY) >= AS_NUMBER(vm, RZ));
        break;
      case LessThanEqualTo:
        RX = mkBooleanValue(AS_NUMBER(vm, RY) <= AS_NUMBER(vm, RZ));
        break;  
      case Equal:
        RX = mkBooleanValue(eqvalue(RY, RZ));
        break;
      case NotEqual:
        RX = mkBooleanValue(!eqvalue(RY, RZ));
        break;  
      case Zero: 
        RX = mkNumberValue(0);
        break;
      case Subtract: 
        RX = mkNumberValue(AS_NUMBER(vm, RY) - AS_NUMBER(vm, RZ));
        break;
      case IDiv: 
        RX = mkNumberValue((int) (AS_NUMBER(vm, RY) / AS_NUMBER(vm, RZ)));
        break;
      case FloatDiv: 
        RX = mkNumberValue(AS_NUMBER(vm, RY) / AS_NUMBER(vm, RZ));
        break;
      case Multiply: 
        RX = mkNumberValue(AS_NUMBER(vm, RY) * AS_NUMBER(vm, RZ));
        break;
      // case SetGlobal: 
      //   VTable_put(vm->globals, LV(), regs[uX(i)]);
      //   break;
      // case GetGlobal: 
      //   RX = VTable_get(vm->globals, LV());
      //   break;
      case SetGlobal:
        global_insert(vm, uYZ(i), regs[uX(i)]);
        break;

      case GetGlobal:
        regs[uX(i)] = global_value(vm, uYZ(i));
        break;

      case MakeConsCell: 
      { 
        VMNEW(struct VMBlock *, bl, vmsize_block(1));
        bl->nslots = 1;
        bl->slots[0] = regs[uX(i)];
        RX = mkConsValue(bl);
        break;
      }
      case ProjectBool: 
        RX = mkBooleanValue(AS_BOOLEAN(vm, RX));
        break;
      case Call:
      {
        int lastarg = uZ(i);
        int funreg = uY(i);
        int destreg = uX(i);

        // if (gc_needed)
        //   GC();

        struct Activation a;
        a.start_window = funreg;
        a.end_window = lastarg;
        a.dest_reg = destreg;
        a.fun = fun;
        a.pc = cip;

	int32_t callstack_size = vm->callstack_size++;

        if (callstack_size >= vm->callstack_length)
          runerror(vm, "Stack overflow!");
        vm->callstack[callstack_size] = a;

        vm->window += a.start_window; //shift the window

        Value callee = regs[funreg];

        if (callee.tag == VMFunction)
         fun = callee.f;
        else if (callee.tag == VMClosure)
         fun = callee.hof->f;
        else
         runerror(vm, "Attempted to call a non function\n");
        if (lastarg - funreg > fun->arity) 
            runerror(vm, "Function arity and arguments mismatched ");
        if (fun->nregs >= 255)
          runerror(vm, "Register file overflowed");
        instrs = fun->instructions;
        cip = 0;
        regs += a.start_window;
        continue;
      }

      case GC:
        // GC();
        break;

      case Return:
      {
        struct Activation a = vm->callstack[vm->callstack_size - 1];
        regs[a.dest_reg - a.start_window] = RX;
        regs -= a.start_window;
        vm->window -= a.start_window;
        vm->callstack_size--;
        cip = a.pc + 1;
        fun = a.fun;
        instrs = fun->instructions;
        continue;
      }
      case Mov: 
        RX = RY;
        break;
      case TailCall:
      {
        int lastarg = uY(i);
        int funreg = uX(i);

        Value callee = regs[funreg];
        if (callee.tag == VMFunction)
         fun = GCVALIDATE(callee.f);
        else if (callee.tag == VMClosure)
         fun = GCVALIDATE(callee.hof->f);
        else         
          runerror(vm, "Attempted to tailcall a non function\n");

        if ((lastarg-funreg) > fun->arity) 
          runerror(vm, "Function arity and arguments mismatched ");
        if (fun->nregs >= 255)
          runerror(vm, "Register file overflowed");

        memmove(regs, regs + funreg, (lastarg - funreg + 1) * sizeof(*regs));
        instrs = fun->instructions;
        cip = 0;
        continue;
      }
      case Symbol_Observer:
        RX = mkBooleanValue(isSymbol(RY));
        break;
      case Boolean_Observer: 
        RX = mkBooleanValue(isBoolean(RY));
        break;
      case Number_Observer:
        RX = mkBooleanValue(isNumber(RY));
        break;
      case Function_Observer:
        RX = mkBooleanValue(isFunction(RY));
        break;
      case Pair_Observer:
        RX = mkBooleanValue(isPair(RY));
        break;
      case Nil_Observer:
        RX = mkBooleanValue(eqvalue(RY, nilValue));
        break;
      case Null_Observer:
        RX = mkBooleanValue(eqvalue(RY, emptylistValue));
        break;
      case CheckAssert:
      {
        Value v = LV();
        check(vm, AS_CSTRING(vm, v),  RX);
        expect(vm, AS_CSTRING(vm, v),  mkBooleanValue(true));
        break;      
      }
      case Car:
      {
        Value v = RY;
        if (eqvalue(emptylistValue, v)) 
          runerror(vm, "car of empty list");
        RX = AS_DENSE_CONS(vm, v)->car;
        break;
      }
      case Cdr:
        RX = AS_DENSE_CONS(vm, RY)->cdr;
        break;
      case MkClosure:
      {
        int size = uZ(i);
        VMNEW(struct VMClosure*, closure, vmsize_closure(size));
        closure->nslots = size;
        closure->f = AS_VMFUNCTION(vm, RY);
        RX = mkClosureValue(closure);
        break;
      }
      case GetClSlot:
        RX = AS_CLOSURE(vm, regs[uY(i)])->captured[uZ(i)];
        break;
      case SetClSlot:
        AS_CLOSURE(vm, RX)->captured[uZ(i)] = RY;
        break;
      case Error:
        runerror(vm, AS_CSTRING(vm, RX));
        break;
      case Cons:
      {
        VMNEW(struct VMDenseCons*, new_list, sizeof(*new_list));
        // new_list->nslots = 2;
        new_list->car = RY;
        new_list->cdr = RZ;
        RX = mkDenseConsValue(new_list);
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
  return;
}
