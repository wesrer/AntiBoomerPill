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
#define VMLOAD()  (fun = vm->current_fun, regs = vm->registers + vm->window, cip = vm->ip, i = fun->instructions[cip])
#define GC() (VMSAVE(), gc(vm), VMLOAD())


void vmrun(VMState vm, struct VMFunction *fun) {
  //cached instruction pointer
  int cip = 0;
  //cached register ptr
  const char *dump_decode = svmdebug_value("decode");
  const char *dump_call   = svmdebug_value("call");
  (void) dump_call;  // make it OK not to use `dump_call`
  vm->current_fun = fun;

  while (true) {
    Value* regs = vm->registers + vm->window;
    Instruction i = fun->instructions[cip];
    Value RX = regs[uX(i)];
    Value RY = regs[uY(i)];
    Value RZ = regs[uZ(i)];
    vm->current_fun = fun;

    // for (int i = 0; i <= fun->nregs; i++){
    //   print("reg %d is %v \n", i, regs[i]);
    // }

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
        if (jump < 0 && gc_needed)
          GC();
        cip += jump;
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
        Value v = literal_value(vm, uYZ(i));
        check(AS_CSTRING(vm, v),  source);
        break;
        }
      case Expect:
        {
        Value source = regs[uX(i)];
        Value v = literal_value(vm, uYZ(i));
        expect(AS_CSTRING(vm, v),  source);
        break;
        }
      case Not:
      {
        bool b = AS_BOOLEAN(vm, regs[uX(i)]);
        print("in not, target boolean is: %v", regs[uX(i)]);
        regs[uX(i)] = mkBooleanValue(!b);
        break;
      }
      case LoadLiteral:
      {
        Value v = literal_value(vm, uYZ(i));
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
      case GreaterThan:
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        Value v = mkBooleanValue(num1 > num2);
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
      case IDiv: 
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        int res = (int) num1 / num2;
        Value v = mkNumberValue(res);
        regs[uX(i)] = v;
        break;
      }
      case FloatDiv: 
      {
        Number_T num1 = AS_NUMBER(vm, regs[uY(i)]);
        Number_T num2 = AS_NUMBER(vm, regs[uZ(i)]);
        int res = (int) num1 / num2;
        Value v = mkNumberValue(res);
        regs[uX(i)] = v;
        break;
      }
      case FloatDiv: 
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
        Value v = literal_value(vm, uYZ(i));
        VTable_put(vm->globals, v, regs[uX(i)]);
        break;
      }
      case GetGlobal: 
      {
        Value v = VTable_get(vm->globals, literal_value(vm, uYZ(i)));
        regs[uX(i)] = v;
        break;
      }
      case MakeConsCell: 
      { 
        VMNEW(struct VMBlock *, bl, vmsize_block(1));
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
      //TODO: check if theclere are right number of arguments
      case Call:
      {
        int lastarg = uZ(i);
        int funreg = uY(i);
        int destreg = uX(i);

        if (gc_needed)
        {
        //  printf("callee before gc: %p\n", (void*) fun);
        //  printf("arity of callee: %d\n", fun->arity);
        //  printf("function ptr changed to:%p\n", (void*) a.fun);
        GC();
        // printf("garbage collected callee: %p\n", (void*) fun);
        }

        struct Activation a;

        a.start_window = funreg;
        a.end_window = lastarg;
        a.dest_reg = destreg;
        // printf("callee in act record set to: %p\n", (void*) fun);

        a.fun = fun;
        a.pc = cip;

        // printf("arity of fun being stored on activation record: %d\n", fun->arity);


        // printf("calling function in r%d, with registers r%d - r%d\n", funreg, funreg, lastarg);
        // print("in function, value in %d is: %v\n", 1, regs[1]);

        if (vm->callstack_size >= vm->callstack_length) {
          runerror(vm, "Stack overflow!");
        }

        vm->callstack[vm->callstack_size] = a;
        int n = lastarg - funreg;
        // print("number of arguments in callee: %d\n", n);
        // print("arity of callee: %d\n", n);

        // print("shifting window by: %d\n", a.start_window);

        vm->callstack_size++; 
        vm->window += a.start_window;
        Value callee = regs[funreg];
        
        if (callee.tag == VMFunction)
         fun = callee.f;
        else if (callee.tag == VMClosure)
         fun = callee.hof->f;
        else
         runerror(vm, "Attempted to call a non function\n");
      
        // printf("number of arguments: %d\n", n);
        // printf("arity of function: %d\n", fun->arity);



        if (n > fun->arity) 
        {
        //   printf("number of arguments of callee: %d\n", n);
        // printf("arity of  callee: %d\n", fun->arity);
        // printf("arity of function: %d\n", a.fun->arity);
          runerror(vm, "Function arity and arguments mismatched ");
        }
        if (fun->nregs >= 255)
          runerror(vm, "Register file overflowed");
        cip = 0;
        // printf("calling gc");
        continue;
      }
      case GC:
      {
        print("calling gc as a command");
        GC();
        break;
      }
      //TODO: check if there are right number of arguments
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
        Value callee = regs[funreg];
        if (callee.tag == VMFunction)
         fun = callee.f;
        else if (callee.tag == VMClosure)
         fun = callee.hof->f;
        else
         runerror(vm, "function used is not defined");

        memmove(regs, regs + funreg, (lastarg-funreg + 1) * sizeof(*regs));

      //  if (lastarg-funreg > fun->arity) 
      //     runerror(vm, "Function arity and arguments mismatched ");
      //   if (fun->nregs >= 255)
      //     runerror(vm, "Register file overflowed");
        //check that there are enough regs for function
        cip = 0;
        continue;
      }
      case Symbol_Observer:
      {
        regs[uX(i)] = mkBooleanValue(isSymbol(regs[uY(i)]));
        break;
      }
      case Boolean_Observer: 
      {
        regs[uX(i)] = mkBooleanValue(isBoolean(regs[uY(i)]));
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
        // print("in null observer, value in %d is: %v\n", uY(i), regs[uY(i)]);
        // print("in null observer, value in %d is: %v\n", 1, regs[1]);

        regs[uX(i)] = mkBooleanValue(eqvalue(regs[uY(i)], emptylistValue));
        // print("in null observer: %v\n", mkBooleanValue(eqvalue(regs[uY(i)], emptylistValue)));
        break;
      }
      case CheckAssert:
      {
        Value v = literal_value(vm, uYZ(i));
        check(AS_CSTRING(vm, v),  regs[uX(i)]);
        expect(AS_CSTRING(vm, v),  mkBooleanValue(true));
        break;      
      }
      case Car:
      {
        Value v = regs[uY(i)];
        if (eqvalue(emptylistValue, v)) 
          runerror(vm, "car of empty list");

        struct VMBlock* bl = AS_CONS_CELL(vm, v);
        // memcpy(regs + uX(i), bl->slots, sizeof(bl->slots[0]));
        regs[uX(i)] = bl->slots[0];
        // print("Car'd value is:%v\n", regs[uX(i)]);
        // print("Car'd value is:%v\n", bl->slots[1]);
        // print("Car'd value is:%v\n",regs[uY(i)]);

        // print("Value at rear is:%v\n", v);
        break;
      }
      case Cdr:
      {
        struct VMBlock* bl = AS_CONS_CELL(vm, regs[uY(i)]);
        // if (bl->nslots == 1)
        //   {regs[uX(i)] = emptylistValue;  
        //   break;}    
        regs[uX(i)] = bl->slots[1];


        // if (eqvalue(regs[uY(i)], emptylistValue)) {
        //    regs[uX(i)] = emptylistValue;
        // }
        // else {
        //   struct VMBlock* bl = AS_CONS_CELL(vm, regs[uY(i)]);
        //   if (bl->nslots == 1)
        //     regs[uX(i)] = emptylistValue;
        //   else {
        //   VMNEW(struct VMBlock*, new_list, vmsize_block(bl->nslots - 1));
        //   new_list->nslots = bl->nslots - 1;
        //   memcpy(new_list->slots, bl->slots + 1, bl->nslots * sizeof(bl->slots[0]));
        //   regs[uX(i)] = mkConsValue(new_list);
        //   }
        // }
        break;
      }
      case MkClosure:
      {
        // Number_T nslots = AS_NUMBER(vm, uZ(i));
        VMNEW(struct VMClosure*, closure, vmsize_closure(uZ(i)));
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
        //  print("in function, value in %d is: %v\n", 1, regs[1]);
        VMNEW(struct VMBlock*, new_list, vmsize_block(2));
        new_list->nslots = 2;
        new_list->slots[1] = regs[uZ(i)];
        new_list->slots[0] = cons_val;
        regs[uX(i)] = mkConsValue(new_list);
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
    // printf("cip in next iteration of while loop:%d\n", cip);

  }

  // Run code from `fun` until it executes a Halt instruction.
  // Then return.
  return;
}
