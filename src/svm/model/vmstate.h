// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <stdint.h>

#include "value.h"

#ifndef TINYVM // production

#define NREGS 256
#define LITSIZE 10000  // could go up to 65536

#else  // easier to read in debugger

#define NREGS 10
#define LITSIZE 10

#endif

typedef struct VMState *VMState;

struct VMState {
    struct VMFunction *running; // currently running function
    int pc;               // index of instruction about to run
    int nlits;            // number of literal slots in use
    struct VTable_T *globals;
    Value registers[NREGS];
    Value literals[LITSIZE];
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState *sp); // deallocate

int literal_slot(VMState state, Value literal);
  // return index of literal in `literals`, adding if needed
  // (at need, can be postponed to module 2)

Value literal_value(VMState state, unsigned index);
  // Return the value at the given index. *Not* intended 
  // for use in `vmrun`, in which you don't want to pay the 
  // overhead of a function call.

#endif /* VMSTATE_INCLUDED */
