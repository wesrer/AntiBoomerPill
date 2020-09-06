// -*- c-indent-level: 4; c-basic-offset: 4 -*-

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "vtable.h"
#include "value.h"

void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    (void)vm; // suppress compiler warnings
    assert(0); // must free all memory associated with `vm`
}

VMState newstate(void) {
    // allocate, initialize, and return a new state
    assert(0);
}

int literal_slot(VMState state, Value literal) {
    (void)state; // suppress compiler warnings
    (void)literal;
    // return a slot containing the literal, updating literal pool if needed
    assert(0);
}
