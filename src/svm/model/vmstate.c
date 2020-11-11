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
    vm->globals = NULL;
    free(vm);
    *sp = NULL;
}

VMState newstate(void) {
    VMState m = calloc(1, sizeof(*m)); // relies on tag for Nil == 0
    assert(m);
    m->globals = VTable_new(200);
    m->registers = malloc (256 * sizeof(*m->registers));
    m->literal_pool = malloc(1000 * sizeof(*m->literal_pool));
    m->callstack = malloc(5000 * sizeof(*m->callstack));
    return m;
}

int literal_slot(VMState state, Value literal) {
    int n = state->nlits++;
    assert(n < LITSIZE);
    state->literal_pool[n] = literal;
    return n;
}

Value literal_value(VMState vm, unsigned index) {
    return vm->literal_pool[index];
}

int literal_count(VMState vm) {
    return vm->nlits;
}
