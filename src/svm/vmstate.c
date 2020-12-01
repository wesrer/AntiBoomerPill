// Memory management and literal addition for VMState

// You'll complete this file as part of module 1


#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "print.h"
#include "vmstate.h"
#include "vtable.h"
#include "value.h"

// constants

#define CALLSTACK_SIZE 50000
#define REGISTER_SIZE 5000
#define LITERAL_SIZE 5000
#define GLOBALS_SIZE 5000


void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    free(vm->registers);
    free(vm->literal_pool);
    free(vm->callstack);
    *sp = NULL
    free(vm);
}

VMState newstate(void) {
    VMState state = malloc(sizeof(*state));
    state->ip = 0;
    state->num_literals = 0;
    // state->instructions = NULL;
    state->registers= calloc(REGISTER_SIZE, sizeof(Value));
    state->callstack_length = CALLSTACK_SIZE;
    state->callstack_size = 0;
    state->callstack = malloc(CALLSTACK_SIZE * sizeof(struct Activation));
    state->literal_pool = malloc(LITERAL_SIZE * sizeof(Value));
    state->globals =  VTable_new(GLOBALS_SIZE);
    // state->highest_reg = 0;
    state->window = 0;
    return state;
}

int literal_slot(VMState state, Value literal) {
    // Return a slot containing the literal, updating literal pool if needed.
    // For module 1, you can get away with putting the literal in slot 0
    // and returning 0.  For module 2, you'll need something slightly
    // more sophisticated.
    int counter = state->num_literals;
    assert(counter < LITERAL_SIZE);

    // printf("literal counter is %d\n", counter);
    (state->literal_pool)[counter] = literal;
    (state->num_literals)++;
    return counter;
}

Value literal_value(VMState state, unsigned index) {
    return (state->literal_pool)[index];
}

int literal_count(VMState state) {
  return state->num_literals;
}
