// The VM loader: read virtual object code and update VM state

// This is the focus of module 2.  In future modules, we'll continue
// to use it, but we won't revise it except for an optional depth goal.
//
// You'll write the core of the loader: function `loadfun`.  This
// function loads a single VM function from a .vo file.  It is called
// by `loadmodule`, which load a single module, and which I provide.
// (A module is just a function that takes no parameters).  I also
// provide `loadmodules`, which loads a list of modules.  Finally, I
// provide `parse_instruction`, which uses the parsing table in file
// instructions.c to parse each single instruction.



#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "loader.h"
#include "itable.h"
#include "tokens.h"
#include "vmstate.h"
#include "vmheap.h"
#include "vmstring.h"
#include "value.h"

//// Variables and utility functions local to this module

static bool saw_a_bad_opcode = false;  

static Instruction
  parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp);
  // Parse `operands` according to the parser for the given `opcode`.
  // Return the parsed instruction.  If the instruction mentions any
  // register, update `*maxregp` to be at least as large as any
  // mentioned register (and no smaller than it was).
  //
  // If the opcode is not recognized, set `saw_a_bad_opcode` and return 0.

static Modules mkModules(struct VMFunction *first, Modules rest);
  // using malloc, make a cons cell for a linked list of modules


//// The function you will write

static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *input);
  // Allocates and returns a new function using `vmalloc`, with all fields
  // correctly set:
  //   - The arity is given by parameter `arity`.
  //   - The number of instructions is given by parameter `count`.
  //   - The instructions themselves are read from `input`, one per line.
  //   - The `nregs` field must be set to one more than the largest
  //     register mentioned in any instruction.


static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *input) {
  (void) vm; (void) arity; (void) count; (void) input; // replace with real code
  (void) parse_instruction; // replace with real code
  assert(0);
}


//// the exported functions

struct VMFunction *loadmodule(VMState vm, FILE *vofile) {
  static Name dotloadname, modulename;
  if (dotloadname == NULL) {
    dotloadname = strtoname(".load");
    modulename  = strtoname("module");
  }

  char *buffer = NULL;
  size_t bufsize;
  if (getline(&buffer, &bufsize, vofile) < 0) {
    // end of file, spec calls for NULL to be returned
    free(buffer);
    return NULL;
  }

  Tokens directive = tokens(buffer);
  Tokens directive_start = directive; // hang onto this so we can free it

  // parse ".load module <count>"
  Name n;
  n = tokens_get_name(&directive, buffer); // removes token from directive
  assert(n == dotloadname);
  n = tokens_get_name(&directive, buffer);
  assert(n == modulename);
  uint32_t count = tokens_get_int(&directive, buffer);
  assert(directive == NULL); // that must be the last token

  free(buffer); // done with the input line and its tokens
  free_tokens(&directive_start);

  return loadfun(vm, 0, count, vofile); // load the module
}

Modules loadmodules(VMState vm, FILE *input) {
  struct VMFunction *module = loadmodule(vm, input);
  if (module == NULL)
    return NULL;
  else
    return mkModules(module, loadmodules(vm, input));
}

///// utility functions

static Modules mkModules(struct VMFunction *first, Modules rest) {
  Modules ms = malloc(sizeof(*ms));
  assert(ms);
  ms->module = first;
  ms->next   = rest;
  return ms;
}

void freemodules(Modules *msp) {
  if (*msp) {
    freemodules(&(*msp)->next);
    free(*msp);
    *msp = NULL;
  }
}

static Instruction
parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp) {
  instruction_info *info = itable_entry(opcode);
  if (info) {
    return info->parser(vm, info->opcode, operands, maxregp);
  } else {
    fprintf(stderr, "No opcode for %s.\n", nametostr(opcode));
    saw_a_bad_opcode = true;
    return 0;
  }
}
