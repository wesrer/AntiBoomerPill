#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

struct Activation {
  int start_window;
  // TODO: take this out
  int end_window;
  int dest_reg;
  struct VMFunction* fun;
  int pc;
};

#endif
