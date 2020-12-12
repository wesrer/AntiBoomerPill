#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

struct Activation {
  int start_window;
  int end_window;
  int dest_reg;
  struct VMFunction* fun;
  // Value check_value;
  // bool check_error;
  int pc;
};

#endif
