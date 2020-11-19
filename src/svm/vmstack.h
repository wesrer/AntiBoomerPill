#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

struct Activation {
  int start_window;
  int end_window;
  int dest_reg;
  struct VMFunction* fun;
  int pc;
};

#endif



// static inline struct VMString   *forward_string  (struct VMString   *p, bool *changed_color);
// static inline struct VMFunction *forward_function(struct VMFunction *p, bool *changed_color);
// static inline struct VMClosure  *forward_closure (struct VMClosure  *p, bool *changed_color);
// static inline struct VMBlock    *forward_block   (struct VMBlock    *p, bool *changed_color);
// static inline struct VTable_T   *forward_table   (struct VTable_T   *p, bool *changed_color);