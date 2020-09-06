#ifndef VTABLE_INCLUDED
#define VTABLE_INCLUDED

#include "value.h"

#define T VTable_T

typedef struct T *T;
extern T     VTable_new   (int hint);
  // allocate a new table from garbage-collected memory, so the heap
  // must be initialized

extern int   VTable_length(T vtable);
extern Value VTable_put   (T vtable, Value key, Value value);
extern Value VTable_get   (T vtable, Value key);
extern Value VTable_remove(T vtable, Value key); // equivalent to putting nilValue

#undef T
#endif
