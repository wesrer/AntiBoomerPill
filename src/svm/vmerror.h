#ifndef ERROR_DEFINED
#define ERROR_DEFINED

#include <stdarg.h>
#include <stdnoreturn.h>

#include "vmstate.h"
#include "value.h"

extern _Noreturn void runerror(VMState state, const char *format, ...);
extern _Noreturn void typeerror(VMState state, const char *expected, Value got,
                                const char *file, int line);


#endif
