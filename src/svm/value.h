// Representation of all VM values.

// This is the core of the VM, used heavily from module 1 on.

#ifndef VALUE_INCLUDED
#define VALUE_INCLUDED

#include <stdint.h>
#include <stdbool.h>

#include "iformat.h"
#include "gcmeta.h"

//// Each value is identified by one of the following tags ////

typedef enum { Nil = 0,   // vScheme value not found in uScheme
                          // (it's the initial value of an otherwise 
                          // undefined variable)

               Boolean,   // vScheme S-expressions
               Number,
               String,
               Emptylist,
               ConsCell,   // represented as Block (for now)

               VMFunction, // first-order function (sequence of instructions)
               VMClosure,  // higher-order function


               ///// stretch goals for more interesting languages

               Block,      // block of values of fixed size: record, etc
               Seq,        // variable-size sequence in the style of Hanson/Icon/CLU
               Table,      // hash table


               ///// potential support for writing primitives/libraries in C

               CFunction, 
               LightUserdata,   // pointer to data on the C heap
} VTag;

extern const char *tagnames[];  // map VTag to string


////////////////  representation of values

typedef double Number_T; // representation of a VM number

typedef struct Value {
  VTag tag;
  union {  // payloads for everything except Nil and EmptyList
    bool b;
    Number_T n;
    struct VMString *s;
    struct VMBlock *block;  // also ConsCell
    struct VMFunction *f;
    struct VMClosure *hof;
    struct VSeq_T *seq;
    struct VTable_T *table;
    struct CFunction *cf;
    void *p;               // light userdata == external memory 
  };
} Value;  // Note!  Not a pointer type!

////////////////////////////////////////////////////////////////

//////   creation/introduction for values

extern struct Value nilValue;       // initialized statically
extern struct Value emptylistValue; // initialized statically

// remaining creators are embedding functions

static inline Value mkNumberValue(Number_T n);
static inline Value mkStringValue(struct VMString *s);
static inline Value mkBooleanValue(bool b);
static inline Value mkVMFunctionValue(struct VMFunction *f);
static inline Value mkClosureValue(struct VMClosure *cl);
static inline Value mkBlockValue(struct VMBlock *bl);
static inline Value mkConsValue(struct VMBlock *bl);
static inline Value mkTableValue(struct VTable_T *table);

////// observation/elimination for values

// projection functions (to be called via macros below)

typedef struct VMState *VMState;  // for stack trace if projection fails

static inline struct VMBlock *asBlock_    (VMState, Value, const char *file, int line);
static inline struct VMBlock *asCons_     (VMState, Value, const char *file, int line);
static inline const char  *asCString_     (VMState, Value, const char *file, int line);
static inline Number_T        asNumber_   (VMState, Value, const char *file, int line);
static inline struct VMClosure *asClosure_(VMState, Value, const char *file, int line);
static inline struct VMFunction *asVMFunction_ 
                                          (VMState, Value, const char *file, int line);
static inline struct VMString *asVMString_(VMState, Value, const char *file, int line);
static inline bool         asBoolean_ (VMState, Value, const char *file, int line);

#define AS_BOOLEAN(VM, V)    asBoolean_    ((VM), (V), __FILE__, __LINE__)
#define AS_BLOCK(VM, V)      asBlock_     ((VM), (V), __FILE__, __LINE__)
#define AS_CONS_CELL(VM, V)  asCons_      ((VM), (V), __FILE__, __LINE__)
#define AS_CSTRING(VM, V)    asCString_   ((VM), (V), __FILE__, __LINE__)
#define AS_NUMBER(VM, V)     asNumber_    ((VM), (V), __FILE__, __LINE__)
#define AS_CLOSURE(VM, V)    asClosure_   ((VM), (V), __FILE__, __LINE__)
#define AS_VMFUNCTION(VM, V) asVMFunction_((VM), (V), __FILE__, __LINE__)
#define AS_VMSTRING(VM, V)   asVMString_  ((VM), (V), __FILE__, __LINE__)

// additional observers for values

extern uint32_t hashvalue(Value v);
extern bool eqvalue  (Value v1, Value v2); // vscheme = primitive
extern bool identical(Value v1, Value v2); // for hashing
extern bool eqtests  (Value v1, Value v2); // for check-expect

////////////////////////////////////////////////////////////////
//
// representations

// when you're read to do cons,car,cdr (from module 1 onward):

struct VMBlock {
  GCMETA(VMBlock)
  int nslots;
  struct Value slots[];
};


// when you implement first-order functions (module 7 and onward):

struct VMFunction {
  GCMETA(VMFunction)
  // Note: as yet, lacks GC support
  int arity; // number of args expected
  int nregs; // one more than the number of highest register read or written
  int size;  // number of instructions
  Instruction instructions[];
};

// when you implement higher-order functions (module 10 and onward):

struct VMClosure {
  GCMETA(VMClosure)
  struct VMFunction *f;
  int nslots;
  struct Value captured[];
};


// stretch goals:

typedef int (*CFunction_Code)(VMState state, void *env, int actuals);
    // result == number of things returned

struct CFunction { // a closure
  GCMETA(CFunction)
  void *env; // environment
  CFunction_Code code;
  int arity;        // number of arguments expected
  int nregs; // one more than the number of highest register read or written
  const char *name; // for diagnostics
};


////////////////////////////////////////////////////////////////
// no user-serviceable parts past this point

#include <stdnoreturn.h>

#include "vmstring.h"

extern _Noreturn void typeerror(VMState state, const char *expected, Value got,
                                const char *file, int line);


static inline struct VMBlock *asBlock_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != Block)
    typeerror(vm, "a block", v, file, line);
  return v.block;
}
static inline struct VMBlock *asCons_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != ConsCell)
    typeerror(vm, "a cons cell", v, file, line);
  return v.block;
}
static inline const char *asCString_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != String)
    typeerror(vm, "a string", v, file, line);
  return v.s->bytes;
}
static inline Number_T asNumber_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != Number)
    typeerror(vm, "a number", v, file, line);
  return v.n;
}
static inline struct VMClosure *asClosure_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != VMClosure)
    typeerror(vm, "a closure", v, file, line);
  return v.hof;
}
static inline struct VMFunction *asVMFunction_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != VMFunction)
    typeerror(vm, "a VM function", v, file, line);
  return v.f;
}
static inline struct VMString *asVMString_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != String)
    typeerror(vm, "a string", v, file, line);
  return v.s;
}


//values that are truthy: non-zero numbers, non-empty strings, cons cell, blocks of any size (even 0)
//values that are falsy: nil, 0, empty string, emptylist
//everything else can't be projected into a bool
static inline bool asBoolean_(VMState vm, Value v, const char *file, int line) {
    VTag tag = v.tag;
    if (tag == Nil)
      return false;
    if (tag == Boolean)
      return v.b;
    if (tag == Number)
      return v.n != 0;
    else if (tag == String)
      return AS_CSTRING(vm, mkStringValue(v.s)) == AS_CSTRING(vm, mkStringValue(Vmstring_newc("")));
    else if (tag == Emptylist)
      return true;
    else if (tag == ConsCell)
      return true;
    else if (tag == Block)
       return true;
    else {
      typeerror(vm, "a Boolean", v, file, line);
    }
}

////////////////////////////////////////////////////////////////


static inline Value mkNumberValue(Number_T n) {
    Value val;
    val.tag = Number;
    val.n = n;
    return val;
}

static inline Value mkStringValue(struct VMString *s) {
    Value val;
    val.tag = String;
    val.s = s;
    return val;
}

static inline Value mkBooleanValue(bool b) {
  Value val;
  val.tag = Boolean;
  val.b = b;
  return val;
}

static inline Value mkVMFunctionValue(struct VMFunction *f) {
  Value val;
  val.tag = VMFunction;
  val.f = f;
  return val;
}

static inline Value mkClosureValue(struct VMClosure *cl) {
  Value val;
  val.tag = VMClosure;
  val.hof = cl;
  return val;
}

static inline Value mkBlockValue(struct VMBlock *bl) {
  Value val;
  val.tag = Block;
  val.block = bl;
  return val;
}

static inline Value mkConsValue(struct VMBlock *bl) {
  Value val;
  val.tag = ConsCell;
  val.block = bl;
  return val;
}

/////////////////////////////////////////////////////////////////////////////////

static inline bool isFunction(Value v) {
  if (v.tag != VMFunction)
    return false;
  return true;
}

static inline bool isSymbol(Value v) {
  if (v.tag != String)
    return false;
  return true;
}

static inline bool isBoolean(Value v) {
  return v.tag == Boolean;
}

static inline bool isNumber(Value v) {
  return v.tag == Number;
}

static inline bool isPair(Value v) {
  if (v.tag != Block)
    return false;
  else if (v.block->slots[1].tag == Block)
    return false;
  return true;
}

static inline Value mkTableValue(struct VTable_T *t) {
  Value val;
  val.tag = Table;
  val.table = t;
  return val;
}




#endif
