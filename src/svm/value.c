#include <assert.h>

#include "value.h"

Value nilValue;

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

Value emptylistValue = { Emptylist };

uint32_t hashvalue(Value v) {
  (void)v;
  return 0;
}

#if 0

static long int hashindex (TObject *ref)
{
  // ALERT!  If objects in the heap are going to move,
  // then their addresses will not do as hash values.
  // Perhaps each object gets a unique ID? 
  long int h;
  switch (ttype(ref)) {
    case LUA_T_NUMBER:
      h = (long int)nvalue(ref); break;
    case LUA_T_STRING: case LUA_T_USERDATA:
      h = tsvalue(ref)->hash; break;
    case LUA_T_FUNCTION:
      h = (IntPoint)ref->value.tf; break;
    case LUA_T_CFUNCTION:
      h = (IntPoint)fvalue(ref); break;
    case LUA_T_ARRAY:
      h = (IntPoint)avalue(ref); break;
    default:
      lua_error ("unexpected type to index table");
      h = 0;  /* UNREACHEABLE */
  }
  if (h < 0) h = -h;
  return h;
}

#endif

bool eqvalue(Value v1, Value v2) { // XXX will not work for hashing!
  if (v1.tag != v2.tag)
    return false;
  else
    switch (v1.tag) {
    case Nil: return true;
    case Boolean: return v1.b == v2.b;
    case Number:  return v1.n == v2.n;
    case String:  return v1.s == v2.s;  // all strings assumed interned
    case Table:   return v1.table == v2.table;  // object identity
    case Seq:     return v1.seq == v2.seq;  // object identity
    case ConsCell: return false;
    case Emptylist: return true;
    case VMFunction: return false;
    case CFunction: return false;
    case VMClosure: return false;
    case LightUserdata: return false;
    default:  assert(0); return false; // not implemented yet
    }
}

bool eqtests(Value v1, Value v2) { // XXX will not work for hashing!
  if (v1.tag == v2.tag && v1.tag == ConsCell)
    return eqtests(v1.block->slots[0], v2.block->slots[0]) &&
           eqtests(v1.block->slots[1], v2.block->slots[1]);
  else
    return eqvalue(v1, v2);
}


const char *tagnames[] = {
   "nil",
   "a boolean", "a number", "a string", "the empty list", "a cons cell",
   "a VM function", "a closure",
   "a record", "a sequence", "a table", 
   "a C function", "a pointer",
};
                          
