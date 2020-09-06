// Virtual-machine strings

// They have a different representation from C strings.  You won't
// need this file until you want to implement instructions that
// allocate strings, or if you reach a stage where `AS_CSTRING` no
// longer meets your needs.

// The main idea here is to make it very efficient to use short
// strings as keys in hash tables.

#ifndef VMSTRING_INCLUDED
#define VMSTRING_INCLUDED

#include <stddef.h>
#include <stdint.h>

// subject to change

typedef struct VMString {
  // that part of a string that is represented on the heap
  size_t length;
  uint32_t hash; // if is zero and string is long, not hashed yet
  struct VMString *next_interned; // for interning table
  char bytes[];
} *VMString;

// invariant: an extra byte is allocated, and s->bytes[s->length] == '\0'

static inline size_t Vmstring_objsize(size_t len) {
  // size of a string object of the given length
  return sizeof(struct VMString) + (len + 1) * sizeof(char);
}


uint32_t Vmstring_hash(const char *s, size_t len);
uint32_t Vmstring_hashlong(VMString s); // done on demand for long string?

extern void Vmstring_init(void);
extern void Vmstring_finish(void); // recover for valgrind

VMString Vmstring_new(const char *p, size_t length);
  // interned only if short
VMString Vmstring_newc(const char *p);
  // interned only if short

VMString Vmstring_newlong(const char *p, size_t len);
  

typedef struct StringBuffer *StringBuffer;

StringBuffer Vmstring_buffer(size_t length);
void Vmstring_putc(StringBuffer p, char c);
VMString Vmstring_of_buffer(StringBuffer *bp);
  // interns the contents of the buffer and frees its memory

#endif
