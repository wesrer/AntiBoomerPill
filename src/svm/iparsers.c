//// Parsers for loading virtual object code.

// In module 2, you add parsers `parseR1LIT` to this file.
// The other parsers may serve as examples you can build on.

#include <assert.h>

#include "iformat.h"
#include "iparsers.h"
#include "vmstate.h"
#include "value.h"

#include <stdlib.h>

#define SEE(R) do { if ((R) > *maxreg) *maxreg = (R); } while(0)

Instruction parseR3(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t regZ = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY); SEE(regZ);
  return eR3(opcode, regX, regY, regZ);
}

Instruction parseR2(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR2(opcode, regX, regY);
}

Instruction parseR1(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  return eR1(opcode, regX);
}

Instruction parseR0(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  assert(operands == NULL);
  return eR0(opcode);
}

Instruction parseR1U16(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint32_t immediate = tokens_get_int(&operands, NULL);
  assert(operands == NULL);
  assert(immediate == (uint16_t) immediate);
  SEE(regX);
  return eR1U16(opcode, regX, immediate);
}

Instruction parseR2U8(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t k    = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR3(opcode, regX, regY, k);
}

Instruction parseR0I24(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  int32_t immediate = tokens_get_int(&operands, NULL);
  assert(immediate == ((immediate << 8) >> 8));
  assert(operands == NULL);
  return eR0I24(opcode, immediate);
}


static Name truename, falsename, nilname, emptyname, stringname, functionname;

static void initnames(void) {
  if (truename == NULL) {
    truename     = strtoname("true");
    falsename    = strtoname("false");
    nilname      = strtoname("nil");
    emptyname    = strtoname("emptylist");
    stringname   = strtoname("string");
    functionname = strtoname("function");
  }
}

static Value get_string(Tokens *litp, const char *input) {
    enum tokentype first = first_token_type(*litp);
    if (first != TU32) {
      printf(input);
    }
    uint32_t length = tokens_get_int(litp, input);
    StringBuffer buf = Vmstring_buffer(length);
    for (int i = 0; i < (int) length; i++) {
      uint32_t c = tokens_get_int(litp, input);
      Vmstring_putc(buf, c);
    }
    Value str = mkStringValue(Vmstring_of_buffer(&buf));
    return str;
}

// dont wanna call strtoname everytime
static Value get_literal(Tokens *litp, const char *input) {
  Tokens* rest = litp;
  enum tokentype first = first_token_type(*rest);
  switch (first) {
    case TNAME:
      {
        Name name = tokens_get_name(rest, input);
        if(strtoname("true") == name) {
          return mkBooleanValue(true);
        } else if (strtoname("false") == name) {
          return mkBooleanValue(false);
        } else if (strtoname("nil") == name) {
          return nilValue;
        } else if (strtoname("emptylist") == name) {
          return emptylistValue;
        } else {
          return get_string(rest, input);
        }
      }
      //if nothing matches?? pulling the wrong typeis a crt
      
    case TU32:
      {      
        uint32_t num = tokens_get_int(rest, input);
        return mkNumberValue((double) num);
      }
    case TDOUBLE:
      {      
        printf("in tdouble\n");
        double num = tokens_get_signed_number(rest, input);
        return mkNumberValue(num);
      }
  }
  //free rest
  return nilValue;
}



Instruction parseR1LIT(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  initnames(); // before comparing names, you must call this function
  uint8_t regX = tokens_get_byte(&operands, NULL);
  char* buffer = malloc(256 * sizeof(*buffer));
  uint16_t immediate = literal_slot(vm, get_literal(&operands, buffer));
  assert(operands == NULL);
  SEE(regX);
  free(buffer);
  return eR1U16(opcode, regX, immediate);
}


