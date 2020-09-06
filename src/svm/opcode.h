#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

typedef enum opcode { 
                      Halt, // R0
                      Print, // R1
                      Check, Expect, // R1LIT
                      Unimp, // stand-in for opcodes not yet implemented
} Opcode;


#endif
