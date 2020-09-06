#ifndef CHECK_EXPECT
#define CHECK_EXPECT

#include "value.h"

// Implements unit tests
//
// Protocol
//    { { (check expect | check_assert) } report_unit_tests }

void check       (const char *source, Value v);
void expect      (const char *source, Value v);
void check_assert(const char *source, Value v);

void report_unit_tests(void);

// N.B. All strings are C strings.  These functions make private copies at need,
// so callers may move or recover memory.


#endif
