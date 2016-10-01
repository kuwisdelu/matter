
#ifndef MATTER_DEFINES_H
#define MATTER_DEFINES_H

#include <limits.h>
#include <stdint.h>

extern "C"
{
  #include <Rdefines.h>
}

#define R_INFINITE(x) (!R_FINITE(x) && !ISNA(x) && !ISNAN(x))

// these match definitions in bigmemory

#define NA_CHAR CHAR_MIN
#define NA_SHORT SHRT_MIN
#define R_INT_MIN (1+INT_MIN)
#define R_INT_MAX INT_MAX
#define R_SHORT_MIN (1+SHRT_MIN)
#define R_SHORT_MAX SHRT_MAX
#define R_CHAR_MIN (1+CHAR_MIN)
#define R_CHAR_MAX CHAR_MAX
#define R_DOUBLE_MIN R_NegInf
#define R_DOUBLE_MAX R_PosInf

// these don't

#define NA_LONG LONG_MIN
#define R_LONG_MIN (1+LONG_MIN)
#define R_LONG_MAX LONG_MAX

#endif
