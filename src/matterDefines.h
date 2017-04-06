
#ifndef MATTER_DEFINES
#define MATTER_DEFINES

#include <limits.h>
#include <stdint.h>

extern "C"
{
  #include <Rdefines.h>
}

#define R_INFINITE(x) (!R_FINITE(x) && !ISNA(x) && !ISNAN(x))

// index types

typedef long index_t;
typedef double Rindex_t;

#define INDEX_PTR(x) REAL(x)

// S4 class codes

#define MATTER_VEC 0
#define MATTER_MATC 1
#define MATTER_MATR 2

// R-level datamodes (must match factor levels)

#define R_RAW			1
#define R_INTEGER       2
#define R_NUMERIC       3

// C-level datamodes (must match factor levels)

#define C_CHAR			1
#define C_UCHAR         2
#define C_SHORT         3
#define C_USHORT		4
#define C_INT           5
#define C_UINT			6
#define C_LONG          7
#define C_ULONG			8
#define C_FLOAT         9
#define C_DOUBLE        10

// Delayed operation index codes

#define INDEX_LHS			0
#define INDEX_RHS			1
#define INDEX_OP			2
#define INDEX_WHERE			3

// Delayed operation OP codes

#define OP_ADD			1
#define OP_SUB			2
#define OP_MUL			3
#define OP_DIV			4
#define OP_EXP			5	// lhs = base, rhs = exp
#define OP_LOG			6	//			   rhs = base

// Delayed operation WHERE codes

#define OPS_SCALAR			0
#define OPS_BY_MAJOR_DIM	1
#define OPS_BY_MINOR_DIM	2

// MatterAccessor access

#define NULL_INDEX -99

// Data type limits (matching definitions in bigmemory)

#define NA_CHAR CHAR_MIN
#define NA_SHORT SHRT_MIN
#define R_CHAR_MIN (1+CHAR_MIN)
#define R_CHAR_MAX CHAR_MAX
#define R_SHORT_MIN (1+SHRT_MIN)
#define R_SHORT_MAX SHRT_MAX
#define R_INT_MIN (1+INT_MIN)
#define R_INT_MAX INT_MAX
#define R_DOUBLE_MIN R_NegInf
#define R_DOUBLE_MAX R_PosInf

// Data type limits (NOT matching definitions in bigmemory)

#define R_UCHAR_MAX UCHAR_MAX
#define R_USHORT_MAX USHRT_MAX
#define R_UINT_MAX UINT_MAX
#define R_ULONG_MAX ULONG_MAX

#define NA_LONG LONG_MIN
#define R_LONG_MIN (1+LONG_MIN)
#define R_LONG_MAX LONG_MAX

#endif
