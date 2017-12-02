
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

#define R_INDEX_PTR(x) REAL(x)

// pair of numerics

struct pair_int {
	int first;
	int second;
};

struct pair_double {
	double first;
	double second;
};

// Matter options

struct MATTER_OPTIONS {
	bool cast_warning;
};

// S4 class codes

#define MATTER_ANY 0
#define MATTER_MATC 1
#define MATTER_MATR 2
#define MATTER_LIST 3

// R-level datamodes (must match factor levels)

#define R_RAW			1
#define R_LOGICAL		2
#define R_INTEGER       3
#define R_NUMERIC       4
#define R_LIST       	5

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

// Delayed operation OP codes (must match factor levels)

#define OP_ADD			1	// +
#define OP_SUB			2	// -
#define OP_MUL			3	// *
#define OP_DIV			4	// /
#define OP_EXP			5	// ^ 	lhs = base, rhs = exp
#define OP_MOD			6	// %%
#define OP_IDIV			7	// %/%
#define OP_EQ			8	// ==
#define OP_NE			9	// !=
#define OP_GT			10	// >
#define OP_LT			11	// <
#define OP_GE			12	// >=
#define OP_LE			13	// <=
#define OP_AND			14	// &
#define OP_OR			15	// |
#define OP_LOG			16	// log 	lhs = base, rhs = x

// Delayed operation WHERE codes

#define BY_GROUP				1
#define BY_EACH_GROUP			2

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
