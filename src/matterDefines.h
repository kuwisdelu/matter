
#ifndef MATTER_DEFINES
#define MATTER_DEFINES

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

// uncomment to get debug messages
// #define MATTER_DEBUG

#ifdef MATTER_DEBUG
	#define MTDEBUG0(x) Rprintf((x))
	#define MTDEBUG1(x, i) Rprintf((x), (i))
	#define MTDEBUG2(x, i, j) Rprintf((x), (i), (j))
#else
	#define MTDEBUG0(x)
	#define MTDEBUG1(x, i)
	#define MTDEBUG2(x, i, j)
#endif

//// Constants
//--------------

// most of these must match factor levels passed from R

// relative comparison
#define ABS_DIFF    1
#define REL_DIFF_X  2
#define REL_DIFF_Y  3

// R-level data types
#define R_RAW		1
#define R_LOGICAL	2
#define R_INTEGER	3
#define R_DOUBLE	4
#define R_CHARACTER	5
#define R_LIST		6

// C-level data types
#define C_CHAR		1
#define C_UCHAR		2
#define C_SHORT		3
#define C_USHORT	4
#define C_INT		5
#define C_UINT		6
#define C_LONG		7
#define C_ULONG		8
#define C_FLOAT		9
#define C_DOUBLE	10

// Arith
#define OP_ADD		1	// +
#define OP_SUB		2	// -
#define OP_MUL		3	// *
#define OP_EXP		4	// ^
#define OP_MOD		5	// %%
#define OP_IDIV		6	// %/%
#define OP_DIV		7	// /

// Compare
#define CMP_EQ		8	// ==
#define CMP_GT		9	// >
#define CMP_LT		10	// <
#define CMP_NE		11	// !=
#define CMP_LE		12	// <=
#define CMP_GE		13	// >=

// Logic
#define LGL_AND		14	// &
#define LGL_OR		15	// |

// Math
#define MATH_LOG	16
#define MATH_LOG10	17
#define MATH_LOG2	18
#define MATH_LOG1P	18
#define MATH_EXP	19

// Summary
#define STAT_MAX		1
#define STAT_MIN		2
#define STAT_RANGE		3
#define STAT_PROD		4
#define STAT_SUM		5
#define STAT_ANY		6
#define STAT_ALL		7
#define STAT_MEAN		8
#define STAT_VAR		9
#define STAT_SD			10
#define STAT_NNZ		11

// Limits
#define NA_CHAR CHAR_MIN
#define R_CHAR_MIN (1+CHAR_MIN)
#define R_CHAR_MAX CHAR_MAX

#define NA_SHORT INT16_MIN
#define R_SHORT_MIN (1+INT16_MIN)
#define R_SHORT_MAX INT16_MAX

#define R_INT_MIN (1+INT32_MIN)
#define R_INT_MAX INT32_MAX

#define R_DOUBLE_MIN R_NegInf
#define R_DOUBLE_MAX R_PosInf

#define R_UCHAR_MAX UCHAR_MAX
#define R_USHORT_MAX UINT16_MAX
#define R_UINT_MAX UINT32_MAX
#define R_ULONG_MAX UINT64_MAX

#define NA_LONG INT64_MIN
#define R_LONG_MIN (1+INT64_MIN)
#define R_LONG_MAX INT64_MAX

//// Typedefs
//-------------------

typedef ptrdiff_t index_t;

//// Pair of values
//------------------

template<typename T1, typename T2>
struct Pair {
	T1 first;
	T2 second;
};

//// Data accessor template
//-------------------------

template<typename T>
T * DataPtr(SEXP x)
{
	return ((T *)(DATAPTR(x)));
}

inline index_t IndexElt(SEXP indx, index_t i)
{
	if ( i == NA_INTEGER )
		return NA_INTEGER;
	switch(TYPEOF(indx)) {
		case INTSXP: {
			int j = INTEGER_ELT(indx, i);
			if ( j == NA_INTEGER )
				return NA_INTEGER;
			else
				return static_cast<index_t>(j);
		}
		case REALSXP: {
			double j = REAL_ELT(indx, i);
			if ( ISNA(j) || ISNAN(j) )
				return NA_INTEGER;
			else
				return static_cast<index_t>(j);
		}
		default:
			Rf_error("invalid index type");
	}
}

//// Comparison
//--------------

// absolute or relative (signed) change
template<typename T>
double rel_change(T x, T y, int ref = ABS_DIFF)
{
	switch(ref) {
		case ABS_DIFF:
			return static_cast<double>(x - y);
		case REL_DIFF_X:
			return static_cast<double>(x - y) / x;
		case REL_DIFF_Y:
			return static_cast<double>(x - y) / y;
		default:
			return NA_REAL;
	}
}

template<> inline
double rel_change(const char * x, const char * y, int ref)
{
	int i = -1, sign = 1;
	int n = 0, nx = 0, ny = 0;
	// count number of initial matched characters
	while ( x[nx] != '\0' || y[ny] != '\0' ) {
		if ( x[nx] != y[ny] && i < 0 ) {
			i = nx > ny ? nx : ny;
			sign = x[nx] < y[ny] ? -1 : 1;
		}
		if ( x[nx] != '\0' )
			nx++;
		if ( y[ny] != '\0' )
			ny++;
	}
	// check string lengths
	n = nx > ny ? nx : ny;
	i = i < 0 ? n : i;
	// return character-based difference
	switch(ref) {
		case ABS_DIFF:
			return sign * static_cast<double>(n - i);
		case REL_DIFF_X:
			return sign * static_cast<double>(n - i) / nx;
		case REL_DIFF_Y:
			return sign * static_cast<double>(n - i) / ny;
		default:
			return NA_REAL;
	}
}

template<> inline
double rel_change(SEXP x, SEXP y, int ref)
{
	if ( TYPEOF(x) != TYPEOF(y) )
		Rf_error("'x' and 'y' must have the same type");
	switch(TYPEOF(x)) {
		case CHARSXP:
			return rel_change(CHAR(x), CHAR(y), ref);
		case STRSXP:
			return rel_change(CHAR(Rf_asChar(x)), CHAR(Rf_asChar(y)), ref);
		case INTSXP:
			return rel_change(Rf_asInteger(x), Rf_asInteger(y), ref);
		case REALSXP:
			return rel_change(Rf_asReal(x), Rf_asReal(y), ref);
		default:
			Rf_error("unsupported data type");
	}
}

// absolute or relative (unsigned) difference
template<typename T>
double rel_diff(T x, T y, int ref = ABS_DIFF)
{
	return std::fabs(rel_change<T>(x, y, ref));
}

inline int switch_tol_ref(int tol_ref)
{
	switch(tol_ref) {
		case ABS_DIFF:
			return ABS_DIFF;
		case REL_DIFF_X:
			return REL_DIFF_Y;
		case REL_DIFF_Y:
			return REL_DIFF_X;
	}
	return NA_INTEGER;
}

template<typename T>
inline bool equal(T x, T y, double tol = DBL_EPSILON)
{
	return rel_diff<T>(x, y) <= tol;
}

//// Generate NA
//---------------

template<typename T>
T NA();

template<> inline
char NA<char>()
{
	Rf_error("NAs not supported for type 'char'");
}

template<> inline
Rbyte NA<Rbyte>()
{
	Rf_error("NAs not supported for type 'Rbyte'");
}

template<> inline
int NA<int>()
{
	return NA_INTEGER;
}

template<> inline
double NA<double>()
{
	return NA_REAL;
}

template<> inline
SEXP NA<SEXP>()
{
	return NA_STRING;
}

template<> inline
index_t NA<index_t>()
{
	return NA_INTEGER;
}

//// Check for NA
//----------------

inline bool isNA(Rbyte x)
{
	return FALSE;
}

inline bool isNA(int x)
{
	return x == NA_INTEGER || x == NA_LOGICAL;
}

inline bool isNA(double x)
{
	return ISNA(x) || ISNAN(x);
}

inline bool isNA(index_t x)
{
	return ((int)(x)) == NA_INTEGER || ((int)(x)) == NA_LOGICAL;
}

inline bool isNA(SEXP x)
{
	return x == NA_STRING;
}

//// Misc utilities
//--------------------

// fill a buffer with a value
template<typename T>
void fill(T * buffer, size_t size, T val, int stride = 1)
{
	for ( size_t i = 0; i < size; i++ )
		buffer[stride * i] = val;
}

// FIXME: Temporary (slow) solution as R_compact_seq_range() is non-API
inline SEXP seq_range(size_t start, size_t end)
{
	SEXP ans;
	size_t len = end - start + 1;
	PROTECT(ans = Rf_allocVector(INTSXP, len));
	int * pa = INTEGER(ans);
	for ( size_t i = 0; i < len; i++ )
		pa[i] = start + i;
	UNPROTECT(1);
	return ans;
}

// FIXME: Temporary solution as Rf_ExtractSubset() is non-API
template<typename T>
SEXP extract_range(SEXP x, size_t start, size_t end)
{
	SEXP ans;
	size_t len = end - start + 1;
	PROTECT(ans = Rf_allocVector(TYPEOF(x), len));
	T * pa = DataPtr<T>(ans);
	T * px = DataPtr<T>(x);
	for ( size_t i = 0; i < len; i++ )
		pa[i] = px[start + i - 1];
	UNPROTECT(1);
	return ans;
}

inline SEXP extract_range(SEXP x, size_t start, size_t end)
{
	switch(TYPEOF(x)) {
		case INTSXP:
			return extract_range<int>(x, start, end);
		case REALSXP:
			return extract_range<double>(x, start, end);
		default:
			Rf_error("unsupported data type");
	}
}

#endif // MATTER_DEFINES
