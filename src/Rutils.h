
#ifndef UTILS
#define UTILS

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

//// Constants
//-------------

#define ABS_DIFF    1
#define REL_DIFF_X  2
#define REL_DIFF_Y  3

#define R_RAW		1
#define R_LOGICAL	2
#define R_INTEGER	3
#define R_DOUBLE	4
#define R_STRING	5
#define R_LIST		6

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

//// Indexing types
//-------------------

typedef ptrdiff_t index_t;

typedef double Rindex_t;

#define INDEX_PTR(x) ((Rindex_t *)(DATAPTR(x)))

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

//// Generate NA
//---------------

template<typename T>
T NA();

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

//// Misc utilities
//--------------------

template<typename T>
size_t fill(T * buffer, size_t n, T val, size_t stride = 1)
{
	size_t count = 0;
	for ( size_t i = 0; i < n; i++ )
		buffer[stride * i] = val;
	return count;
}

// FIXME: Temporary (slow) solution as R_compact_intrange() is non-API
inline SEXP intrange(size_t start, size_t end)
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
SEXP extractrange(SEXP x, size_t start, size_t end)
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

inline SEXP extractrange(SEXP x, size_t start, size_t end)
{
	switch(TYPEOF(x)) {
		case INTSXP:
			return extractrange<int>(x, start, end);
		case REALSXP:
			return extractrange<double>(x, start, end);
		default:
			Rf_error("unsupported data type");
	}
}

#endif // UTILS
