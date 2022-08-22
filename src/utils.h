
#ifndef UTILS
#define UTILS

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

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

inline bool isNA(long x)
{
    return ((int)(x)) == NA_INTEGER || ((int)(x)) == NA_LOGICAL;
}

inline bool isNA(double x)
{
    return ISNA(x) || ISNAN(x);
}

inline bool isNA(SEXP x)
{
    return x == NA_STRING;
}

//// Misc utilities
//--------------------

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
