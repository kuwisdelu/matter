
#ifndef UTILS
#define UTILS

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cstdlib>

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

#endif // UTILS
