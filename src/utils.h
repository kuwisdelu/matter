
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

template<typename T>
T DataElt(SEXP x, index_t i)
{
	return ((T *)(DATAPTR(x)))[i];
}

inline index_t IndexElt(SEXP indx, index_t i)
{
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
