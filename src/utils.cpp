
#include "utils.h"

extern "C" {

	void swap_bytes(void * pntr, size_t n)
	{
	    char * p = (char *) pntr;
	    size_t lo, hi;
	    for ( lo = 0, hi = n - 1; lo < hi; lo++, hi-- )
	    {
	        char tmp = p[lo];
	        p[lo] = p[hi];
	        p[hi] = tmp;
	    }
	}

}

//// Data Pointer Access
//----------------------

template<>
Rbyte * DataPtr<Rbyte>(SEXP x)
{
	return static_cast<Rbyte *>(DATAPTR(x));
}

template<>
int * DataPtr<int>(SEXP x)
{
	return static_cast<int *>(DATAPTR(x));
}

template<>
double * DataPtr<double>(SEXP x)
{
	return static_cast<double *>(DATAPTR(x));
}

//// Get Data Element
//--------------------

template<typename T>
T * DataElt(SEXP x, size_t i)
{
	return static_cast<double *>(DATAPTR(x));
}

template<>
const char * DataElt<const char *>(SEXP x, size_t i)
{
	return CHAR(STRING_ELT(x, i));
}

//// Set Data Element
//--------------------

template<typename T>
void SetDataElt(SEXP x, size_t i, T value)
{
	DataPtr<T>(x)[i] = value;
}

template<>
void SetDataElt<const char *>(SEXP x, size_t i, const char * value)
{
	SET_STRING_ELT(x, i, Rf_mkChar(value));
}

//// Missing values
//------------------

template<>
Rbyte DataNA<Rbyte>()
{
	Rf_error("NAs unsupported for type 'Rbyte'");
	return 0;
}

template<>
int DataNA<int>()
{
	return NA_INTEGER;
}

template<>
double DataNA<double>()
{
	return NA_REAL;
}

template<>
SEXP DataNA<SEXP>()
{
	return NA_STRING;
}

//// Test missingness
//--------------------

template<>
bool IsNA<Rbyte>(Rbyte x)
{
	return false;
}

template<>
bool IsNA<int>(int x)
{
	return x == NA_INTEGER || x == NA_LOGICAL;
}

template<>
bool IsNA<double>(double x)
{
	return ISNA(x) || ISNAN(x);
}

template<>
bool IsNA<SEXP>(SEXP x)
{
	return x == NA_STRING;
}

