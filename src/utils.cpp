
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

template<>
Rbyte * DataPtr<Rbyte>(SEXP x)
{
	return RAW(x);
}

template<>
int * DataPtr<int>(SEXP x)
{
	return INTEGER(x);
}

template<>
double * DataPtr<double>(SEXP x)
{
	return REAL(x);
}

template<>
SEXPTYPE DataType<Rbyte>()
{
	return RAWSXP;
}

template<>
SEXPTYPE DataType<int>()
{
	return INTSXP;
}

template<>
SEXPTYPE DataType<double>()
{
	return REALSXP;
}

template<>
Rbyte DataNA<Rbyte>()
{
	error("NAs unsupported for type RAW");
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
