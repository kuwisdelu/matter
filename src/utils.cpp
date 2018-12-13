
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
Rbyte * DataPtr<Rbyte,RAWSXP>(SEXP x)
{
	return RAW(x);
}

template<>
int * DataPtr<int,LGLSXP>(SEXP x)
{
	return LOGICAL(x);
}

template<>
int * DataPtr<int,INTSXP>(SEXP x)
{
	return INTEGER(x);
}

template<>
double * DataPtr<double,REALSXP>(SEXP x)
{
	return REAL(x);
}


template<>
Rbyte DataNA<Rbyte>()
{
	Rf_error("NAs unsupported for type 'Rbyte'");
	return 0;
}

template<>
bool DataNA<bool>()
{
	Rf_error("NAs unsupported for type 'bool'");
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
