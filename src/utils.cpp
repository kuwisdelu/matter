
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
int DataElt<int>(SEXP x, size_t i)
{
	return INTEGER(x)[i];
}

template<>
double DataElt<double>(SEXP x, size_t i)
{
	return REAL(x)[i];
}

template<>
const char * DataElt<const char *>(SEXP x, size_t i)
{
	return CHAR(STRING_ELT(x, i));
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

template<>
SEXP DataNA<SEXP>()
{
	return NA_STRING;
}

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

