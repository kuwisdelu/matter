
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


//// Get Data Element
//--------------------

template<>
int DataElt<int,INTSXP>(SEXP x, size_t i)
{
	return INTEGER(x)[i];
}

template<>
double DataElt<double,REALSXP>(SEXP x, size_t i)
{
	return REAL(x)[i];
}

template<>
const char * DataElt<const char *,STRSXP>(SEXP x, size_t i)
{
	return CHAR(STRING_ELT(x, i));
}


//// Set Data Element
//--------------------

template<>
void DataSetElt<int,INTSXP>(SEXP x, size_t i, int value)
{
	INTEGER(x)[i] = value;
}

template<>
void DataSetElt<double,REALSXP>(SEXP x, size_t i, double value)
{
	REAL(x)[i] = value;
}

template<>
void DataSetElt<const char *,STRSXP>(SEXP x, size_t i, const char * value)
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

