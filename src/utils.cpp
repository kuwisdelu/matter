
#include "utils.h"

//// Data pointer access
//----------------------

template<>
Rbyte * DataPtr<Rbyte>(SEXP x)
{
	return ((Rbyte *)(DATAPTR(x)));
}

template<>
int * DataPtr<int>(SEXP x)
{
	return ((int *)(DATAPTR(x)));
}

template<>
double * DataPtr<double>(SEXP x)
{
	return ((double *)(DATAPTR(x)));
}

//// Get data element
//--------------------

template<typename T>
T * DataElt(SEXP x, size_t i)
{
	return ((double *)(DATAPTR(x)))[i];
}

template<>
const char * DataElt<const char *>(SEXP x, size_t i)
{
	return CHAR(STRING_ELT(x, i));
}

//// Set data element
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
Rbyte NA<Rbyte>()
{
	Rf_error("NAs not supported for type 'Rbyte'");
	return 0;
}

template<>
int NA<int>()
{
	return NA_INTEGER;
}

template<>
double NA<double>()
{
	return NA_REAL;
}

//// Test missingness
//--------------------

bool isNA(Rbyte x)
{
    return FALSE;
}

bool isNA(int x)
{
    return x == NA_INTEGER || x == NA_LOGICAL;
}

bool isNA(double x)
{
    return ISNA(x) || ISNAN(x);
}
