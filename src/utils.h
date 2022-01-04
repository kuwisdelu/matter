
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

//// Data access templates
//-------------------------

template<typename T>
T * DataPtr(SEXP x);

template<typename T>
T DataElt(SEXP x, size_t i);

template<typename T>
void SetDataElt(SEXP x, size_t i, T value);

//// Missingness
//---------------

template<typename T>
T NA();

bool isNA(Rbyte x);

bool isNA(int x);

bool isNA(double x);

#endif // UTILS
