
#ifndef UTILS
#define UTILS

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cstdlib>

#define SWAP(x) swap_bytes(&x, sizeof(x));

extern "C" {

	void swap_bytes(void * pntr, size_t n);

}

template<typename T, int S>
T * DataPtr(SEXP x);

template<typename T>
T DataElt(SEXP x, size_t i);

template<typename T>
T DataNA();

template<typename T>
bool IsNA(T x);

#endif // UTILS
