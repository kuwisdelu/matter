
#ifndef UTILS
#define UTILS

#include <R.h>
#include <Rdefines.h>

#include <cstdlib>

struct Bool { int x; };

typedef struct Bool Bool;

#define SWAP(x) swap_bytes(&x, sizeof(x));

extern "C" {

	void swap_bytes(void * pntr, size_t n);

}

template<typename T, int S>
T * DataPtr(SEXP x);

template<typename T>
T DataNA();

#endif // UTILS
