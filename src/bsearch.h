#ifndef BSEARCH
#define BSEARCH

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cmath>
#include <cstring>

#define ABS_DIFF 	1
#define REL_DIFF_X	2
#define REL_DIFF_Y	3

extern "C" {

	SEXP binarySearch(SEXP key, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest);

}

// counts matches in 2 strings until there's a non-match
int strmatch(const char * s1, const char * s2);

// computes absolute or relative distance between x and y
double dblcmp(double x, double y, int relative = ABS_DIFF);

// does a fuzzy binary search of 'values' for a vector of keys
template<int SType>
SEXP map_binary_search(SEXP key, SEXP values, double tol,
	int tol_ref, int nomatch, bool nearest);

// does a fuzzy binary search of 'values' for a single key
template<typename T>
size_t binary_search(T key, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest);

#endif // BSEARCH
