#ifndef MATTER_SEARCH
#define MATTER_SEARCH

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cmath>
#include <cfloat>

#define ABS_DIFF 	1
#define REL_DIFF_X	2
#define REL_DIFF_Y	3

#define NO_DUPS		1
#define SUM_DUPS	2
#define MAX_DUPS	3
#define MIN_DUPS	4

typedef ptrdiff_t index_t;

extern "C" {

	SEXP relativeDiff(SEXP x, SEXP y, SEXP ref);

	SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest);

	SEXP keyvalSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP dups, SEXP sorted);

}

// return the absolute or relative (signed) change between two values
template<typename T>
double rel_change(T x, T y, int ref = ABS_DIFF);

// return the absolute or relative (unsigned) difference between two values
template<typename T>
double rel_diff(T x, T y, int ref = ABS_DIFF);

// return whether two values are approximately equal (within tolerance)
template<typename T>
bool approx_eq(T x, T y, double tol = FLT_EPSILON, int tol_ref = ABS_DIFF);

// return whether a sequence is sorted
template<typename T, int S>
bool is_sorted(SEXP x, bool strictly = FALSE);

// fuzzy binary search returning position of 'x' in 'table'
template<typename T, int S>
index_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest);

// fuzzy binary search returning positions of all 'x' in 'table'
template<typename T, int S>
SEXP do_binary_search(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest);

// fuzzy key-value search returning all 'value's matching 'keys' in 'x'
template<typename Tkey, int Skey, typename Tval, int Sval>
SEXP do_keyval_search(SEXP x, SEXP keys, SEXP values, double tol, int tol_ref,
	Tval nomatch, int dups = NO_DUPS, bool sorted = FALSE);

#endif // MATTER_SEARCH
