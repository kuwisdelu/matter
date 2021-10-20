#ifndef BSEARCH
#define BSEARCH

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cmath>
#include <cstring>
#include <climits>

#define ABS_DIFF 	1
#define REL_DIFF_X	2
#define REL_DIFF_Y	3

extern "C" {

	SEXP binarySearch(SEXP key, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest);

	SEXP fastMatch(SEXP key, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest);

}

// counts matches in 2 strings until there's a non-match
int strmatch(const char * s1, const char * s2);

// computes absolute or relative distance between x and y
double dblcmp(double x, double y, int relative = ABS_DIFF);

// does a fuzzy binary search of 'values' for a vector of 'keys'
template<int SType>
SEXP map_binary_search(SEXP key, SEXP values, double tol,
	int tol_ref, int nomatch, bool nearest);

// does a fuzzy binary search of 'values' for a single 'key'
template<typename T>
size_t binary_search(T key, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest);

// returns the absolute or relative difference between two values
template<typename T>
double relative_diff(T x, T y, double tol, int tol_ref);

// returns whether two values are approximately equal or not
template<typename T>
bool approx_equals(T x, T y, double tol, int tol_ref);

// does a fuzzy sequential search of 'values' for a vector of 'keys'
template<typename TKey, typename TIndex>
size_t fast_match(TKey * keys, TKey * values, TIndex * index_out,
	size_t len_keys, size_t len_values, double tol, int tol_ref,
	int nomatch, bool nearest)
{
	bool sorted_keys = TRUE, sorted_values = TRUE;
	for ( int i = 0, j = 0; i < len_keys; i++ )
	{
		if ( ISNA(keys[i]) )
			index_out[i] = nomatch;
		else
		{
			if ( i > 0 && keys[i] < keys[i - 1] )
				sorted_keys = FALSE;
			for ( ; j < len_values; j++ )
			{
				if ( j > 0 && values[j] < values[j - 1] )
					sorted_values = FALSE;
				// implement me!
			}
		}
	}
}

#endif // BSEARCH
