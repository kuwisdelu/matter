#ifndef MATTER_SEARCH
#define MATTER_SEARCH

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cmath>
#include <cfloat>

#include "utils.h"
#include "vectools.h"

#define ABS_DIFF 	1
#define REL_DIFF_X	2
#define REL_DIFF_Y	3

#define EST_NEAR	1
#define EST_AVG		2
#define EST_SUM		3
#define EST_MAX		4
#define EST_MIN		5
#define EST_LERP	6
#define EST_GAUS	7

typedef ptrdiff_t index_t;

extern "C" {

	SEXP relativeDiff(SEXP x, SEXP y, SEXP ref);

	SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest);

	SEXP approxSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP interp, SEXP sorted);

}

// return the absolute or relative (signed) change between two values
template<typename T>
double rel_change(T x, T y, int ref = ABS_DIFF)
{
	switch(ref) {
		case ABS_DIFF:
			return static_cast<double>(x - y);
		case REL_DIFF_X:
			return static_cast<double>(x - y) / x;
		case REL_DIFF_Y:
			return static_cast<double>(x - y) / y;
		default:
			return NA_REAL;
	}
}

template<> inline
double rel_change<const char *>(const char * x, const char * y, int ref)
{
	int i = -1, sign = 1;
	int n = 0, nx = 0, ny = 0;
	while ( x[nx] != '\0' || y[ny] != '\0' ) {
		if ( x[nx] != y[ny] && i < 0 ) {
			i = nx > ny ? nx : ny;
			sign = x[nx] < y[ny] ? -1 : 1;
		}
		if ( x[nx] != '\0' )
			nx++;
		if ( y[ny] != '\0' )
			ny++;
	}
	n = nx > ny ? nx : ny;
	i = i < 0 ? n : i;
	switch(ref) {
		case ABS_DIFF:
			return sign * static_cast<double>(n - i);
		case REL_DIFF_X:
			return sign * static_cast<double>(n - i) / nx;
		case REL_DIFF_Y:
			return sign * static_cast<double>(n - i) / ny;
		default:
			return NA_REAL;
	}
}

template<> inline
double rel_change<SEXP>(SEXP x, SEXP y, int ref)
{
	return rel_change(CHAR(x), CHAR(y), ref);
}

// return the absolute or relative (unsigned) difference between two values
template<typename T>
double rel_diff(T x, T y, int ref = ABS_DIFF)
{
	return fabs(rel_change<T>(x, y, ref));
}

// return whether two values are approximately equal (within tolerance)
template<typename T>
bool approx_eq(T x, T y, double tol = FLT_EPSILON, int tol_ref = ABS_DIFF)
{
	return rel_diff<T>(x, y, tol_ref) < tol;
}

// return whether a sequence is sorted
template<typename T>
bool is_sorted(SEXP x, bool strictly = FALSE)
{
	R_xlen_t len = XLENGTH(x);
	T * pX = DataPtr<T>(x);
	for ( size_t i = 1; i < len; i++ ) {
		double diff = rel_change(pX[i], pX[i - 1]);
		if ( diff < 0 || (strictly && diff <= 0) )
			return FALSE;
	}
	return TRUE;
}

// fuzzy binary search returning position of 'x' in 'table'
template<typename T>
index_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest, int err = -1)
{
	double diff;
	index_t min = start, max = end, mid = nomatch;
	T * pTable = DataPtr<T>(table);
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		double d1 = rel_change(pTable[start], pTable[mid]);
		double d2 = rel_change(pTable[mid], pTable[end - 1]);
		if ( d1 > 0 || d2 > 0 )
			return err; // table is not sorted
		diff = rel_change(x, pTable[mid], tol_ref);
		if ( diff < 0 )
			end = mid;
		else if ( diff > 0 )
			start = mid + 1;
		else
			return mid;
	}
	if ( nearest || tol > 0 ) {
		index_t left = mid >= min + 1 ? mid - 1 : min;
		index_t right = mid < max - 1 ? mid + 1 : max - 1;
		double dleft = rel_diff(x, pTable[left], tol_ref);
		double dmid = rel_diff(x, pTable[mid], tol_ref);
		double dright = rel_diff(x, pTable[right], tol_ref);
		if ( (mid == left && diff < 0) && (nearest || dleft < tol) )
			return left;
		else if ( (mid == right && diff > 0) && (nearest || dright < tol) )
			return right;
		else {
			if ( (dleft <= dmid && dleft <= dright) && (nearest || dleft < tol) )
				return left;
			else if ( (dmid <= dleft && dmid <= dright) && (nearest || dmid < tol) )
				return mid;
			else if ( nearest || dright < tol )
				return right;
		}
	}
	return nomatch;
}

template<typename T>
size_t do_binary_search(int * ptr, SEXP x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = FALSE,
	bool index1 = FALSE, int err = -1)
{
	R_xlen_t len = XLENGTH(x);
	T * pX = DataPtr<T>(x);
	R_xlen_t n = XLENGTH(table);
	size_t num_matches = 0;
	for ( size_t i = 0; i < len; i++ ) {
		if ( isNA(pX[i]) )
			ptr[i] = nomatch;
		else
			ptr[i] = binary_search<T>(pX[i], table, 0, n,
				tol, tol_ref, nomatch, nearest, err);
		if ( ptr[i] != nomatch ) {
			num_matches++;
			ptr[i] += index1; // adjust for R indexing from 1
		}
	}
	return num_matches;
}

template<typename T>
SEXP do_binary_search(SEXP x, SEXP table, double tol, int tol_ref,
	int nomatch, bool nearest = FALSE, bool index1 = FALSE)
{
	R_xlen_t len = XLENGTH(x);
	SEXP pos;
	if ( IS_LONG_VEC(table) ) {
		PROTECT(pos = Rf_allocVector(REALSXP, len));
	} else {
		PROTECT(pos = Rf_allocVector(INTSXP, len));
	}
	int * pPos = INTEGER(pos);
	do_binary_search<T>(pPos, x, table, 0, len,
		tol, tol_ref, nomatch, nearest, index1);
	UNPROTECT(1);
	return pos;
}

// fuzzy key-value search returning all 'value's with 'keys' matching 'x'
template<typename TKey, typename TVal>
Pair<index_t,TVal> approx_search(TKey x, SEXP keys, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, TVal nomatch, int interp = EST_NEAR, bool sorted = TRUE)
{
	TKey * pKeys = DataPtr<TKey>(keys);
	TVal * pValues = DataPtr<TVal>(values);
	index_t pos = NA_INTEGER;
	TVal retval = nomatch;
	size_t num_matches = 0;
	Pair<index_t,TVal> result;
	if ( !isNA(x) )
	{
		if ( sorted ) {
			pos = binary_search<TKey>(x, keys, start, end,
				tol, tol_ref, NA_INTEGER, FALSE);
			if ( interp == EST_NEAR || isNA(pos) || pos < 0 )
			{
				if ( !isNA(pos) && pos >= 0 )
					retval = pValues[pos];
				result = {pos, retval};
				return result;
			}
			start = pos;
		}
		double bounding_diff[] = {DBL_MAX, DBL_MAX};
		int bounding_pos[] = {NA_INTEGER, NA_INTEGER};
		double diff_min = DBL_MAX;
		double wt = 1, wscale = 0;
		for ( index_t i = start; i < end; i++ )
		{
			double delta = rel_change<TKey>(x, pKeys[i], tol_ref);
			double diff = fabs(delta);
			if ( diff <= tol )
			{
				switch(interp) {
					case EST_NEAR:
						retval = diff < diff_min ? pValues[i] : retval;
						break;
					case EST_GAUS:
						wt = kgaussian(diff, (2 * tol + 1) / 4);
					case EST_AVG:
					case EST_SUM:
						retval = num_matches > 0 ? wt * pValues[i] + retval : wt * pValues[i];
						wscale += wt;
						break;
					case EST_MAX:
						retval = (num_matches == 0 || pValues[i] > retval) ? pValues[i] : retval;
						break;
					case EST_MIN:
						retval = (num_matches == 0 || pValues[i] < retval) ? pValues[i] : retval;
						break;
					case EST_LERP:
					{
						if ( delta > 0 && diff < bounding_diff[0] )
						{
							bounding_diff[0] = diff;
							bounding_pos[0] = i;
						}
						if ( delta < 0 && diff < bounding_diff[1] )
						{
							bounding_diff[1] = diff;
							bounding_pos[1] = i;
						}
						break;
					}
				}
				num_matches++;
				if ( diff < diff_min )
				{
					diff_min = diff;
					pos = i;
					if ( interp == EST_NEAR && diff_min == 0 )
						break;
				}
			}
			else if ( sorted )
				break;
		}
		for ( index_t i = start - 1; sorted && i >= 0; i-- )
		{
			double delta = rel_change<TKey>(x, pKeys[i], tol_ref);
			double diff = fabs(delta);
			if ( diff <= tol )
			{
				switch(interp) {
					case EST_NEAR:
						retval = diff < diff_min ? pValues[i] : retval;
						break;
					case EST_GAUS:
						wt = kgaussian(diff, (2 * tol + 1) / 4);
					case EST_AVG:
					case EST_SUM:
						retval = num_matches > 0 ? wt * pValues[i] + retval : wt * pValues[i];
						wscale += wt;
						break;
					case EST_MAX:
						retval = (num_matches == 0 || pValues[i] > retval) ? pValues[i] : retval;
						break;
					case EST_MIN:
						retval = (num_matches == 0 || pValues[i] < retval) ? pValues[i] : retval;
						break;
					case EST_LERP:
					{
						if ( delta > 0 && diff < bounding_diff[0] )
						{
							bounding_diff[0] = diff;
							bounding_pos[0] = i;
						}
						if ( delta < 0 && diff < bounding_diff[1] )
						{
							bounding_diff[1] = diff;
							bounding_pos[1] = i;
						}
						break;
					}
				}
				num_matches++;
			}
			else if ( sorted )
				break;
		}
		if ( !isNA(pos) )
		{
			switch(interp) {
				case EST_AVG:
				case EST_GAUS:
					retval = retval / wscale;
					break;
				case EST_LERP:
				{
					int i0 = !isNA(bounding_pos[0]) ? bounding_pos[0] : pos;
					int i1 = !isNA(bounding_pos[1]) ? bounding_pos[1] : pos;
					TKey x0 = pKeys[i0], x1 = pKeys[i1];
					TVal y0 = pValues[i0], y1 = pValues[i1];
					double dx = rel_change(x1, x0, ABS_DIFF);
					double dy = rel_change(y1, y0, ABS_DIFF);
					double tx = rel_change(x, x0, ABS_DIFF);
					if ( i0 != i1 && diff_min > 0 )
						retval = y0 + (dy / dx) * tx;
					else
						retval = pValues[pos];
					break;
				}
			}
		}
	}
	result = {pos, retval};
	return result;
}

template<typename TKey, typename TVal>
size_t do_approx_search(TVal * ptr, TKey * x, size_t xlen, SEXP keys, SEXP values,
	size_t start, size_t end, double tol, int tol_ref, TVal nomatch,
	int interp = EST_NEAR, bool sorted = TRUE)
{
	size_t num_matches = 0;
	index_t i = 0, current = start;
	while ( i < xlen )
	{
		ptr[i] = nomatch;
		if ( !isNA(x[i]) )
		{
			Pair<index_t,TVal> result = approx_search(x[i], keys, values,
				current, end, tol, tol_ref, nomatch, interp, sorted);
			if ( !isNA(result.first) ) {
				if ( result.first < 0 ) {
					sorted = FALSE; // fall back to linear search
					current = start; // reset search space
					continue;
				}
				current = result.first;
				ptr[i] = result.second;
				num_matches++;
			}
			if ( !sorted || (i + 1 < xlen && x[i + 1] < x[i]) )
				current = start; // reset if either 'x' or 'keys' is unsorted
		}
		i++;
	}
	return num_matches;
}

template<typename TKey, typename TVal>
SEXP do_approx_search(SEXP x, SEXP keys, SEXP values, double tol, int tol_ref,
	TVal nomatch, int interp = EST_NEAR, bool sorted = TRUE)
{
	R_xlen_t xlen = XLENGTH(x);
	R_xlen_t keylen = XLENGTH(keys);
	SEXP result;
	PROTECT(result = Rf_allocVector(TYPEOF(values), xlen));
	TVal * pResult = DataPtr<TVal>(result);
	TKey * pX = DataPtr<TKey>(x);
	do_approx_search<TKey, TVal>(pResult, pX, xlen, keys, values,
		0, keylen, tol, tol_ref, nomatch, interp, sorted);
	UNPROTECT(1);
	return result;
}

#endif // MATTER_SEARCH
