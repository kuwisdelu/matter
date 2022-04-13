#ifndef MATTER_SEARCH
#define MATTER_SEARCH

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include <cmath>
#include <cfloat>

#include "utils.h"

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

template<> inline
bool is_sorted<const char *>(SEXP x, bool strictly)
{
	R_xlen_t len = XLENGTH(x);
	for ( size_t i = 1; i < len; i++ ) {
		const char * xi = CHAR(STRING_ELT(x, i));
		const char * xim1 = CHAR(STRING_ELT(x, i - 1));
		double diff = rel_change(xi, xim1);
		if ( diff < 0 || (strictly && diff <= 0) )
			return FALSE;
	}
	return TRUE;
}

// fuzzy binary search returning position of 'x' in 'table'
template<typename T>
index_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	index_t min = start, max = end, mid = nomatch;
	T * pTable = DataPtr<T>(table);
	while ( start < end )
	{
		mid = start + (end - start) / 2;
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

template<> inline
index_t binary_search<const char *>(const char * x, SEXP table,
	size_t start, size_t end, double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	index_t min = start, max = end, mid = nomatch;
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		diff = rel_change(x, CHAR(STRING_ELT(table, mid)));
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
		double dleft = rel_diff(x, CHAR(STRING_ELT(table, left)), tol_ref);
		double dmid = rel_diff(x, CHAR(STRING_ELT(table, mid)), tol_ref);
		double dright = rel_diff(x, CHAR(STRING_ELT(table, right)), tol_ref);
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
	bool index1 = FALSE, index_t skip = 1)
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
				tol, tol_ref, nomatch, nearest);
		if ( ptr[i] != nomatch ) {
			num_matches++;
			ptr[i] += index1; // adjust for R indexing from 1
		}
	}
	return num_matches;
}

template<> inline
size_t do_binary_search<const char *>(int * ptr, SEXP x, SEXP table,
	size_t start, size_t end, double tol, int tol_ref,
	int nomatch, bool nearest, bool index1, index_t skip)
{
	R_xlen_t len = XLENGTH(x);
	SEXP pX;
	R_xlen_t n = XLENGTH(table);
	size_t num_matches = 0;
	for ( size_t i = 0; i < len; i++ ) {
		pX = STRING_ELT(x, i);
		if ( pX == NA_STRING )
			ptr[i] = nomatch;
		else
			ptr[i] = binary_search<const char *>(CHAR(pX), table, 0, n,
				tol, tol_ref, nomatch, nearest);
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
		tol, tol_ref, nomatch, nearest, index1, 1);
	UNPROTECT(1);
	return pos;
}

// fuzzy key-value search returning all 'value's with 'keys' matching 'x'
template<typename TKey, typename TVal>
Pair<index_t,TVal> keyval_search(TKey x, SEXP keys, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, TVal nomatch, int dups = NO_DUPS, bool sorted = FALSE)
{
	TKey * pKeys = DataPtr<TKey>(keys);
	TVal * pValues = DataPtr<TVal>(values);
	index_t pos = NA_INTEGER;
	TVal retVal = nomatch;
	if ( !isNA(x) )
	{
		if ( sorted ) { // sorted keys -- binary search
			pos = binary_search<TKey>(x, keys, start, end,
				tol, tol_ref, NA_INTEGER, FALSE);
			if ( pos != NA_INTEGER ) {
				retVal = pValues[pos];
				if ( dups != NO_DUPS )
				{
					for ( int j = 1; pos - j >= 0; j++ )
					{
						if ( approx_eq<TKey>(x, pKeys[pos - j], tol, tol_ref) )
						{
							TVal dupVal = pValues[pos - j];
							switch(dups) {
								case SUM_DUPS:
									retVal += dupVal;
									break;
								case MAX_DUPS:
									retVal = dupVal > retVal ? dupVal : retVal;
									break;
								case MIN_DUPS:
									retVal = dupVal < retVal ? dupVal : retVal;
									break;
							}
						}
						else
							break;
					}
					for ( int k = 1; pos + k < end; k++ )
					{
						if ( approx_eq<TKey>(x, pKeys[pos + k], tol, tol_ref) )
						{
							TVal dupVal = pValues[pos + k];
							switch(dups) {
								case SUM_DUPS:
									retVal += dupVal;
									break;
								case MAX_DUPS:
									retVal = dupVal > retVal ? dupVal : retVal;
									break;
								case MIN_DUPS:
									retVal = dupVal < retVal ? dupVal : retVal;
									break;
							}
						}
						else
							break;
					}
				}
			}
		}
		else { // unsorted keys -- linear search
			size_t count = 0;
			double min_diff = DBL_MAX;
			for ( size_t i = start; i < end; i++ )
			{
				double diff = rel_diff<TKey>(x, pKeys[i], tol_ref);
				if ( diff <= tol )
				{
					count++;
					switch(dups) {
						case NO_DUPS:
							retVal = diff < min_diff ? pValues[i] : retVal;
							break;
						case SUM_DUPS:
							retVal = count > 1 ? pValues[i] + retVal : pValues[i];
							break;
						case MAX_DUPS:
							retVal = count == 1 || pValues[i] > retVal ? pValues[i] : retVal;
							break;
						case MIN_DUPS:
							retVal = count == 1 || pValues[i] < retVal ? pValues[i] : retVal;
							break;
					}
					if ( diff < min_diff ) {
						min_diff = diff;
						pos = i;
					}
					if ( dups == NO_DUPS && min_diff == 0 )
						break;
				}
			}
		}
	}
	Pair<index_t,TVal> result = {pos, retVal};
	return result;
}

template<typename TKey, typename TVal>
size_t do_keyval_search(TVal * ptr, TKey * x, size_t xlen, SEXP keys, SEXP values,
	size_t start, size_t end, double tol, int tol_ref, TVal nomatch,
	int dups = NO_DUPS, bool sorted = FALSE, index_t skip = 1)
{
	size_t num_matches = 0;
	for ( index_t ix = 0, curkey = start; ix < xlen; ix += skip )
	{
		ptr[ix] = nomatch;
		if ( !isNA(x[ix]) )
		{
			Pair<index_t,TVal> result = keyval_search(x[ix], keys, values,
				curkey, end, tol, tol_ref, nomatch, dups, sorted);
			if ( result.first != NA_INTEGER ) {
				curkey = result.first;
				ptr[ix] = result.second;
			}
			if ( !sorted || (ix + 1 < xlen && x[ix + 1] < x[ix]) )
				curkey = start; // reset if either 'x' or 'keys' is unsorted
		}
	}
	return num_matches;
}

template<typename TKey, typename TVal, int S>
SEXP do_keyval_search(SEXP x, SEXP keys, SEXP values, double tol, int tol_ref,
	TVal nomatch, int dups = NO_DUPS, bool sorted = FALSE)
{
	R_xlen_t xlen = XLENGTH(x);
	R_xlen_t keylen = XLENGTH(keys);
	SEXP result;
	PROTECT(result = Rf_allocVector(S, xlen));
	TVal * pResult = DataPtr<TVal>(result);
	TKey * pX = DataPtr<TKey>(x);
	do_keyval_search<TKey, TVal>(pResult, pX, xlen, keys, values,
		0, keylen, tol, tol_ref, nomatch, dups, sorted, 1);
	UNPROTECT(1);
	return result;
}

#endif // MATTER_SEARCH
