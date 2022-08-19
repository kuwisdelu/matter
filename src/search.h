#ifndef MATTER_SEARCH
#define MATTER_SEARCH

#define R_NO_REMAP

#include <R.h>

#include <cmath>
#include <cfloat>

#include "utils.h"
#include "signal.h"

//// Linear search
//-----------------

// fuzzy linear search (fallback for unsorted arrays)
template<typename T>
index_t linear_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = FALSE)
{
	index_t pos = nomatch;
	double diff, diff_min = DBL_MAX;
	T * pTable = DataPtr<T>(table);
	for ( size_t i = start; i < end; i++ )
	{
		diff = rel_diff(x, pTable[i], tol_ref);
		if ( diff == 0 )
			return i;
		if ( diff < diff_min ) {
			diff_min = diff;
			pos = i;
		}
	}
	if ( diff_min < tol || nearest )
		return pos;
	else
		return nomatch;
}

//// Binary search
//-----------------

// fuzzy binary search returning position of 'x' in 'table'
template<typename T>
index_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = FALSE, int err = -1)
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
	if ( (nearest || tol > 0) && (max - min) > 0 )
	{
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
	bool ind1 = FALSE, int err = -1)
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
			ptr[i] += ind1; // adjust for R indexing from 1
		}
	}
	return num_matches;
}

template<typename T>
SEXP do_binary_search(SEXP x, SEXP table, double tol, int tol_ref,
	int nomatch, bool nearest = FALSE, bool ind1 = FALSE)
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
		tol, tol_ref, nomatch, nearest, ind1);
	UNPROTECT(1);
	return pos;
}

//// Approximate search
//----------------------

// search for 'values' indexed by 'keys' with interpolation
template<typename Tkey, typename Tval>
Pair<index_t,Tval> approx_search(Tkey x, SEXP keys, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, Tval nomatch, int interp = EST_NEAR, bool sorted = TRUE)
{
	Tkey * pKeys = DataPtr<Tkey>(keys);
	Tval * pValues = DataPtr<Tval>(values);
	index_t pos = NA_INTEGER;
	Tval val = nomatch;
	size_t num_matches = 0;
	Pair<index_t,Tval> result = {pos, val};
	if ( isNA(x) )
		return result;
	if ( sorted ) {
		pos = binary_search<Tkey>(x, keys,
			start, end, tol, tol_ref, NA_INTEGER);
		// return early if no interpolation, no match, or unsorted
		if ( interp == EST_NEAR || isNA(pos) || pos < 0 )
		{
			if ( !isNA(pos) && pos >= 0 )
				val = pValues[pos];
			result = {pos, val};
			return result;
		}
		start = pos;
	}
	// perform linear search, or expand search for interpolation
	int init = start, step = 1;
	double diff_min = DBL_MAX; // track nearest key to 'x'
	index_t pos_max = NA_INTEGER; // track extrema
	index_t pj[] = {NA_INTEGER, NA_INTEGER, NA_INTEGER, NA_INTEGER}; // nearest key idxs
	double pdiff[] = {DBL_MAX, DBL_MAX, DBL_MAX, DBL_MAX}; // dists to nearest key
	double wt = 1, wscale = 0; // weights for kernels
	while ( TRUE ) // search right, then left
	{
		for ( index_t i = init; i < end && i >= 0; i += step )
		{
			double delta = rel_change<Tkey>(x, pKeys[i], tol_ref);
			double diff = fabs(delta);
			if ( diff <= tol )
			{
				if ( diff < diff_min )
				{
					diff_min = diff;
					pos = i;
				}
				switch(interp) {
					case EST_NEAR:
						if ( diff <= diff_min ) {
							val = pValues[i];
							if ( diff <= 0 ) {
								result = {pos, val};
								return result;
							}
						}
						break;
					case EST_SUM:
					case EST_AVG:
					case EST_GAUS:
					case EST_SINC:
						if ( interp == EST_GAUS )
							wt = kgaussian(diff, (2 * tol + 1) / 4);
						if ( interp == EST_SINC )
							Rf_error("interp = 'lanczos' not implemented yet");
						if ( num_matches == 0 )
							val = wt * pValues[i];
						else
							val += wt * pValues[i];
						wscale += wt;
						break;
					case EST_AREA:
						Rf_error("interp = 'area' not implemented yet");
					case EST_MAX:
						if ( num_matches == 0 || pValues[i] > val ) {
							val = pValues[i];
							pos_max = i;
						}
						break;
					case EST_MIN:
						if ( num_matches == 0 || pValues[i] < val ) {
							val = pValues[i];
							pos_max = i;
						}
						break;
					case EST_LERP:
					case EST_CUBIC:
					{
						if ( delta > 0 )
						{
							if ( diff < pdiff[1] ) {
								pdiff[1] = diff;
								pj[1] = i;
							}
							if ( (diff > pdiff[1] && diff < pdiff[0]) ||
								(pj[1] == pj[0] || isNA(pj[0])) )
							{
								pdiff[0] = diff;
								pj[0] = i;
							}
						}
						if ( delta < 0 )
						{
							if ( diff < pdiff[2] ) {
								pdiff[2] = diff;
								pj[2] = i;
							}
							if ( (diff > pdiff[2] && diff < pdiff[3]) ||
								(pj[2] == pj[3] || isNA(pj[3])) )
							{
								pdiff[3] = diff;
								pj[3] = i;
							}
						}
						break;
					}
				}
				num_matches++;
			}
			else if ( sorted )
				break;
		}
		if ( init == start && sorted )
		{
			init = start - 1;
			step = -1;
		}
		else
			break;
	}
	if ( !isNA(pos) )
	{
		// setup params for interpolation methods
		double tx, dxs[3], ys[4];
		switch(interp) {
			case EST_LERP:
			case EST_CUBIC:
				pj[1] = !isNA(pj[1]) ? pj[1] : pos;
				pj[0] = !isNA(pj[0]) ? pj[0] : pj[1];
				pj[2] = !isNA(pj[2]) ? pj[2] : pos;
				pj[3] = !isNA(pj[3]) ? pj[3] : pj[2];
				tx = rel_change(x, pKeys[pj[1]]);
				dxs[0] = rel_change(pKeys[pj[1]], pKeys[pj[0]]);
				dxs[1] = rel_change(pKeys[pj[2]], pKeys[pj[1]]);
				dxs[2] = rel_change(pKeys[pj[3]], pKeys[pj[2]]);
				ys[0] = static_cast<double>(pValues[pj[0]]);
				ys[1] = static_cast<double>(pValues[pj[1]]);
				ys[2] = static_cast<double>(pValues[pj[2]]);
				ys[3] = static_cast<double>(pValues[pj[3]]);
		}
		// calculate interpolated values
		switch(interp) {
			case EST_AVG:
			case EST_GAUS:
			case EST_SINC:
				val = val / wscale;
				break;
			case EST_LERP:
			{
				if ( dxs[1] > 0 && diff_min > 0 )
					val = lerp(ys[1], ys[2], tx / dxs[1]);
				else
					val = pValues[pos];
				break;
			}
			case EST_CUBIC:
			{
				if ( dxs[1] > 0 && diff_min > 0 )
					val = cubic(ys, dxs, tx / dxs[1]);
				else
					val = pValues[pos];
				break;
			}
		}
	}
	result = {pos, val};
	return result;
}

template<typename Tkey, typename Tval>
size_t do_approx_search(Tval * ptr, Tkey * x, size_t xlen, SEXP keys, SEXP values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch,
	int interp = EST_NEAR, bool sorted = TRUE, size_t stride = 1)
{
	size_t num_matches = 0;
	index_t i = 0, current = start;
	while ( i < xlen )
	{
		ptr[i * stride] = nomatch;
		if ( !isNA(x[i]) )
		{
			Pair<index_t,Tval> result = approx_search(x[i], keys, values,
				current, end, tol, tol_ref, nomatch, interp, sorted);
			if ( !isNA(result.first) ) {
				if ( result.first < 0 ) { // keys are not sorted
					sorted = FALSE; // fall back to linear search
					current = start; // reset search space
					continue;
				}
				current = result.first;
				ptr[i * stride] = result.second;
				num_matches++;
			}
			if ( !sorted || (i + 1 < xlen && x[i + 1] < x[i]) )
				current = start; // reset if either 'x' or 'keys' is unsorted
		}
		i++;
	}
	return num_matches;
}

template<typename Tkey, typename Tval>
SEXP do_approx_search(SEXP x, SEXP keys, SEXP values, double tol, int tol_ref,
	Tval nomatch, int interp = EST_NEAR, bool sorted = TRUE)
{
	R_xlen_t xlen = XLENGTH(x);
	R_xlen_t keylen = XLENGTH(keys);
	SEXP result;
	PROTECT(result = Rf_allocVector(TYPEOF(values), xlen));
	Tval * pResult = DataPtr<Tval>(result);
	Tkey * pX = DataPtr<Tkey>(x);
	do_approx_search<Tkey, Tval>(pResult, pX, xlen, keys, values,
		0, keylen, tol, tol_ref, nomatch, interp, sorted);
	UNPROTECT(1);
	return result;
}

#endif // MATTER_SEARCH
