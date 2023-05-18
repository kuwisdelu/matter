#ifndef SEARCH
#define SEARCH

#include "matterDefines.h"
#include "signal.h"

#define SEARCH_ERROR -1

//// Sortedness
//--------------

// return whether a sequence is sorted
template<typename T>
bool is_sorted(T * x, size_t size, bool strictly = false)
{
	for ( size_t i = 1; i < size; i++ ) {
		double delta = sdiff(x[i], x[i - 1]);
		if ( delta < 0 || (strictly && delta <= 0) )
			return false;
	}
	return true;
}

//// Binary search
//-----------------

// fuzzy binary search returning position of x in table
template<typename T>
index_t binary_search(T x, T * table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = false,
	bool ind1 = false, int err = SEARCH_ERROR)
{
	double delta;
	index_t min = start, max = end, mid = nomatch;
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		double d1 = sdiff(table[start], table[mid]);
		double d2 = sdiff(table[mid], table[end - 1]);
		if ( d1 > 0 || d2 > 0 )
			return err; // table is not sorted
		delta = sdiff(x, table[mid], tol_ref);
		if ( delta < 0 )
			end = mid;
		else if ( delta > 0 )
			start = mid + 1;
		else
			return mid + ind1;
	}
	if ( (nearest || tol > 0) && (max - min) > 0 )
	{
		index_t left = mid >= min + 1 ? mid - 1 : min;
		index_t right = mid < max - 1 ? mid + 1 : max - 1;
		double dleft = udiff(x, table[left], tol_ref);
		double dmid = udiff(x, table[mid], tol_ref);
		double dright = udiff(x, table[right], tol_ref);
		if ( (mid == left && delta < 0) && (nearest || dleft <= tol) )
			return left + ind1;
		else if ( (mid == right && delta > 0) && (nearest || dright <= tol) )
			return right + ind1;
		else {
			if ( (dleft <= dmid && dleft <= dright) && (nearest || dleft <= tol) )
				return left + ind1;
			else if ( (dmid <= dleft && dmid <= dright) && (nearest || dmid <= tol) )
				return mid + ind1;
			else if ( nearest || dright <= tol )
				return right + ind1;
		}
	}
	return nomatch;
}

// fuzzy binary search returning position of x in table
template<> inline
index_t binary_search(double x, double * table,
	size_t start, size_t end, double tol, int tol_ref,
	int nomatch, bool nearest, bool ind1, int err)
{
	index_t i = start, j = end, mid;
	while ( i < j - 1 )
	{
		mid = (i + j) / 2;
		if ( x < table[mid] )
			j = mid;
		else
			i = mid;
	}
	if ( x == table[i] )
		return i + ind1;
	if ( x == table[j] )
		return j + ind1;
	double di = udiff(x, table[i], tol_ref);
	double dj = udiff(x, table[j], tol_ref);
	if ( di < dj && (nearest || di < tol ) )
		return i + ind1;
	if ( dj < di && (nearest || dj < tol ) )
		return j + ind1;
	return nomatch;
}

// apply binary search over an array x, return via ptr
template<typename T>
index_t do_binary_search(int * ptr, T * x, size_t xlen, T * table,
	size_t start, size_t end, double tol, int tol_ref, int nomatch,
	bool nearest = false, bool ind1 = false, int err = SEARCH_ERROR)
{
	size_t num_matches = 0;
	for ( size_t i = 0; i < xlen; i++ ) {
		if ( isNA(x[i]) )
			ptr[i] = nomatch;
		else {
			index_t pos = binary_search(x[i], table, start, end,
				tol, tol_ref, nomatch, nearest, ind1, err);
			if ( pos != err ) {
				ptr[i] = pos;
				num_matches++;
			}
			else
				return err;
		}
	}
	return num_matches;
}

//// Approximate search
//----------------------

// approximate search for values indexed by keys w/ interpolation
template<typename Tkey, typename Tval>
Pair<index_t,Tval> approx_search(Tkey x, Tkey * keys, Tval * values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch,
	int interp = EST_NEAR)
{
	index_t pos = NA_INTEGER;
	Tval val = nomatch;
	Pair<index_t,Tval> result = {pos, val};
	if ( isNA(x) )
		return result;
	pos = binary_search(x, keys, start, end,
		tol, tol_ref, NA_INTEGER);
	result.first = pos;
	if ( !isNA(pos) && pos >= 0 )
	{
		if ( tol > 0 )
			val = resample1(x, keys, values,
				pos, end, tol, tol_ref, interp);
		else
			val = values[pos];
		result.second = val;
	}
	return result;
}

// apply approximate search over an array of x, return via ptr
template<typename Tkey, typename Tval>
index_t do_approx_search(Tval * ptr, Tkey * x, size_t xlen, Tkey * keys, Tval * values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch,
	int interp = EST_NEAR, int stride = 1)
{
	index_t num_matches = 0;
	Pair<index_t,Tval> result;
	if ( xlen < 2 * (end - start) || !is_sorted(x, xlen) )
	{
		// if len(x) << 2*len(keys) then iterate x (downsampling)
		for ( size_t i = 0; i < xlen; i++ )
		{
			result = approx_search(x[i], keys, values,
				start, end, tol, tol_ref, nomatch, interp);
			num_matches += !isNA(result.first);
			ptr[i * stride] = result.second;
		}
	}
	else
	{
		// if len(x) >> 2*len(keys) then iterate keys (upsampling)
		int pos [end];
		bool not_matched [xlen];
		do_binary_search(pos, keys, end, x, 0, xlen,
			tol, switch_diff_ref(tol_ref), NA_INTEGER);
		for ( size_t i = 0; i < xlen; i++ )
		{
			if ( isNA(x[i]) )
				ptr[i * stride] = NA<Tval>();
			else
				ptr[i * stride] = nomatch;
			not_matched[i] = true;
		}
		for ( size_t k = 0; k < end; k++ )
		{
			if ( isNA(pos[k]) )
				continue;
			// iterate to left along x
			for ( index_t i = pos[k]; i < xlen; i++ )
			{
				if ( !not_matched[i] )
					break;
				if ( udiff(x[i], keys[k], tol_ref) > tol )
					break;
				result = approx_search(x[i], keys, values,
					k, end, tol, tol_ref, nomatch, interp);
				num_matches += !isNA(result.first);
				ptr[i * stride] = result.second;
				not_matched[i] = false;
			}
			// iterate to right along x
			for ( index_t i = pos[k] - 1; i >= 0; i-- )
			{
				if ( !not_matched[i] )
					break;
				if ( udiff(x[i], keys[k], tol_ref) > tol )
					break;
				result = approx_search(x[i], keys, values,
					k, end, tol, tol_ref, nomatch, interp);
				num_matches += !isNA(result.first);
				ptr[i * stride] = result.second;
				not_matched[i] = false;
			}
		}
		// fill in non-matches
		if ( !isNA(nomatch) )
		{
			for ( size_t i = 0; i < xlen; i++ )
				if ( not_matched[i] )
					ptr[i * stride] = nomatch;
		}
	}
	return num_matches;
}

#endif // SEARCH
