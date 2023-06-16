#ifndef SEARCH
#define SEARCH

#include "matterDefines.h"

#define SEARCH_ERROR -1

#define swap(x, y, T) do { T swap = x; x = y; y = swap; } while (false)

//// Sortedness
//--------------

template<typename T>
bool is_sorted(T * x, size_t n, bool strictly = false)
{
	for ( size_t i = 1; i < n; i++ ) {
		double d = sdiff(x[i], x[i - 1]);
		if ( d < 0 || (strictly && d <= 0) )
			return false;
	}
	return true;
}

//// Quick select
//-----------------

// find the k-th element of array x (modifed in-place!!!)
template<typename T>
T quick_select(T * x, size_t start, size_t end, size_t k)
{
	index_t left = start, right = end - 1, i, j, pivot;
	do {
		if ( left == right )
			return x[left];
		// find pivot by median of 1st/mid/last
		pivot = (left + right) / 2;
		if ( x[pivot] < x[left] )
			swap(x[pivot], x[left], T);
		if ( x[pivot] > x[right] )
		{
			swap(x[pivot], x[right], T);
			if ( x[pivot] < x[left] )
				swap(x[pivot], x[left], T);
		}
		// use Hoare's partition method 
		i = left + 1;
		j = right - 1;
		do {
			while ( x[i] < x[pivot] ) i++;
			while ( x[j] > x[pivot] ) j--;
			// swap inversions
			if ( i < j && x[i] != x[j] )
			{

				swap(x[i], x[j], T);
				if ( pivot == i )
					pivot = j;
				else if ( pivot == j )
					pivot = i;
			}
			// allow pointers to cross
			else if ( i == j )
			{
				i++;
				j--;
			}
			// account for ties
			else
			{
				if ( i != pivot )
					i++;
				if ( j != pivot )
					j--;
			}
		} while (i <= j);
		// return k-th element or loop again
		if ( k == pivot )
			return x[k];
		else if ( k < pivot )
			right = pivot - 1;
		else
			left = pivot + 1;
	}
	while (true);
}

// find the k-th elements of an array x
template<typename T>
void do_quick_select(T * ptr, T * x, size_t start, size_t end, int * k, size_t n)
{
	T dup[end];
	std::memcpy(dup, x, end * sizeof(T));
	ptr[0] = quick_select(dup, start, end, k[0]);
	for ( index_t i = 1; i < n; i++ )
	{
		if ( k[i] > k[i - 1] )
			ptr[i] = quick_select(dup, k[i - 1] + 1, end, k[i]);
		else if ( k[i] < k[i - 1] )
			ptr[i] = quick_select(dup, start, k[i - 1], k[i]);
		else 
			ptr[i] = k[i - 1];
	}
}

//// Median
//-----------

template<typename T>
double quick_median(T * x, size_t n)
{
	T dup[n];
	std::memcpy(dup, x, n * sizeof(T));
	size_t k = n / 2;
	if ( n % 2 == 0 )
	{
		double m1 = quick_select(dup, 0, n, k - 1);
		double m2 = quick_select(dup, k, n, k);
		return 0.5 * (m1 + m2);
	}
	else
		return quick_select(dup, 0, n, k);
}

template<typename T>
double quick_mad(T * x, size_t n, double center = NA_REAL, double scale = 1.4826)
{
	double dev[n];
	if ( isNA(center) )
		center = quick_median(x, n);
	for ( index_t i = 0; i < n; i++ )
		dev[i] = std::fabs(x[i] - center);
	return scale * quick_median(dev, n);
}

//// Binary search
//-----------------

// fuzzy binary search returning position of x in table
template<typename T>
index_t binary_search(T x, T * table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = false,
	bool ind1 = false, int err = SEARCH_ERROR)
{
	index_t i = start, j = end, mid;
	while ( i < j - 1 )
	{
		mid = (i + j) / 2;
		if ( sdiff(x, table[mid]) < 0 )
			j = mid;
		else
			i = mid;
	}
	if ( j == end )
		j = i;
	if ( equal(x, table[i]) )
		return i + ind1;
	if ( equal(x, table[j]) )
		return j + ind1;
	double di = udiff(x, table[i], tol_ref);
	double dj = udiff(x, table[j], tol_ref);
	if ( di <= dj && (nearest || di <= tol ) )
		return i + ind1;
	if ( dj <= di && (nearest || dj <= tol ) )
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

#endif // SEARCH
