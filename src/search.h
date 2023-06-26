#ifndef SEARCH
#define SEARCH

#include "matterDefines.h"

#define swap(x, y, T) do { T swap = x; x = y; y = swap; } while (false)

//// Comparison
//--------------

template<typename T>
bool lt(T x, T y)
{
	return sdiff<T>(x, y) < 0;
}

template<typename T>
bool gt(T x, T y)
{
	return sdiff<T>(x, y) > 0;
}

template<typename T>
bool lteq(T x, T y)
{
	return sdiff<T>(x, y) <= 0;
}

template<typename T>
bool gteq(T x, T y)
{
	return sdiff<T>(x, y) >= 0;
}

//// Sortedness
//--------------

template<typename T>
bool is_sorted(T * x, size_t n, bool strictly = false)
{
	for ( size_t i = 1; i < n; i++ ) {
		if ( lt(x[i], x[i - 1]) )
			return false;
		else if ( strictly && lteq(x[i], x[i - 1]) )
			return false;
	}
	return true;
}

//// Quick select
//-----------------

// select pivot and partition x (modifed in-place!!!)
template<typename T>
index_t partition(T * x, index_t left, index_t right, int * v = NULL)
{
	// find pivot by median of 1st/mid/last
	index_t i, j, pivot = (left + right) / 2;
	if ( lt(x[pivot], x[left]) ) {
		swap(x[pivot], x[left], T);
		if ( v != NULL )
			swap(v[pivot], v[left], int);
	}
	if ( gt(x[pivot], x[right]) )
	{
		swap(x[pivot], x[right], T);
		if ( v != NULL )
			swap(v[pivot], v[right], int);
		if ( lt(x[pivot], x[left]) ) {
			swap(x[pivot], x[left], T);
			if ( v != NULL )
				swap(v[pivot], v[left], int);
		}
	}
	// use Hoare's partition method 
	i = left + 1;
	j = right - 1;
	do {
		while ( lt(x[i], x[pivot]) ) i++;
		while ( gt(x[j], x[pivot]) ) j--;
		// swap inversions
		if ( i < j && !equal(x[i], x[j]) )
		{
			swap(x[i], x[j], T);
			if ( v != NULL )
				swap(v[i], v[j], int);
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
	return pivot;
}

// find the sorted order of array x (modifed in-place!!!)
template<typename T>
void quick_order(int * ptr, T * x, size_t start, size_t end, bool ind1 = false)
{
	index_t pivot, left = start, right = end - 1;
	// initialize indices
	for ( size_t i = start; i < end; i++ )
		ptr[i] = i + ind1;
	if ( left == right )
		return;
	T dup [end];
	std::memcpy(dup, x, end * sizeof(T));
	// initialize stack
	int stack_size = 2 * std::ceil(std::log2(end - start));
	int stack [stack_size];
	int top = -1;
	stack[++top] = left;
	stack[++top] = right;
	// keep going while the stack is non-empty
	while ( top >= 0 )
	{
		// pop and partition current subarray
		right = stack[top--];
		left = stack[top--];
		if ( right - left < 8 )
		{
			// use insertion sort for small subarrays
			for ( size_t i = left + 1; i <= right; i++ )
			{
				size_t j = i;
				while ( j > left && lt(dup[j], dup[j - 1]) )
				{
					swap(dup[j], dup[j - 1], T);
					swap(ptr[j], ptr[j - 1], int);
					j--;
				}
			}
			continue;
		}
		pivot = partition(dup, left, right, ptr);
		// push larger subarray then smaller subarray
		if ( pivot - left < right - pivot )
		{
			// push right subarray if non-empty
			if ( pivot + 1 < right )
			{
				stack[++top] = pivot + 1;
				stack[++top] = right;
			}
			// push left subarray if non-empty
			if ( pivot - 1 > left )
			{
				stack[++top] = left;
				stack[++top] = pivot - 1;
			}
		}
		else {
			// push left subarray if non-empty
			if ( pivot - 1 > left )
			{
				stack[++top] = left;
				stack[++top] = pivot - 1;
			}
			// push right subarray if non-empty
			if ( pivot + 1 < right )
			{
				stack[++top] = pivot + 1;
				stack[++top] = right;
			}
		}
	}
}

// find the k-th element of array x (modifed in-place!!!)
template<typename T>
T quick_select(T * x, size_t start, size_t end, size_t k)
{
	index_t pivot, left = start, right = end - 1;
	do {
		if ( left == right )
			return x[left];
		pivot = partition(x, left, right);
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
	T dup [end];
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
	T dup [n];
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
	double dev [n];
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
	bool ind1 = false)
{
	if ( start >= end )
		return nomatch;
	index_t i = start, j = end, mid;
	while ( i < j - 1 )
	{
		mid = (i + j) / 2;
		if ( lt(x, table[mid]) )
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
	bool nearest = false, bool ind1 = false)
{
	size_t num_matches = 0;
	for ( size_t i = 0; i < xlen; i++ )
	{
		if ( isNA(x[i]) )
			ptr[i] = nomatch;
		else
		{
			ptr[i] = binary_search(x[i], table, start, end,
				tol, tol_ref, nomatch, nearest, ind1);
			if ( ptr[i] != nomatch )
				num_matches++;
		}
	}
	return num_matches;
}

#endif // SEARCH
