
#ifndef SELECT
#define SELECT

#include "matterDefines.h"

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

// find the k-th element of an array x
template<typename T>
T quick_select(T * x, size_t start, size_t end, size_t k)
{
	T y[end - start];
	std::memcpy(y, x, (end - start) * sizeof(T));
	index_t left = start, right = end - 1, i, j, pivot;
	do {
		if ( left == right )
			return y[left];
		// find pivot by median of 1st/mid/last
		pivot = (left + right) / 2;
		if ( y[pivot] < y[left] )
			swap(y[pivot], y[left], T);
		if ( y[pivot] > y[right] )
		{
			swap(y[pivot], y[right], T);
			if ( y[pivot] < y[left] )
				swap(y[pivot], y[left], T);
		}
		// use Hoare's partition method 
		i = left + 1;
		j = right - 1;
		do {
			while ( y[i] < y[pivot] ) i++;
			while ( y[j] > y[pivot] ) j--;
			if ( i < j )
			{
				swap(y[i], y[j], T);
				if ( pivot == i )
					pivot = j;
				else if ( pivot == j )
					pivot = i;
			}
			else if ( i == j )
			{
				i++;
				j--;
				break;
			}
		} while (i <= j);
		// return k-th element or loop again
		if ( k == pivot )
			return y[k];
		else if ( k < pivot )
			right = pivot - 1;
		else
			left = pivot + 1;
	}
	while (true);
}

#endif // SELECT
