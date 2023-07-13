#ifndef SEARCH
#define SEARCH

#include "matterDefines.h"

// distance metrics
#define DIST_EUC	1 // Euclidean (L2) distance
#define DIST_MAX	2 // Maximum distance
#define DIST_ABS	3 // Manhattan (L1) distance 
#define DIST_MKW	4 // Minkowski distance

// size to fallback to linear search
#define LINEAR_THRESHOLD 8

// exact equality tolerance
// (avoid duplicates or division by 0)
#define EXACT 0

// swap items (use with caution)
#define swap(x, y, T) do { T swap = x; x = y; y = swap; } while (false)

//// Comparison
//--------------

template<typename T>
bool naeq(T x, T y, double tol = DBL_EPSILON)
{
	if ( isNA(x) && isNA(y) )
		return true; 	// NAs equal for sorting
	return udiff<T>(x, y) < DBL_EPSILON;
}

template<typename T>
bool lt(T x, T y)
{
	if ( isNA(x) && isNA(y) )
		return false; 	// NAs equal for sorting
	if ( isNA(x) || isNA(y) )
		return isNA(y);	// sort NAs last
	return sdiff<T>(x, y) < 0;
}

template<typename T>
bool gt(T x, T y)
{
	if ( isNA(x) && isNA(y) )
		return false; 	// NAs equal for sorting
	if ( isNA(x) || isNA(y) )
		return isNA(x);	// sort NAs last
	return sdiff<T>(x, y) > 0;
}

template<typename T>
bool lteq(T x, T y)
{
	if ( isNA(x) && isNA(y) )
		return true; 	// NAs equal for sorting
	if ( isNA(x) || isNA(y) )
		return isNA(y);	// sort NAs last
	return sdiff<T>(x, y) <= 0;
}

template<typename T>
bool gteq(T x, T y)
{
	if ( isNA(x) && isNA(y) )
		return true; 	// NAs equal for sorting
	if ( isNA(x) || isNA(y) )
		return isNA(x);	// sort NAs last
	return sdiff<T>(x, y) >= 0;
}

template<typename T>
index_t argmin(T * x, size_t n)
{
	if ( n == 0 )
		return NA_INTEGER;
	index_t arg = 0;
	for ( index_t i = 1; i < n; i++ )
	{
		if ( lt(x[i], x[arg]) )
			arg = i;
	}
	return arg;
}

template<typename T>
index_t argmax(T * x, size_t n)
{
	if ( n == 0 )
		return NA_INTEGER;
	index_t arg = 0;
	for ( index_t i = 1; i < n; i++ )
	{
		if ( gt(x[i], x[arg]) )
			arg = i;
	}
	return arg;
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
template<typename Tx, typename Tv>
index_t partition(Tx * x, index_t left, index_t right, Tv * v = NULL)
{
	// find pivot by median of 1st/mid/last
	index_t i, j, pivot = (left + right) / 2;
	if ( lt(x[pivot], x[left]) ) {
		swap(x[pivot], x[left], Tx);
		if ( v != NULL )
			swap(v[pivot], v[left], Tv);
	}
	if ( gt(x[pivot], x[right]) )
	{
		swap(x[pivot], x[right], Tx);
		if ( v != NULL )
			swap(v[pivot], v[right], Tv);
		if ( lt(x[pivot], x[left]) ) {
			swap(x[pivot], x[left], Tx);
			if ( v != NULL )
				swap(v[pivot], v[left], Tv);
		}
	}
	// use Hoare's partition method 
	i = left + 1;
	j = right - 1;
	do {
		while ( lt(x[i], x[pivot]) ) i++;
		while ( gt(x[j], x[pivot]) ) j--;
		// swap inversions
		if ( i < j && !naeq(x[i], x[j]) )
		{
			swap(x[i], x[j], Tx);
			if ( v != NULL )
				swap(v[i], v[j], Tv);
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

// sort an array x (modifed in-place!!!)
template<typename Tx, typename Tv>
void quick_sort(Tx * x, size_t start, size_t end, Tv * v = NULL)
{
	index_t pivot, left = start, right = end - 1;
	if ( left == right )
		return;
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
		if ( right - left < LINEAR_THRESHOLD )
		{
			// use insertion sort for small subarrays
			for ( size_t i = left + 1; i <= right; i++ )
			{
				size_t j = i;
				while ( j > left && lt(x[j], x[j - 1]) )
				{
					swap(x[j], x[j - 1], Tx);
					if ( v != NULL )
						swap(v[j], v[j - 1], Tv);
					j--;
				}
			}
			continue;
		}
		pivot = partition(x, left, right, v);
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

// sort an array x and return sorted indices in ptr
template<typename T>
void do_quick_sort(int * ptr, T * x, size_t start, size_t end, bool ind1 = false)
{
	index_t n = end - start;
	if ( n == 0 )
		return;
	// initialize indices
	for ( index_t i = 0; i < n; i++ )
		ptr[i] = i + ind1;
	T * dup = R_Calloc(n, T);
	std::memcpy(dup, x + start, n * sizeof(T));
	quick_sort(dup, 0, n, ptr);
	Free(dup);
}

// sort an array x and return ranks in ptr
template<typename T>
index_t do_quick_rank(int * ptr, T * x, size_t start, size_t end, bool ties_max = false)
{
	index_t n = end - start;
	if ( n == 0 )
		return 0;
	// initialize working buffers
	int * indx = R_Calloc(n, int);
	for ( index_t i = 0; i < n; i++ )
		indx[i] = i;
	T * dup = R_Calloc(n, T);
	std::memcpy(dup, x + start, n * sizeof(T));
	// sort the array
	quick_sort(dup, 0, n, indx);
	index_t count, j, i = 0, rank = 0;
	// rank the values
	while ( i < n )
	{
		if ( isNA(dup[i]) )
		{
			ptr[indx[i]] = NA_INTEGER;
			i++;
			continue;
		}
		else
		{
			j = i + 1;
			while ( j < n && equal(dup[i], dup[j]) )
				j++;
			count = j - i;
			while ( i < j )
			{
				if ( ties_max )
					ptr[indx[i]] = rank + count;
				else
					ptr[indx[i]] = rank + 1;
				i++;
			}
			rank += count;
		}
	}
	Free(indx);
	Free(dup);
	return rank;
}

// find the k-th element of array x (modifed in-place!!!)
template<typename T>
T quick_select(T * x, size_t start, size_t end, size_t k)
{
	index_t pivot, left = start, right = end - 1;
	do {
		if ( left == right )
			return x[left];
		pivot = partition<T,void*>(x, left, right);
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

// find the k-th elements of an array x and return in ptr
template<typename T>
void do_quick_select(T * ptr, T * x, size_t start, size_t end, int * k, size_t nk)
{
	index_t n = end - start;
	if ( n == 0 )
		return;
	T * dup = R_Calloc(n, T);
	std::memcpy(dup, x + start, n * sizeof(T));
	ptr[0] = quick_select(dup, 0, n, k[0]);
	for ( index_t i = 1; i < nk; i++ )
	{
		if ( k[i] > k[i - 1] )
			ptr[i] = quick_select(dup, k[i - 1] + 1, n, k[i]);
		else if ( k[i] < k[i - 1] )
			ptr[i] = quick_select(dup, 0, k[i - 1], k[i]);
		else 
			ptr[i] = ptr[i - 1];
	}
	Free(dup);
}

//// Median
//-----------

template<typename T>
double quick_median(T * x, size_t n)
{
	if ( n == 0 )
		return NA_REAL;
	T * dup = R_Calloc(n, T);
	std::memcpy(dup, x, n * sizeof(T));
	size_t len = 0;
	for ( index_t i = 0; i < n; i++ )
	{
		if ( !isNA(x[i]) )
			len++;
	}
	size_t k = len / 2;
	double result = NA_REAL;
	if ( len % 2 == 0 )
	{
		double m1 = quick_select(dup, 0, n, k - 1);
		double m2 = quick_select(dup, k, n, k);
		result = 0.5 * (m1 + m2);
	}
	else
		result = quick_select(dup, 0, n, k);
	Free(dup);
	return result;
}

template<typename T>
double quick_mad(T * x, size_t n, double center = NA_REAL, double scale = 1.4826)
{
	if ( n == 0 )
		return NA_REAL;
	double * dev = R_Calloc(n, double);
	if ( isNA(center) )
		center = quick_median(x, n);
	for ( index_t i = 0; i < n; i++ )
	{
		if ( isNA(x[i]) )
			dev[i] = NA_REAL;
		else
			dev[i] = std::fabs(x[i] - center);
	}
	double mad = scale * quick_median(dev, n);
	Free(dev);
	return mad;
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

//// K-D search
//-----------------

// build a kd-tree from an n x k array
template<typename T>
index_t kd_tree_build(T * x, size_t k, size_t n,
	int * left_child, int * right_child)
{
	if ( n == 0 || k == 0 )
		return NA_INTEGER;
	index_t parent, depth, start = 0, end = n;
	// initialize stack
	int stack_size = 8 * std::ceil(std::log2(n) + 1);
	int stack [stack_size];
	int top = -1;
	// initialize indices and working buffer
	T * xs = R_Calloc(n, T);
	int * indx = R_Calloc(n, int);
	for ( index_t i = 0; i < n; i++ )
	{
		left_child[i] = NA_INTEGER;
		right_child[i] = NA_INTEGER;
		xs[i] = x[i];
		indx[i] = i;
	}
	// find root
	quick_sort(xs, start, end, indx);
	index_t mid = (start + end) / 2;
	// account for duplicates
	while ( mid > start && equal(xs[mid - 1], xs[mid], EXACT) )
		mid--;
	// insert root
	index_t root = indx[mid];
	// add left child to stack
	if ( mid > 0 )
	{
		stack[++top] = root;	// parent
		stack[++top] = 1;		// depth
		stack[++top] = 0;		// start
		stack[++top] = mid;		// end
		// assign next dimension
		for ( index_t i = 0; i < mid; i++ )
			xs[i] = x[n * (1 % k) + indx[i]];
	}
	// add right child to stack
	if ( mid + 1 < n )
	{
		stack[++top] = root;	// parent
		stack[++top] = 1;		// depth
		stack[++top] = mid + 1;	// start
		stack[++top] = n;		// end
		// assign next dimension
		for ( index_t i = mid + 1; i < n; i++ )
			xs[i] = x[n * (1 % k) + indx[i]];
	}
	// keep going while the stack is non-empty
	while ( top >= 0 )
	{
		// pop node
		end = stack[top--];
		start = stack[top--];
		depth = stack[top--];
		parent = stack[top--];
		// find partition
		quick_sort(xs, start, end, indx);
		mid = (start + end) / 2;
		// account for duplicates
		while ( mid > start && equal(xs[mid - 1], xs[mid], EXACT) )
			mid--;
		// insert node under parent
		index_t jprev = (depth - 1) % k;
		index_t jnext = (depth + 1) % k;
		if ( lt(x[n * jprev + indx[mid]], x[n * jprev + parent]) )
			left_child[parent] = indx[mid];
		else
			right_child[parent] = indx[mid];
		// push left child to stack
		if ( mid > start )
		{
			stack[++top] = indx[mid];
			stack[++top] = depth + 1;
			stack[++top] = start;
			stack[++top] = mid;
			// assign next dimension
			for ( index_t i = start; i < mid; i++ )
				xs[i] = x[n * jnext + indx[i]];
		}
		// push right child to stack
		if ( mid + 1 < end )
		{
			stack[++top] = indx[mid];
			stack[++top] = depth + 1;
			stack[++top] = mid + 1;
			stack[++top] = end;
			// assign next dimension
			for ( index_t i = mid + 1; i < end; i++ )
				xs[i] = x[n * jnext + indx[i]];
		}
	}
	Free(indx);
	Free(xs);
	return root;
}

// search for points within tol of x, return via ptr
template<typename T>
index_t kd_tree_search(int * ptr, T * x, T * data, size_t k, size_t n,
	int * left_child, int * right_child, size_t root,
	double * tol, int tol_ref, bool ind1 = false)
{
	if ( n == 0 || k == 0 )
		return 0;
	index_t node, depth, num_matches = 0;
	// initialize stack
	int stack_size = 2 * std::ceil(std::log2(n) + 1);
	int stack [stack_size];
	int top = -1;
	stack[++top] = root;	// node
	stack[++top] = 0;		// depth
	while ( top >= 0 )
	{
		// pop node
		depth = stack[top--];
		node = stack[top--];
		index_t j = depth % k;
		double ds = sdiff(x[j], data[n * j + node], tol_ref);
		double du = std::fabs(ds);
		// check if we need to search left subtree
		if ( (ds < 0 || du <= tol[j]) && !isNA(left_child[node]) )
		{
			stack[++top] = left_child[node];
			stack[++top] = depth + 1;
		}
		// check if we need to search right subtree
		if ( (ds > 0 || du <= tol[j]) && !isNA(right_child[node]) )
		{
			stack[++top] = right_child[node];
			stack[++top] = depth + 1;
		}
		// check if this point neighbors x
		if ( du <= tol[j] )
		{
			bool is_neighbor = true;
			for ( j = 0; j < k; j++ )
			{
				double dj = udiff(x[j], data[n * j + node], tol_ref);
				is_neighbor = dj <= tol[j];
				if ( !is_neighbor )
					break;
			}
			if ( is_neighbor )
			{
				ptr[num_matches] = node + ind1;
				num_matches++;
			}
		}
	}
	return num_matches;
}

//// K-NN search
//-----------------

// calculate distance between k-dim points
template<typename T>
double do_dist(T * x, T * y, size_t k, int stepx = 1, int stepy = 1,
	int metric = DIST_EUC, double p = 2, double * scale = NULL)
{
	double si, di, D = 0;
	for ( index_t i = 0; i < k; i++ )
	{
		if ( scale != NULL )
			si = scale[i];
		else
			si = 1;
		di = udiff(x[i * stepx], y[i * stepy]) / si;
		switch(metric) {
			case DIST_EUC:
				D += di * di;
				break;
			case DIST_MAX:
				D = di > D ? di : D;
				break;
			case DIST_ABS:
				D += di;
				break;
			case DIST_MKW:
				D += std::pow(di, p);
				break;
			default:
				Rf_error("unrecognized distance metric");
		}
	}
	switch(metric) {
		case DIST_EUC:
			return std::sqrt(D);
		case DIST_MAX:
			return D;
		case DIST_ABS:
			return D;
		case DIST_MKW:
			return std::pow(D, 1 / p);
		default:
			return NA_REAL;
	}
}

// search for knn points nearest x, return via ptr
template<typename T>
void knn_search(int * ptr, T * x, T * data, size_t k, size_t n,
	int * left_child, int * right_child, size_t root, int knn,
	int metric = DIST_EUC, double p = 2, bool ind1 = false)
{
	if ( n == 0 || k == 0 || knn == 0 )
		return;
	index_t node, depth;
	// initialize knn
	double best [knn];
	for ( index_t i = 0; i < knn; i++ )
	{
		ptr[i] = NA_INTEGER;
		best[i] = R_PosInf;
	}
	// initialize stack
	int stack_size = 2 * std::ceil(std::log2(n) + 1);
	int stack [stack_size];
	int top = -1;
	stack[++top] = root;	// node
	stack[++top] = 0;		// depth
	while ( top >= 0 )
	{
		// pop node
		depth = stack[top--];
		node = stack[top--];
		index_t j = depth % k;
		double ds = sdiff(x[j], data[n * j + node]);
		double du = std::fabs(ds);
		double d2 = do_dist(x, data + node, k, 1, n, metric, p);
		// check if this is a better neighbor
		if ( d2 < best[knn - 1] )
		{
			index_t i = knn - 1;
			ptr[i] = node + ind1;
			best[i] = d2;
			// sort this neighbor into place
			while ( i > 0 && lt(best[i], best[i - 1]) )
			{
				swap(ptr[i], ptr[i - 1], double);
				swap(best[i], best[i - 1], double);
				i--;
			}
		}
		// check if we need to search left subtree
		if ( (ds < 0 || du <= best[knn - 1]) && !isNA(left_child[node]) )
		{
			stack[++top] = left_child[node];
			stack[++top] = depth + 1;
		}
		// check if we need to search right subtree
		if ( (ds > 0 || du <= best[knn - 1]) && !isNA(right_child[node]) )
		{
			stack[++top] = right_child[node];
			stack[++top] = depth + 1;
		}
	}
}

// search for knn points nearest x, return via ptr
template<typename T>
void do_knn_search(int * ptr, T * x, T * data, size_t k, size_t nx, size_t ndata,
	int * left_child, int * right_child, size_t root, int knn,
	int metric = DIST_EUC, double p = 2, bool ind1 = false)
{
	T xi [k];
	int nn [knn];
	for ( index_t i = 0; i < nx; i++ )
	{
		for ( index_t j = 0; j < k; j++ )
			xi[j] = x[j * nx + i];
		knn_search(nn, xi, data, k, ndata,
			left_child, right_child, root, knn, metric, p, ind1);
		for ( index_t j = 0; j < knn; j++ )
			ptr[j * nx + i] = nn[j];
	}
}

#endif // SEARCH
