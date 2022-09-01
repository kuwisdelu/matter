#ifndef SEARCH
#define SEARCH

#include "Rutils.h"
#include "signal.h"

#define SEARCH_ERROR -1

//// Sortedness
//--------------

// return whether a sequence is sorted
template<typename T>
bool is_sorted(T * x, size_t size, bool strictly = false)
{
	for ( size_t i = 1; i < size; i++ ) {
		double delta = rel_change(x[i], x[i - 1]);
		if ( delta < 0 || (strictly && delta <= 0) )
			return false;
	}
	return true;
}

//// Linear search
//-----------------

// fuzzy linear search (fallback for unsorted arrays)
template<typename T>
index_t linear_search(T x, T * table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest = false, bool ind1 = false)
{
	index_t pos = nomatch;
	double diff, diff_min = DBL_MAX;
	for ( size_t i = start; i < end; i++ )
	{
		diff = rel_diff(x, table[i], tol_ref);
		if ( diff == 0 )
			return i + ind1;
		if ( diff < diff_min ) {
			diff_min = diff;
			pos = i;
		}
	}
	if ( diff_min <= tol || nearest )
		return pos + ind1;
	else
		return nomatch;
}

template<typename T>
index_t do_linear_search(int * ptr, T * x, size_t xlen, T * table,
	size_t start, size_t end, double tol, int tol_ref, int nomatch,
	bool nearest = false, bool ind1 = false)
{
	size_t num_matches = 0;
	for ( size_t i = 0; i < xlen; i++ ) {
		if ( isNA(x[i]) )
			ptr[i] = nomatch;
		else {
			index_t pos = linear_search(x[i], table, start, end,
				tol, tol_ref, nomatch, nearest, ind1);
			ptr[i] = pos;
			num_matches++;
		}
	}
	return num_matches;
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
		double d1 = rel_change(table[start], table[mid]);
		double d2 = rel_change(table[mid], table[end - 1]);
		if ( d1 > 0 || d2 > 0 )
			return err; // table is not sorted
		delta = rel_change(x, table[mid], tol_ref);
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
		double dleft = rel_diff(x, table[left], tol_ref);
		double dmid = rel_diff(x, table[mid], tol_ref);
		double dright = rel_diff(x, table[right], tol_ref);
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

// apply binary search over a SEXP array
inline SEXP do_binary_search(SEXP x, SEXP table, double tol, int tol_ref,
	int nomatch, bool nearest = false, bool ind1 = false)
{
	SEXP pos;
	PROTECT(pos = Rf_allocVector(INTSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_binary_search(INTEGER(pos), INTEGER(x), LENGTH(x), INTEGER(table),
				0, LENGTH(table), tol, tol_ref, nomatch, nearest, ind1);
			break;
		case REALSXP:
			do_binary_search(INTEGER(pos), REAL(x), LENGTH(x), REAL(table),
				0, LENGTH(table), tol, tol_ref, nomatch, nearest, ind1);
			break;
		case STRSXP:
			do_binary_search(INTEGER(pos), STRING_PTR(x), LENGTH(x), STRING_PTR(table),
				0, LENGTH(table), tol, tol_ref, nomatch, nearest, ind1);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return pos;
}

//// Approximate search
//----------------------

// approximate search for values indexed by keys w/ interpolation
template<typename Tkey, typename Tval>
Pair<index_t,Tval> approx_search(Tkey x, Tkey * keys, Tval * values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch,
	int interp = EST_NEAR, bool sorted = true)
{
	index_t pos = NA_INTEGER;
	Tval val = nomatch;
	Pair<index_t,Tval> result;
	if ( isNA(x) ) {
		result = {pos, NA<Tval>()};
		return result;
	}
	if ( sorted )
		pos = binary_search(x, keys, start, end,
			tol, tol_ref, NA_INTEGER);
	else
		pos = linear_search(x, keys, start, end,
			tol, tol_ref, NA_INTEGER);
	if ( !isNA(pos) && pos >= 0 )
	{
		if ( tol > 0 )
			val = interp1(x, keys, values, pos, end,
				tol, tol_ref, interp, sorted);
		else
			val = values[pos];
	}
	result = {pos, val};
	return result;
}

// apply approximate search over an array of x, return via ptr
template<typename Tkey, typename Tval>
index_t do_approx_search(Tval * ptr, Tkey * x, size_t xlen, Tkey * keys, Tval * values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch, int interp = EST_NEAR,
	bool sorted = true, int stride = 1)
{
	index_t num_matches;
	if ( xlen > 2 * (end - start) && is_sorted(x, xlen) )
	{
		// x >> keys -> iterate keys (upsampling)
		int pos [end];
		num_matches = do_binary_search(pos, keys, end,
			x, 0, xlen, tol, switch_tol_ref(tol_ref), NA_INTEGER);
		for ( size_t i = 0; i < xlen; i++ ) {
			if ( isNA(x[i]) )
				ptr[i * stride] = NA<Tval>();
			else
				ptr[i * stride] = nomatch;
		}
		for ( size_t j = 0; j < end; j++ )
		{
			if ( isNA(pos[j]) )
				continue;
			for ( index_t i = pos[j]; i < xlen; i++ )
			{
				if ( rel_diff(x[i], keys[j], tol_ref) > tol )
					break;
				ptr[i * stride] = interp1(x[i], keys, values,
					j, end, tol, tol_ref, interp, sorted);
			}
			for ( index_t i = pos[j] - 1; i >= 0; i-- )
			{
				if ( rel_diff(x[i], keys[j], tol_ref) > tol )
					break;
				ptr[i * stride] = interp1(x[i], keys, values,
					j, end, tol, tol_ref, interp, sorted);
			}
		}
	}
	else
	{
		// keys >> x -> iterate x (downsampling)
		int pos [xlen];
		if ( sorted )
		{
			num_matches = do_binary_search(pos, x, xlen,
				keys, start, end, tol, tol_ref, NA_INTEGER);
			if ( num_matches == SEARCH_ERROR )
				sorted = false;
		}
		if ( !sorted )
			num_matches = do_linear_search(pos, x, xlen,
				keys, start, end, tol, tol_ref, NA_INTEGER);
		for ( size_t i = 0; i < xlen; i++ )
		{
			if ( isNA(x[i]) )
				ptr[i * stride] = NA<Tval>();
			else if ( isNA(pos[i]) )
				ptr[i * stride] = nomatch;
			else
				ptr[i * stride] = interp1(x[i], keys, values,
					pos[i], end, tol, tol_ref, interp, sorted);
		}
	}
	return num_matches;
}

// apply approximate search over array of x to SEXP keys and values
template<typename Tkey, typename Tval>
index_t do_approx_search(Tval * ptr, Tkey * x, size_t xlen, SEXP keys, SEXP values,
	double tol, int tol_ref, Tval nomatch, int interp = EST_NEAR,
	bool sorted = true, int stride = 1)
{
	return do_approx_search(ptr, x, xlen, DataPtr<Tkey>(keys), DataPtr<Tval>(values),
		0, LENGTH(values), tol, tol_ref, nomatch, interp, sorted, stride);
}

// apply approximate search with all SEXP data structures
inline SEXP do_approx_search(SEXP x, SEXP keys, SEXP values, double tol, int tol_ref,
	SEXP nomatch, int interp = EST_NEAR, bool sorted = true)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(TYPEOF(values), LENGTH(x)));
	switch(TYPEOF(values)) {
		case INTSXP:
			switch(TYPEOF(x)) {
				case INTSXP:
					do_approx_search<int,int>(INTEGER(result), INTEGER(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asInteger(nomatch), interp, sorted);
					break;
				case REALSXP:
					do_approx_search<double,int>(INTEGER(result), REAL(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asInteger(nomatch), interp, sorted);
					break;
				case STRSXP:
					do_approx_search<SEXP,int>(INTEGER(result), STRING_PTR(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asInteger(nomatch), interp, sorted);
					break;
				default:
					Rf_error("unsupported key type");
			}
			break;
		case REALSXP:
			switch(TYPEOF(x)) {
				case INTSXP:
					do_approx_search<int,double>(REAL(result), INTEGER(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asReal(nomatch), interp, sorted);
					break;
				case REALSXP:
					do_approx_search<double,double>(REAL(result), REAL(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asReal(nomatch), interp, sorted);
					break;
				case STRSXP:
					do_approx_search<SEXP,double>(REAL(result), STRING_PTR(x), LENGTH(x),
						keys, values, tol, tol_ref, Rf_asReal(nomatch), interp, sorted);
					break;
				default:
					Rf_error("unsupported key type");
			}
			break;
		default:
			Rf_error("unsupported value type");
	}
	UNPROTECT(1);
	return result;
}

#endif // SEARCH
