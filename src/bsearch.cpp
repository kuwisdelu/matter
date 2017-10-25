
#include "bsearch.h"

int strmatch(const char * s1, const char * s2) {
	int nmatch = 0;
	while ( *s1 == *s2 ) {
		if ( *s1 == '\0' )
			break;
		nmatch++;
		s1++;
		s2++;
	}
	return nmatch;
}

double dblcmp(double x, double y, int relative) {
	switch(relative) {
		case ABS_DIFF:
			return x - y;
		case REL_DIFF_X:
			return (x - y) / x;
		case REL_DIFF_Y:
			return (x - y) / y;
		default:
			return NA_REAL;
	}
}

template<>
size_t binary_search<const char *>(const char * key, SEXP values,
	size_t start, size_t end, double tol, int tol_ref, int nomatch, bool nearest)
{
	int result;
	size_t min = start, max = end, mid = nomatch;
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		result = strcmp(key, CHAR(STRING_ELT(values, mid)));
		if ( result < 0 )
			end = mid;
		else if ( result > 0 )
			start = mid + 1;
		else if ( result == 0 )
			return mid;
	}
	if ( nearest ) {
		size_t left = mid >= min + 1 ? mid - 1 : min;
		size_t right = mid < max - 1 ? mid + 1 : max - 1;
		if ( mid == left && result < 0 )
			return left;
		else if ( mid == right && result > 0 )
			return right;
		else {
			int nleft = strmatch(key, CHAR(STRING_ELT(values, left)));
			int nmid = strmatch(key, CHAR(STRING_ELT(values, mid)));
			int nright = strmatch(key, CHAR(STRING_ELT(values, right)));
			if ( nleft >= nmid && nleft >= nright )
				return left;
			else if ( nmid >= nleft && nmid >= nright )
				return mid;
			else
				return right;
		}
	}
	else
		return nomatch;
}

template<>
size_t binary_search<int>(int key, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	size_t min = start, max = end, mid = nomatch;
	int * pValues = INTEGER(values);
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		if ( key < pValues[mid] )
			end = mid;
		else if ( key > pValues[mid] )
			start = mid + 1;
		else if ( key == pValues[mid] )
			return mid;
	}
	if ( nearest ) {
		size_t left = mid >= min + 1 ? mid - 1 : min;
		size_t right = mid < max - 1 ? mid + 1 : max - 1;
		if ( mid == left && key < pValues[left] )
			return left;
		else if ( mid == right && key > pValues[right] )
			return right;
		else {
			int dleft = abs(key - pValues[left]);
			int dmid = abs(key - pValues[mid]);
			int dright = abs(key - pValues[right]);
			if ( dleft <= dmid && dleft <= dright )
				return left;
			else if ( dmid <= dleft && dmid <= dright )
				return mid;
			else
				return right;
		}
	}
	else
		return nomatch;
}

template<>
size_t binary_search<double>(double key, SEXP values, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	size_t min = start, max = end, mid = nomatch;
	double * pValues = REAL(values);
	while ( start < end )
	{
		mid = start + (end - start) / 2;
		diff = dblcmp(key, pValues[mid], tol_ref);
		if ( fabs(diff) > tol ) {
			if ( diff < 0 )
				end = mid;
			else if ( diff > 0 )
				start = mid + 1;	
		}
		else
			return mid;
	}
	if ( nearest ) {
		size_t left = mid >= min + 1 ? mid - 1 : min;
		size_t right = mid < max - 1 ? mid + 1 : max - 1;
		if ( mid == left && diff < 0 )
			return left;
		else if ( mid == right && diff > 0 )
			return right;
		else {
			double dleft = fabs(dblcmp(key, pValues[left]));
			double dmid = fabs(dblcmp(key, pValues[mid]));
			double dright = fabs(dblcmp(key, pValues[right]));
			if ( dleft <= dmid && dleft <= dright )
				return left;
			else if ( dmid <= dleft && dmid <= dright )
				return mid;
			else
				return right;
		}
	}
	else
		return nomatch;
}

template<>
SEXP map_binary_search<STRSXP>(SEXP key, SEXP values, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	int num_keys = LENGTH(key);
	SEXP index;
	PROTECT(index = NEW_INTEGER(num_keys));
	int * pIndex = INTEGER(index);
	const char * pKey;
	int n = LENGTH(values);
	for ( int i = 0; i < num_keys; i++ ) {
		pKey = CHAR(STRING_ELT(key, i));
		pIndex[i] = binary_search<const char *>(pKey, values, 0, n,
			tol, tol_ref, nomatch, nearest);
		if ( pIndex[i] != nomatch )
			pIndex[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return index;
}

template<>
SEXP map_binary_search<INTSXP>(SEXP key, SEXP values, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	int num_keys = LENGTH(key);
	SEXP index;
	PROTECT(index = NEW_INTEGER(num_keys));
	int * pIndex = INTEGER(index);
	int * pKey = INTEGER(key);
	int n = LENGTH(values);
	for ( int i = 0; i < num_keys; i++ ) {
		pIndex[i] = binary_search<int>(pKey[i], values, 0, n,
			tol, tol_ref, nomatch, nearest);
		if ( pIndex[i] != nomatch )
			pIndex[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return index;
}

template<>
SEXP map_binary_search<REALSXP>(SEXP key, SEXP values, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	int num_keys = LENGTH(key);
	SEXP index;
	PROTECT(index = NEW_INTEGER(num_keys));
	int * pIndex = INTEGER(index);
	double * pKey = REAL(key);
	int n = LENGTH(values);
	for ( int i = 0; i < num_keys; i++ ) {
		pIndex[i] = binary_search<double>(pKey[i], values, 0, n,
			tol, tol_ref, nomatch, nearest);
		if ( pIndex[i] != nomatch )
			pIndex[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return index;
}

extern "C" {

	SEXP binarySearch(SEXP key, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(key) != TYPEOF(values) )
			error("'key' and 'values' must have the same type");
		double _tol = NUMERIC_VALUE(tol);
		int _tol_ref = INTEGER_VALUE(tol_ref);
		int _nomatch = INTEGER_VALUE(nomatch);
		bool _nearest = static_cast<bool>(LOGICAL_VALUE(nearest));
		switch(TYPEOF(key)) {
            case STRSXP:
        		return map_binary_search<STRSXP>(key, values,
					_tol, _tol_ref, _nomatch, _nearest);
                break;
            case INTSXP:
        		return map_binary_search<INTSXP>(key, values,
					_tol, _tol_ref, _nomatch, _nearest);
                break;
            case REALSXP:
        		return map_binary_search<REALSXP>(key, values,
					_tol, _tol_ref, _nomatch, _nearest);
                break;
        }
        error("supported types are 'integer', 'numeric', or 'character'");
	}

}



