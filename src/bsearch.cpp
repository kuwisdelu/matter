
#include "utils.h"
#include "bsearch.h"

// search utilities

template<typename T>
double rel_change(T x, T y, int ref)
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

template<>
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

template<typename T>
double rel_diff(T x, T y, int ref)
{
	return fabs(rel_change<T>(x, y, ref));
}

template<typename T>
bool approx_eq(T x, T y, double tol, int tol_ref)
{
	return rel_diff<T>(x, y, tol_ref) < tol;
}

template<typename T, int S>
size_t linear_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	T * pTable = DataPtr<T,S>(table);
	size_t cur = start;
	double cur_diff = rel_diff(x, pTable[cur], tol_ref);
	for ( size_t i = start + 1; i < end; i++ )
	{
		diff = rel_diff(x, pTable[i], tol_ref);
		if ( diff < cur_diff )
		{
			cur = i;
			cur_diff = diff;
		}
	}
	if ( cur_diff <= tol || nearest )
		return cur;
	else
		return nomatch;
}

template<>
size_t linear_search<const char *, STRSXP>(const char * x, SEXP table,
	size_t start, size_t end, double tol, int tol_ref,
	int nomatch, bool nearest)
{
	double diff;
	size_t cur = start;
	double cur_diff = rel_diff(x, CHAR(STRING_ELT(table, cur)));
	for ( size_t i = start + 1; i < end; i++ )
	{
		diff = rel_diff(x, CHAR(STRING_ELT(table, i)), tol_ref);
		if ( diff < cur_diff )
		{
			cur = i;
			cur_diff = diff;
		}
	}
	if ( cur_diff <= tol || nearest )
		return cur;
	else
		return nomatch;
}

template<typename T, int S>
SEXP do_linear_search(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	R_xlen_t len = XLENGTH(x);
	SEXP positions;
	PROTECT(positions = Rf_allocVector(INTSXP, len));
	int * pPos = INTEGER(positions);
	T * pX = DataPtr<T,S>(x);
	R_xlen_t n = XLENGTH(table);
	for ( size_t i = 0; i < len; i++ ) {
		if ( IsNA<T>(pX[i]) )
			pPos[i] = nomatch;
		else
			pPos[i] = linear_search<T,S>(pX[i], table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( pPos[i] != nomatch )
			pPos[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return positions;
}

template<>
SEXP do_linear_search<const char *, STRSXP>(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	R_xlen_t len = XLENGTH(x);
	SEXP positions;
	PROTECT(positions = Rf_allocVector(INTSXP, len));
	int * pPos = INTEGER(positions);
	SEXP pX;
	R_xlen_t n = XLENGTH(table);
	for ( size_t i = 0; i < len; i++ ) {
		pX = STRING_ELT(x, i);
		if ( pX == NA_STRING )
			pPos[i] = nomatch;
		else
			pPos[i] = linear_search<const char *, STRSXP>(CHAR(pX), table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( pPos[i] != nomatch )
			pPos[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return positions;
}

template<typename T, int S>
size_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	size_t min = start, max = end, mid = nomatch;
	T * pTable = DataPtr<T,S>(table);
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
		size_t left = mid >= min + 1 ? mid - 1 : min;
		size_t right = mid < max - 1 ? mid + 1 : max - 1;
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

template<>
size_t binary_search<const char *, STRSXP>(const char * x, SEXP table,
	size_t start, size_t end, double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	size_t min = start, max = end, mid = nomatch;
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
		size_t left = mid >= min + 1 ? mid - 1 : min;
		size_t right = mid < max - 1 ? mid + 1 : max - 1;
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

template<typename T, int S>
SEXP do_binary_search(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	R_xlen_t len = XLENGTH(x);
	SEXP positions;
	PROTECT(positions = Rf_allocVector(INTSXP, len));
	int * pPos = INTEGER(positions);
	T * pX = DataPtr<T,S>(x);
	R_xlen_t = XLENGTH(table);
	for ( size_t i = 0; i < len; i++ ) {
		if ( IsNA<T>(pX[i]) )
			pPos[i] = nomatch;
		else
			pPos[i] = binary_search<T,S>(pX[i], table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( pPos[i] != nomatch )
			pPos[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return positions;
}

template<>
SEXP do_binary_search<const char *, STRSXP>(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest)
{
	R_xlen_t len = XLENGTH(x);
	SEXP positions;
	PROTECT(positions = Rf_allocVector(INTSXP, len));
	int * pPos = INTEGER(positions);
	SEXP pX;
	R_xlen_t n = XLENGTH(table);
	for ( size_t i = 0; i < len; i++ ) {
		pX = STRING_ELT(x, i);
		if ( pX == NA_STRING )
			pPos[i] = nomatch;
		else
			pPos[i] = binary_search<const char *, STRSXP>(CHAR(pX), table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( pPos[i] != nomatch )
			pPos[i] += 1; // adjust for R indexing from 1
	}
	UNPROTECT(1);
	return positions;
}

template<typename TKey, int SKey, typename TVal, int SVal>
SEXP get_keyvals_sorted(TVal * ptr, SEXP x, SEXP keys, SEXP values,
	double tol, int tol_ref, TVal nomatch, bool nearest, int dups = SUM_DUPS)
{
	size_t count;
	R_xlen_t xlen = XLENGTH(x);
	R_xlen_t keylen = XLENGTH(table);
	TVal * pValues = DataPtr<TVal,SVal>(values);
	TKey * pX = DataPtr<TKey,SKey>(x);
	for ( size_t ix = 0, ikey = 0; ix < xlen; ix++ ) {
		if ( IsNA<T>(pX[ix]) )
			ptr[i] = nomatch;
		else
		{
			ikey = binary_search(pX[ix], keys, ikey, keylen,
				tol, tol_ref, NA_INTEGER, nearest)
			if ( ikey != NA_INTEGER )
			{
				ptr[ix] = pValues[ikey];
				for ( int j = 1; ix - j >= 0; j++ )
				{
					if ( approx_eq(pX[ix - j], keys, tol, tol_ref) )
					{
						TVal dupVal = pValues[ikey];
						switch(dups) {
							case SUM_DUPS:
								ptr[ix] += dupVal;
							case MIN_DUPS:
								ptr[ix] = dupVal < ptr[ix] ? dupVal : ptr[ix];
							case MAX_DUPS:
								ptr[ix] = dupVal > ptr[ix] ? dupVal : ptr[ix];
						}
					}
					else
						break;
				}
				for ( int k = 1; ix + k >= 0; j++ )
				{
					if ( approx_eq(pX[ix + k], keys, tol, tol_ref) )
					{
						TVal dupVal = pValues[ikey];
						switch(dups) {
							case SUM_DUPS:
								ptr[ix] += dupVal;
							case MIN_DUPS:
								ptr[ix] = dupVal < ptr[ix] ? dupVal : ptr[ix];
							case MAX_DUPS:
								ptr[ix] = dupVal > ptr[ix] ? dupVal : ptr[ix];
						}
					}
					else
						break;
				}
				count++;
			}
			else
				ptr[ix] = nomatch;
		}
	}
	UNPROTECT(1);
	return count;
}

extern "C" {

	SEXP relativeDiff(SEXP x, SEXP y, SEXP ref)
	{
		if ( TYPEOF(x) != TYPEOF(y) )
			Rf_error("'x' and 'y' must have the same type");
		double _ref = Rf_asInteger(ref);
		if ( _ref != ABS_DIFF && _ref != REL_DIFF_X && _ref != REL_DIFF_Y )
			Rf_error("unrecognized value for 'ref' (must be 1, 2, or 3");
		switch(TYPEOF(x)) {
			case STRSXP:
				return Rf_ScalarReal(rel_diff<const char *>(CHAR(Rf_asChar(x)),
					CHAR(Rf_asChar(y)), _ref));
			case INTSXP:
				return Rf_ScalarReal(rel_diff<int>(Rf_asInteger(x),
					Rf_asInteger(y), _ref));
			case REALSXP:
				return Rf_ScalarReal(rel_diff<double>(Rf_asReal(x),
					Rf_asReal(y), _ref));
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

	SEXP linearSearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(x) != TYPEOF(table) )
			Rf_error("'x' and 'table' must have the same type");
		double _tol = Rf_asReal(tol);
		if ( _tol < 0 )
			Rf_error("'tol' must be non-negative");
		int _tol_ref = Rf_asInteger(tol_ref);
		int _nomatch = Rf_asInteger(nomatch);
		bool _nearest = static_cast<bool>(Rf_asLogical(nearest));
		switch(TYPEOF(x)) {
			case STRSXP:
				return do_linear_search<const char *, STRSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
			case INTSXP:
				return do_linear_search<int, INTSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
			case REALSXP:
				return do_linear_search<double, REALSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

	SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(x) != TYPEOF(table) )
			Rf_error("'x' and 'table' must have the same type");
		double _tol = Rf_asReal(tol);
		if ( _tol < 0 )
			Rf_error("'tol' must be non-negative");
		int _tol_ref = Rf_asInteger(tol_ref);
		int _nomatch = Rf_asInteger(nomatch);
		bool _nearest = static_cast<bool>(Rf_asLogical(nearest));
		switch(TYPEOF(x)) {
			case STRSXP:
				return do_binary_search<const char *, STRSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
			case INTSXP:
				return do_binary_search<int, INTSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
			case REALSXP:
				return do_binary_search<double, REALSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest);
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

}



