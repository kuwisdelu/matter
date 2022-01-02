
#include "utils.h"
#include "search.h"

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
bool is_sorted(SEXP x, bool strictly)
{
	R_xlen_t len = XLENGTH(x);
	T * pX = DataPtr<T,S>(x);
	for ( size_t i = 1; i < len; i++ ) {
		double diff = rel_change(pX[i], pX[i - 1]);
		if ( diff < 0 || (strictly && diff <= 0) )
			return FALSE;
	}
	return TRUE;
}

template<>
bool is_sorted<const char *, STRSXP>(SEXP x, bool strictly)
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

template<typename T, int S>
index_t binary_search(T x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest)
{
	double diff;
	index_t min = start, max = end, mid = nomatch;
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

template<>
index_t binary_search<const char *, STRSXP>(const char * x, SEXP table,
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

template<typename T, int S>
size_t do_binary_search(int * ptr, SEXP x, SEXP table, size_t start, size_t end,
	double tol, int tol_ref, int nomatch, bool nearest, bool index1, index_t skip)
{
	R_xlen_t len = XLENGTH(x);
	T * pX = DataPtr<T,S>(x);
	R_xlen_t n = XLENGTH(table);
	size_t num_matches = 0;
	for ( size_t i = 0; i < len; i++ ) {
		if ( IsNA<T>(pX[i]) )
			ptr[i] = nomatch;
		else
			ptr[i] = binary_search<T,S>(pX[i], table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( ptr[i] != nomatch ) {
			num_matches++;
			ptr[i] += index1; // adjust for R indexing from 1
		}
	}
	return num_matches;
}

template<>
size_t do_binary_search<const char *, STRSXP>(int * ptr, SEXP x, SEXP table,
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
			ptr[i] = binary_search<const char *, STRSXP>(CHAR(pX), table, 0, n,
				tol, tol_ref, nomatch, nearest);
		if ( ptr[i] != nomatch ) {
			num_matches++;
			ptr[i] += index1; // adjust for R indexing from 1
		}
	}
	return num_matches;
}

template<typename T, int S>
SEXP do_binary_search(SEXP x, SEXP table, double tol,
	int tol_ref, int nomatch, bool nearest, bool index1)
{
	R_xlen_t len = XLENGTH(x);
	SEXP positions;
	PROTECT(positions = Rf_allocVector(INTSXP, len));
	int * pPos = INTEGER(positions);
	do_binary_search<T, S>(pPos, x, table, 0, len,
		tol, tol_ref, nomatch, nearest, index1, 1);
	UNPROTECT(1);
	return positions;
}

template<typename Tkey, int Skey, typename Tval, int Sval>
size_t do_keyval_search(Tval * ptr, SEXP x, SEXP keys, SEXP values,
	size_t start, size_t end, double tol, int tol_ref, Tval nomatch,
	int dups, bool sorted, index_t skip)
{
	R_xlen_t xlen = XLENGTH(x);
	Tkey * pX = DataPtr<Tkey,Skey>(x);
	Tkey * pKeys = DataPtr<Tkey,Skey>(keys);
	Tval * pValues = DataPtr<Tval,Sval>(values);
	size_t num_matches = 0;
	for ( index_t ix = 0, ikey = start; ix < xlen; ix += skip )
	{
		ptr[ix] = nomatch;
		if ( !IsNA<Tkey>(pX[ix]) )
		{
			if ( sorted ) {
				// sorted keys -- binary search
				index_t ifound = binary_search<Tkey, Skey>(pX[ix], keys,
					ikey, end, tol, tol_ref, NA_INTEGER, FALSE);
				if ( ifound != NA_INTEGER ) {
					num_matches++;
					ikey = ifound;
					ptr[ix] = pValues[ikey];
					if ( dups != NO_DUPS )
					{
						for ( int j = 1; ikey - j >= 0; j++ )
						{
							if ( approx_eq<Tkey>(pX[ix], pKeys[ikey - j], tol, tol_ref) )
							{
								Tval dupVal = pValues[ikey - j];
								switch(dups) {
									case SUM_DUPS:
										ptr[ix] += dupVal;
										break;
									case MAX_DUPS:
										ptr[ix] = dupVal > ptr[ix] ? dupVal : ptr[ix];
										break;
									case MIN_DUPS:
										ptr[ix] = dupVal < ptr[ix] ? dupVal : ptr[ix];
										break;
								}
							}
							else
								break;
						}
						for ( int k = 1; ikey + k >= 0; k++ )
						{
							if ( approx_eq<Tkey>(pX[ix], pKeys[ikey + k], tol, tol_ref) )
							{
								Tval dupVal = pValues[ikey + k];
								switch(dups) {
									case SUM_DUPS:
										ptr[ix] += dupVal;
										break;
									case MAX_DUPS:
										ptr[ix] = dupVal > ptr[ix] ? dupVal : ptr[ix];
										break;
									case MIN_DUPS:
										ptr[ix] = dupVal < ptr[ix] ? dupVal : ptr[ix];
										break;
								}
							}
							else
								break;
						}
					}
					if ( ix + 1 < xlen && pX[ix + 1] < pX[ix] )
						ikey = start;
				}
			}
			else {
				// unsorted keys -- linear search
				size_t icount = 0;
				double min_diff = DBL_MAX;
				for ( ikey = start; ikey < end; ikey++ )
				{
					double diff = rel_diff<Tkey>(pX[ix], pKeys[ikey], tol_ref);
					if ( diff <= tol )
					{
						num_matches++;
						icount++;
						switch(dups) {
							case NO_DUPS:
								ptr[ix] = diff < min_diff ? pValues[ikey] : ptr[ix];
								break;
							case SUM_DUPS:
								ptr[ix] = icount > 1 ? pValues[ikey] + ptr[ix] : pValues[ikey];
								break;
							case MAX_DUPS:
								ptr[ix] = icount == 1 || pValues[ikey] > ptr[ix] ? pValues[ikey] : ptr[ix];
								break;
							case MIN_DUPS:
								ptr[ix] = icount == 1 || pValues[ikey] < ptr[ix] ? pValues[ikey] : ptr[ix];
								break;
						}
						if ( diff < min_diff )
							min_diff = diff;
						if ( dups == NO_DUPS && diff == 0 )
							break;
					}
				}
			}
		}
	}
	return num_matches;
}

template<typename Tkey, int Skey, typename Tval, int Sval>
SEXP do_keyval_search(SEXP x, SEXP keys, SEXP values, double tol,
	int tol_ref, Tval nomatch, int dups, bool sorted)
{
	R_xlen_t xlen = XLENGTH(x);
	R_xlen_t keylen = XLENGTH(keys);
	SEXP out;
	PROTECT(out = Rf_allocVector(Sval, xlen));
	Tval * pOut = DataPtr<Tval,Sval>(out);
	do_keyval_search<Tkey, Skey, Tval, Sval>(pOut, x, keys, values,
		0, keylen, tol, tol_ref, nomatch, dups, sorted, 1);
	UNPROTECT(1);
	return out;
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
					_tol, _tol_ref, _nomatch, _nearest, TRUE);
			case INTSXP:
				return do_binary_search<int, INTSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest, TRUE);
			case REALSXP:
				return do_binary_search<double, REALSXP>(x, table,
					_tol, _tol_ref, _nomatch, _nearest, TRUE);
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

	SEXP keyvalSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP dups, SEXP sorted)
	{
		if ( TYPEOF(x) != TYPEOF(keys) )
			Rf_error("'x' and 'keys' must have the same type");
		double _tol = Rf_asReal(tol);
		if ( _tol < 0 )
			Rf_error("'tol' must be non-negative");
		int _tol_ref = Rf_asInteger(tol_ref);
		int _dups = Rf_asInteger(dups);
		bool _sorted = static_cast<bool>(Rf_asLogical(sorted));
		switch(TYPEOF(values)) {
			case INTSXP: {
				int _dnomatch = Rf_asInteger(nomatch);
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_keyval_search<int, INTSXP, int, INTSXP>(x,
							keys, values, _tol, _tol_ref, _dnomatch, _dups, _sorted);
					case REALSXP:
						return do_keyval_search<double, REALSXP, int, INTSXP>(x,
							keys, values, _tol, _tol_ref, _dnomatch, _dups, _sorted);
				}
			}
			case REALSXP: {
				double _fnomatch = Rf_asReal(nomatch);
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_keyval_search<int, INTSXP, double, REALSXP>(x,
							keys, values, _tol, _tol_ref, _fnomatch, _dups, _sorted);
					case REALSXP:
						return do_keyval_search<double, REALSXP, double, REALSXP>(x,
							keys, values, _tol, _tol_ref, _fnomatch, _dups, _sorted);
				}
			}
		}
		Rf_error("supported types are 'integer' or 'numeric'");
	}

}



