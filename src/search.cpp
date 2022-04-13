
#include "search.h"

// search utilities

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
				return do_binary_search<const char *>(x, table,
					_tol, _tol_ref, _nomatch, _nearest, TRUE);
			case INTSXP:
				return do_binary_search<int>(x, table,
					_tol, _tol_ref, _nomatch, _nearest, TRUE);
			case REALSXP:
				return do_binary_search<double>(x, table,
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
				int dnomatch = Rf_asInteger(nomatch);
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_keyval_search<int, int, INTSXP>(x,
							keys, values, _tol, _tol_ref, dnomatch, _dups, _sorted);
					case REALSXP:
						return do_keyval_search<double, int, INTSXP>(x,
							keys, values, _tol, _tol_ref, dnomatch, _dups, _sorted);
				}
			}
			case REALSXP: {
				double fnomatch = Rf_asReal(nomatch);
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_keyval_search<int, double, REALSXP>(x,
							keys, values, _tol, _tol_ref, fnomatch, _dups, _sorted);
					case REALSXP:
						return do_keyval_search<double, double, REALSXP>(x,
							keys, values, _tol, _tol_ref, fnomatch, _dups, _sorted);
				}
			}
		}
		Rf_error("supported types are 'integer' or 'numeric'");
	}

}



