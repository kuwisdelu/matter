
#ifndef MATTER_EXPORTS
#define MATTER_EXPORTS

#include "signal.h"
#include "search.h"
#include "sparse.h"

extern "C" {

	SEXP relativeDiff(SEXP x, SEXP y, SEXP ref)
	{
		if ( TYPEOF(x) != TYPEOF(y) )
			Rf_error("'x' and 'y' must have the same type");
		return Rf_ScalarReal(rel_diff(x, y, Rf_asInteger(ref)));
	}

	SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(x) != TYPEOF(table) )
			Rf_error("'x' and 'table' must have the same type");
		if ( Rf_asReal(tol) < 0 )
			Rf_error("'tol' must be non-negative");
		return do_binary_search(x, table, Rf_asReal(tol), Rf_asInteger(tol_ref),
			Rf_asInteger(nomatch), Rf_asLogical(nearest), TRUE);
	}

	SEXP approxSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP interp, SEXP sorted)
	{
		if ( TYPEOF(x) != TYPEOF(keys) )
			Rf_error("'x' and 'keys' must have the same type");
		if ( Rf_asReal(tol) < 0 )
			Rf_error("'tol' must be non-negative");
		switch(TYPEOF(values)) {
			case INTSXP:
				return do_approx_search<int>(x, keys, values,
					Rf_asReal(tol), Rf_asInteger(tol_ref),
					Rf_asReal(nomatch), Rf_asInteger(interp),
					Rf_asLogical(sorted));
			case REALSXP:
				return do_approx_search<double>(x, keys, values,
					Rf_asReal(tol), Rf_asInteger(tol_ref),
					Rf_asReal(nomatch), Rf_asInteger(interp),
					Rf_asLogical(sorted));
			default:
				Rf_error("unsupported data type");
		}
	}

	SEXP getSparseVector(SEXP x, SEXP i)
	{
		SparseVector y(x);
		return y.getElements(i);
	}

	SEXP getSparseMatrixC(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixC y(x);	
		return y.getElements(i, j);
	}

	SEXP getSparseMatrixR(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixR y(x);	
		return y.getElements(i, j);
	}

}

#endif // MATTER_EXPORTS
