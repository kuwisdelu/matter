
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
		double ref_ = Rf_asInteger(ref);
		if ( ref_ != ABS_DIFF && ref_ != REL_DIFF_X && ref_ != REL_DIFF_Y )
			Rf_error("unrecognized value for 'ref' (must be 1, 2, or 3");
		switch(TYPEOF(x)) {
			case INTSXP:
				return Rf_ScalarReal(rel_diff<int>(
					Rf_asInteger(x), Rf_asInteger(y), ref_));
			case REALSXP:
				return Rf_ScalarReal(rel_diff<double>(
					Rf_asReal(x), Rf_asReal(y), ref_));
			case STRSXP:
				return Rf_ScalarReal(rel_diff<const char *>(
					CHAR(Rf_asChar(x)), CHAR(Rf_asChar(y)), ref_));
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

	SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(x) != TYPEOF(table) )
			Rf_error("'x' and 'table' must have the same type");
		double tol_ = Rf_asReal(tol);
		if ( tol_ < 0 )
			Rf_error("'tol' must be non-negative");
		int tol_ref_ = Rf_asInteger(tol_ref);
		int nomatch_ = Rf_asInteger(nomatch);
		bool nearest_ = static_cast<bool>(Rf_asLogical(nearest));
		switch(TYPEOF(x)) {
			case INTSXP:
				return do_binary_search<int>(x, table,
					tol_, tol_ref_, nomatch_, nearest_, TRUE);
			case REALSXP:
				return do_binary_search<double>(x, table,
					tol_, tol_ref_, nomatch_, nearest_, TRUE);
			case STRSXP:
				return do_binary_search<SEXP>(x, table,
					tol_, tol_ref_, nomatch_, nearest_, TRUE);
		}
		Rf_error("supported types are 'integer', 'numeric', or 'character'");
	}

	SEXP approxSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP interp, SEXP sorted)
	{
		if ( TYPEOF(x) != TYPEOF(keys) )
			Rf_error("'x' and 'keys' must have the same type");
		double tol_ = Rf_asReal(tol);
		if ( tol_ < 0 )
			Rf_error("'tol' must be non-negative");
		int tol_ref_ = Rf_asInteger(tol_ref);
		int interp_ = Rf_asInteger(interp);
		bool sorted_ = static_cast<bool>(Rf_asLogical(sorted));
		switch(TYPEOF(values)) {
			case INTSXP: {
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_approx_search<int,int>(
							x, keys, values, tol_, tol_ref_,
							Rf_asInteger(nomatch), interp_, sorted_);
					case REALSXP:
						return do_approx_search<double,int>(
							x, keys, values, tol_, tol_ref_,
							Rf_asInteger(nomatch), interp_, sorted_);
					case STRSXP:
						return do_approx_search<SEXP,int>(
							x, keys, values, tol_, tol_ref_,
							Rf_asInteger(nomatch), interp_, sorted_);
				}
				Rf_error("supported key types are 'integer', 'numeric', or 'character'");
			}
			case REALSXP: {
				switch(TYPEOF(keys)) {
					case INTSXP:
						return do_approx_search<int,double>(
							x, keys, values, tol_, tol_ref_,
							Rf_asReal(nomatch), interp_, sorted_);
					case REALSXP:
						return do_approx_search<double,double>(
							x, keys, values, tol_, tol_ref_,
							Rf_asReal(nomatch), interp_, sorted_);
					case STRSXP:
						return do_approx_search<SEXP,double>(
							x, keys, values, tol_, tol_ref_,
							Rf_asReal(nomatch), interp_, sorted_);
				}
				Rf_error("supported key types are 'integer', 'numeric', or 'character'");
			}
		}
		Rf_error("supported value types are 'integer' or 'numeric'");
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
