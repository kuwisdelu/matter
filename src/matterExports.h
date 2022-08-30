
#ifndef MATTER_EXPORTS
#define MATTER_EXPORTS

#include "drle.h"
#include "atoms.h"
#include "signal.h"
#include "search.h"
#include "sparse.h"

extern "C" {

	// search

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

	// compression

	SEXP encodeDRLE(SEXP x, SEXP cr)
	{
		return encode_drle(x, Rf_asReal(cr));
	}

	SEXP recodeDRLE(SEXP x, SEXP i)
	{
		return recode_drle(x, i);
	}

	SEXP decodeDRLE(SEXP x, SEXP i)
	{
		SEXP values = R_do_slot(x, Rf_install("values"));
		switch(TYPEOF(values)) {
			case INTSXP: {
				CompressedVector<int> y(x);
				return y.getElements(i);
			}
			case REALSXP: {
				CompressedVector<double> y(x);
				return y.getElements(i);
			}
			default:
				Rf_error("unsupported data type");
		}
	}

	// internal

	SEXP readAtom(SEXP x, SEXP i, SEXP type)
	{
		SEXP ans;
		Atoms2 y(x);
		int j = Rf_asInteger(i);
		R_xlen_t len = static_cast<R_xlen_t>(y.extent(j));
		switch(Rf_asInteger(type)) {
			case R_RAW:
				PROTECT(ans = Rf_allocVector(RAWSXP, len));
				y.get_atom<Rbyte>(RAW(ans), j, 0, len);
				break;
			case R_INTEGER:
				PROTECT(ans = Rf_allocVector(INTSXP, len));
				y.get_atom<int>(INTEGER(ans), j, 0, len);
				break;
			case R_DOUBLE:
				PROTECT(ans = Rf_allocVector(REALSXP, len));
				y.get_atom<double>(REAL(ans), j, 0, len);
				break;
			default:
				y.self_destruct();
				Rf_error("data type must be raw, integer, or double");
		}
		UNPROTECT(1);
		return ans;
	}

	SEXP writeAtom(SEXP x, SEXP i, SEXP value)
	{
		Atoms2 y(x);
		int j = Rf_asInteger(i);
		R_xlen_t len = static_cast<R_xlen_t>(y.extent(j));
		if ( len != XLENGTH(value) ) {
			y.self_destruct();
			Rf_error("length of value does not match atom extent");
		}
		switch(TYPEOF(value)) {
			case RAWSXP:
				y.set_atom<Rbyte>(RAW(value), j, 0, len);
				break;
			case INTSXP:
				y.set_atom<int>(INTEGER(value), j, 0, len);
				break;
			case REALSXP:
				y.set_atom<double>(REAL(value), j, 0, len);
				break;
			default:
				y.self_destruct();
				Rf_error("data type must be raw, integer, or double");
		}
		return x;
	}

	SEXP readAtoms(SEXP x, SEXP indx, SEXP type, SEXP grp)
	{
		SEXP ans;
		Atoms2 y(x);
		int g = Rf_asInteger(grp);
		R_xlen_t len = XLENGTH(indx);
		switch(Rf_asInteger(type)) {
			case R_RAW:
				PROTECT(ans = Rf_allocVector(RAWSXP, len));
				y.get_elements<Rbyte>(RAW(ans), indx, g);
				break;
			case R_INTEGER:
				PROTECT(ans = Rf_allocVector(INTSXP, len));
				y.get_elements<int>(INTEGER(ans), indx, g);
				break;
			case R_DOUBLE:
				PROTECT(ans = Rf_allocVector(REALSXP, len));
				y.get_elements<double>(REAL(ans), indx, g);
				break;
			default:
				y.self_destruct();
				Rf_error("data type must be raw, integer, or double");
		}
		UNPROTECT(1);
		return ans;
	}

	SEXP writeAtoms(SEXP x, SEXP indx, SEXP value, SEXP grp)
	{
		Atoms2 y(x);
		int g = Rf_asInteger(grp);
		R_xlen_t len = XLENGTH(indx);
		if ( len != XLENGTH(value) ) {
			y.self_destruct();
			Rf_error("length of value does not match atom extent");
		}
		switch(TYPEOF(value)) {
			case RAWSXP:
				y.set_elements<Rbyte>(RAW(value), indx, g);
				break;
			case INTSXP:
				y.set_elements<int>(INTEGER(value), indx, g);
				break;
			case REALSXP:
				y.set_elements<double>(REAL(value), indx, g);
				break;
			default:
				y.self_destruct();
				Rf_error("data type must be raw, integer, or double");
		}
		return x;
	}

	// data structures

	SEXP getSparseVector(SEXP x, SEXP i)
	{
		SparseVector y(x);
		return y.getElements(i);
	}

	SEXP getSparseMatrixC(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixC y(x);	
		return y.getSubMatrix(i, j);
	}

	SEXP getSparseMatrixR(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixR y(x);	
		return y.getSubMatrix(i, j);
	}

}

#endif // MATTER_EXPORTS
