
#ifndef MATTER_EXPORTS
#define MATTER_EXPORTS

#include "drle.h"
#include "atoms.h"
#include "search.h"
#include "sparse.h"

extern "C" {

	// Search (binary and approximate)
	//--------------------------------

	static inline SEXP relativeDiff(SEXP x, SEXP y, SEXP ref)
	{
		if ( TYPEOF(x) != TYPEOF(y) )
			Rf_error("'x' and 'y' must have the same type");
		return Rf_ScalarReal(rel_diff(x, y, Rf_asInteger(ref)));
	}

	static inline SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP nearest)
	{
		if ( TYPEOF(x) != TYPEOF(table) )
			Rf_error("'x' and 'table' must have the same type");
		if ( Rf_asReal(tol) < 0 )
			Rf_error("'tol' must be non-negative");
		return do_binary_search(x, table, Rf_asReal(tol), Rf_asInteger(tol_ref),
			Rf_asInteger(nomatch), Rf_asLogical(nearest), TRUE);
	}

	static inline SEXP approxSearch(SEXP x, SEXP keys, SEXP values, SEXP tol,
		SEXP tol_ref, SEXP nomatch, SEXP interp, SEXP sorted)
	{
		if ( TYPEOF(x) != TYPEOF(keys) )
			Rf_error("'x' and 'keys' must have the same type");
		if ( Rf_asReal(tol) < 0 )
			Rf_error("'tol' must be non-negative");
		return do_approx_search(x, keys, values,
			Rf_asReal(tol), Rf_asInteger(tol_ref), nomatch,
			Rf_asInteger(interp), Rf_asLogical(sorted));
	}

	// Compression (delta run length encoding)
	//-----------------------------------------

	static inline SEXP encodeDRLE(SEXP x, SEXP cr)
	{
		return encode_drle(x, Rf_asReal(cr));
	}

	static inline SEXP recodeDRLE(SEXP x, SEXP i)
	{
		return recode_drle(x, i);
	}

	static inline SEXP decodeDRLE(SEXP x, SEXP i)
	{
		return decode_drle(x, i);
	}

	// Internal testing for Atoms
	//---------------------------

	static inline SEXP readAtom(SEXP x, SEXP i, SEXP type)
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

	static inline SEXP writeAtom(SEXP x, SEXP i, SEXP value)
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

	static inline SEXP readAtoms(SEXP x, SEXP indx, SEXP type, SEXP grp)
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

	static inline SEXP writeAtoms(SEXP x, SEXP indx, SEXP value, SEXP grp)
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

	static inline SEXP subsetAtoms(SEXP x, SEXP indx)
	{
		Atoms2 y(x);
		R_xlen_t size = XLENGTH(indx);
		switch(TYPEOF(indx)) {
			case INTSXP:
				return y.subset_index<int>(INTEGER(indx), size, true);
			case REALSXP:
				return y.subset_index<double>(REAL(indx), size, true);
			default:
				y.self_destruct();
				Rf_error("invalid index type");
		}
	}

	// Sparse data structures
	//-----------------------

	static inline SEXP getSparseVector(SEXP x, SEXP i)
	{
		SparseVector y(x);
		return y.getElements(i);
	}

	static inline SEXP getSparseMatrixC(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixC y(x);	
		return y.getSubMatrix(i, j);
	}

	static inline SEXP getSparseMatrixR(SEXP x, SEXP i, SEXP j)
	{
		SparseMatrixR y(x);	
		return y.getSubMatrix(i, j);
	}

}

#endif // MATTER_EXPORTS
