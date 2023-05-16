
#include "matterExports.h"

extern "C" {

// Search (binary and approximate)
//--------------------------------

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
	return do_approx_search(x, keys, values,
		Rf_asReal(tol), Rf_asInteger(tol_ref), nomatch,
		Rf_asInteger(interp), Rf_asLogical(sorted));
}

// Compression (delta run length encoding)
//-----------------------------------------

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
	return decode_drle(x, i);
}

// Internal testing for Atoms
//---------------------------

SEXP readAtom(SEXP x, SEXP i, SEXP type)
{
	SEXP ans;
	Atoms xm(x);
	int j = Rf_asInteger(i);
	R_xlen_t len = static_cast<R_xlen_t>(xm.extent(j));
	switch(Rf_asInteger(type)) {
		case R_RAW:
			PROTECT(ans = Rf_allocVector(RAWSXP, len));
			xm.get_atom<Rbyte>(RAW(ans), j, 0, len);
			break;
		case R_INTEGER:
			PROTECT(ans = Rf_allocVector(INTSXP, len));
			xm.get_atom<int>(INTEGER(ans), j, 0, len);
			break;
		case R_DOUBLE:
			PROTECT(ans = Rf_allocVector(REALSXP, len));
			xm.get_atom<double>(REAL(ans), j, 0, len);
			break;
		default:
			xm.self_destruct();
			Rf_error("data type must be raw, integer, or double");
	}
	UNPROTECT(1);
	return ans;
}

SEXP writeAtom(SEXP x, SEXP i, SEXP value)
{
	Atoms xa(x);
	int j = Rf_asInteger(i);
	R_xlen_t len = static_cast<R_xlen_t>(xa.extent(j));
	if ( len != XLENGTH(value) ) {
		xa.self_destruct();
		Rf_error("length of value does not match atom extent");
	}
	switch(TYPEOF(value)) {
		case RAWSXP:
			xa.set_atom<Rbyte>(RAW(value), j, 0, len);
			break;
		case INTSXP:
			xa.set_atom<int>(INTEGER(value), j, 0, len);
			break;
		case REALSXP:
			xa.set_atom<double>(REAL(value), j, 0, len);
			break;
		default:
			xa.self_destruct();
			Rf_error("data type must be raw, integer, or double");
	}
	return x;
}

SEXP readAtoms(SEXP x, SEXP indx, SEXP type, SEXP grp)
{
	SEXP ans;
	Atoms xa(x);
	int g = Rf_asInteger(grp);
	R_xlen_t len = XLENGTH(indx);
	switch(Rf_asInteger(type)) {
		case R_RAW:
			PROTECT(ans = Rf_allocVector(RAWSXP, len));
			xa.get_elements<Rbyte>(RAW(ans), indx, g);
			break;
		case R_INTEGER:
			PROTECT(ans = Rf_allocVector(INTSXP, len));
			xa.get_elements<int>(INTEGER(ans), indx, g);
			break;
		case R_DOUBLE:
			PROTECT(ans = Rf_allocVector(REALSXP, len));
			xa.get_elements<double>(REAL(ans), indx, g);
			break;
		default:
			xa.self_destruct();
			Rf_error("data type must be raw, integer, or double");
	}
	UNPROTECT(1);
	return ans;
}

SEXP writeAtoms(SEXP x, SEXP indx, SEXP value, SEXP grp)
{
	Atoms xa(x);
	int g = Rf_asInteger(grp);
	R_xlen_t len = XLENGTH(indx);
	if ( len != XLENGTH(value) ) {
		xa.self_destruct();
		Rf_error("length of value does not match atom extent");
	}
	switch(TYPEOF(value)) {
		case RAWSXP:
			xa.set_elements<Rbyte>(RAW(value), indx, g);
			break;
		case INTSXP:
			xa.set_elements<int>(INTEGER(value), indx, g);
			break;
		case REALSXP:
			xa.set_elements<double>(REAL(value), indx, g);
			break;
		default:
			xa.self_destruct();
			Rf_error("data type must be raw, integer, or double");
	}
	return x;
}

SEXP subsetAtoms(SEXP x, SEXP indx)
{
	Atoms xa(x);
	R_xlen_t size = XLENGTH(indx);
	switch(TYPEOF(indx)) {
		case INTSXP:
			return xa.subset_index<int>(INTEGER(indx), size, true);
		case REALSXP:
			return xa.subset_index<double>(REAL(indx), size, true);
		default:
			xa.self_destruct();
			Rf_error("invalid index type");
	}
}

SEXP regroupAtoms(SEXP x, SEXP n)
{
	Atoms xa(x);
	return xa.regroup_index(Rf_asInteger(n));
}

SEXP ungroupAtoms(SEXP x)
{
	Atoms xa(x);
	return xa.ungroup_index();
}

// Matter data structures
//-----------------------

SEXP getMatterArray(SEXP x, SEXP i)
{
	MatterArray xm(x);
	return xm.get_elements(i);
}

SEXP setMatterArray(SEXP x, SEXP i, SEXP value)
{
	MatterArray xm(x);
	xm.set_elements(i, value);
	return x;
}

SEXP getMatterMatrix(SEXP x, SEXP i, SEXP j)
{
	MatterMatrix xm(x);
	return xm.get_submatrix(i, j);
}

SEXP setMatterMatrix(SEXP x, SEXP i, SEXP j, SEXP value)
{
	MatterMatrix xm(x);
	xm.set_submatrix(i, j, value);
	return x;
}

SEXP getMatterListElt(SEXP x, SEXP i, SEXP j)
{
	MatterList xm(x);
	return xm.get(Rf_asInteger(i) - 1, j);
}

SEXP setMatterListElt(SEXP x, SEXP i, SEXP j, SEXP value)
{
	MatterList xm(x);
	xm.set(Rf_asInteger(i) - 1, j, value);
	return x;
}

SEXP getMatterListSubset(SEXP x, SEXP i, SEXP j)
{
	MatterList xm(x);
	return xm.get_elements(i, j);
}

SEXP setMatterListSubset(SEXP x, SEXP i, SEXP j, SEXP value)
{
	MatterList xm(x);
	xm.set_elements(i, j, value);
	return x;
}

SEXP getMatterStrings(SEXP x, SEXP i, SEXP j)
{
	MatterStringList xm(x);
	return xm.get_strings(i, j);
}

SEXP setMatterStrings(SEXP x, SEXP i, SEXP j, SEXP value)
{
	MatterStringList xm(x);
	xm.set_strings(i, j, value);
	return x;
}

// Sparse data structures
//-----------------------

SEXP getSparseArray(SEXP x, SEXP i)
{
	SparseArray xm(x);
	return xm.get_elements(i);
}

SEXP getSparseMatrix(SEXP x, SEXP i, SEXP j)
{
	SparseMatrix xm(x);	
	return xm.get_submatrix(i, j);
}

// Signal processing
//------------------

SEXP binUpdate(SEXP score, SEXP lower, SEXP upper)
{
	SEXP ans, new_lower, new_upper;
	if ( LENGTH(score) != LENGTH(lower) )
		Rf_error("scores and bounds must have equal length");
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(new_lower = Rf_allocVector(INTSXP, LENGTH(lower)));
	PROTECT(new_upper = Rf_allocVector(INTSXP, LENGTH(upper)));
	PROTECT(ans = Rf_allocVector(VECSXP, 2));
	bin_update(REAL(score), INTEGER(lower), INTEGER(upper),
		LENGTH(lower), INTEGER(new_lower), INTEGER(new_upper));
	SET_VECTOR_ELT(ans, 0, new_lower);
	SET_VECTOR_ELT(ans, 1, new_upper);
	UNPROTECT(3);
	return ans;
}

SEXP binVector(SEXP x, SEXP lower, SEXP upper, SEXP stat)
{
	SEXP ans;
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(REALSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			bin_vector(INTEGER(x), LENGTH(x), INTEGER(lower), INTEGER(upper),
				LENGTH(lower), REAL(ans), Rf_asInteger(stat));
			break;
		case REALSXP:
			bin_vector(REAL(x), LENGTH(x), INTEGER(lower), INTEGER(upper),
				LENGTH(lower), REAL(ans), Rf_asInteger(stat));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP sampleLTOB(SEXP x, SEXP t, SEXP lower, SEXP upper)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(t) )
		Rf_error("x and t must have equal length");
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(INTSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			sample_ltob(INTEGER(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		case REALSXP:
			sample_ltob(REAL(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP sampleLTTB(SEXP x, SEXP t, SEXP lower, SEXP upper)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(t) )
		Rf_error("x and t must have equal length");
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(INTSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			sample_lttb(INTEGER(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		case REALSXP:
			sample_lttb(REAL(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP convexHull(SEXP x, SEXP y, SEXP upper)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(y) )
		Rf_error("x and y must have equal length");
	PROTECT(ans = Rf_allocVector(INTSXP, LENGTH(x)));
	size_t h;
	switch(TYPEOF(x)) {
		case INTSXP:
			h = convex_hull(INTEGER(x), INTEGER(y), LENGTH(x),
				INTEGER(ans), Rf_asLogical(upper));
			break;
		case REALSXP:
			h = convex_hull(REAL(x), REAL(y), LENGTH(x),
				INTEGER(ans), Rf_asLogical(upper));
			break;
		default:
			Rf_error("unsupported data type");
	}
	PROTECT(ans = extract_region(ans, 0, h));
	UNPROTECT(2);
	return ans;
}

SEXP smoothSNIP(SEXP x, SEXP m, SEXP decreasing)
{
	SEXP ans;
	PROTECT(ans = Rf_allocVector(TYPEOF(x), LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			smooth_snip(INTEGER(x), LENGTH(x), INTEGER(ans),
				Rf_asInteger(m), Rf_asLogical(decreasing));
			break;
		case REALSXP:
			smooth_snip(REAL(x), LENGTH(x), REAL(ans),
				Rf_asInteger(m), Rf_asLogical(decreasing));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP localMaxima(SEXP x, SEXP width)
{
	SEXP ans;
	PROTECT(ans = Rf_allocVector(LGLSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			local_maxima(INTEGER(x), LENGTH(x),
				LOGICAL(ans), Rf_asInteger(width));
			break;
		case REALSXP:
			local_maxima(REAL(x), LENGTH(x),
				LOGICAL(ans), Rf_asInteger(width));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP peakBoundaries(SEXP x, SEXP peaks)
{
	SEXP ans, left_bounds, right_bounds;
	PROTECT(left_bounds = Rf_allocVector(INTSXP, LENGTH(peaks)));
	PROTECT(right_bounds = Rf_allocVector(INTSXP, LENGTH(peaks)));
	PROTECT(ans = Rf_allocVector(VECSXP, 2));
	switch(TYPEOF(x)) {
		case INTSXP:
			peak_boundaries(INTEGER(x), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_bounds), INTEGER(right_bounds));
			break;
		case REALSXP:
			peak_boundaries(REAL(x), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_bounds), INTEGER(right_bounds));
			break;
		default:
			Rf_error("unsupported data type");
	}
	SET_VECTOR_ELT(ans, 0, left_bounds);
	SET_VECTOR_ELT(ans, 1, right_bounds);
	UNPROTECT(3);
	return ans;
}

SEXP peakBases(SEXP x, SEXP peaks)
{
	SEXP ans, left_bases, right_bases;
	PROTECT(left_bases = Rf_allocVector(INTSXP, LENGTH(peaks)));
	PROTECT(right_bases = Rf_allocVector(INTSXP, LENGTH(peaks)));
	PROTECT(ans = Rf_allocVector(VECSXP, 2));
	switch(TYPEOF(x)) {
		case INTSXP:
			peak_bases(INTEGER(x), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_bases), INTEGER(right_bases));
			break;
		case REALSXP:
			peak_bases(REAL(x), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_bases), INTEGER(right_bases));
			break;
		default:
			Rf_error("unsupported data type");
	}
	SET_VECTOR_ELT(ans, 0, left_bases);
	SET_VECTOR_ELT(ans, 1, right_bases);
	UNPROTECT(3);
	return ans;
}

SEXP peakWidths(SEXP x, SEXP peaks, SEXP domain,
	SEXP left_limits, SEXP right_limits, SEXP heights)
{
	SEXP ans, left_ips, right_ips;
	if ( LENGTH(x) != LENGTH(domain) )
		Rf_error("signal and domain must have equal length");
	PROTECT(left_ips = Rf_allocVector(REALSXP, LENGTH(peaks)));
	PROTECT(right_ips = Rf_allocVector(REALSXP, LENGTH(peaks)));
	PROTECT(ans = Rf_allocVector(VECSXP, 2));
	switch(TYPEOF(x)) {
		case INTSXP:
			peak_widths(INTEGER(x), REAL(domain), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_limits), INTEGER(right_limits), REAL(heights),
				REAL(left_ips), REAL(right_ips));
			break;
		case REALSXP:
			peak_widths(REAL(x), REAL(domain), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_limits), INTEGER(right_limits), REAL(heights),
				REAL(left_ips), REAL(right_ips));
			break;
		default:
			Rf_error("unsupported data type");
	}
	SET_VECTOR_ELT(ans, 0, left_ips);
	SET_VECTOR_ELT(ans, 1, right_ips);
	UNPROTECT(3);
	return ans;
}

SEXP peakAreas(SEXP x, SEXP peaks, SEXP domain,
	SEXP left_limits, SEXP right_limits)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(domain) )
		Rf_error("signal and domain must have equal length");
	PROTECT(ans = Rf_allocVector(REALSXP, LENGTH(peaks)));
	switch(TYPEOF(x)) {
		case INTSXP:
			peak_areas(INTEGER(x), REAL(domain), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_limits), INTEGER(right_limits),
				REAL(ans));
			break;
		case REALSXP:
			peak_areas(REAL(x), REAL(domain), LENGTH(x),
				INTEGER(peaks), LENGTH(peaks),
				INTEGER(left_limits), INTEGER(right_limits),
				REAL(ans));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

} // extern "C"
