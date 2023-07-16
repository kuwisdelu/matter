
#include "matterExports.h"

extern "C" {

// Search, sort and select
//------------------------

SEXP relativeDiff(SEXP x, SEXP y, SEXP ref)
{
	if ( TYPEOF(x) != TYPEOF(y) )
		Rf_error("'x' and 'y' must have the same type");
	return Rf_ScalarReal(sdiff(x, y, Rf_asInteger(ref)));
}

SEXP quickOrder(SEXP x)
{
	SEXP indx;
	PROTECT(indx = Rf_allocVector(INTSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_quick_sort(INTEGER(indx), INTEGER(x), 0, XLENGTH(x), true);
			break;
		case REALSXP:
			do_quick_sort(INTEGER(indx), REAL(x), 0, XLENGTH(x), true);
			break;
		case STRSXP:
			do_quick_sort(INTEGER(indx), STRING_PTR(x), 0, XLENGTH(x), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return indx;
}

SEXP quickRank(SEXP x, SEXP ties_max)
{
	SEXP rank;
	PROTECT(rank = Rf_allocVector(INTSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_quick_rank(INTEGER(rank), INTEGER(x), 0, XLENGTH(x),
				Rf_asLogical(ties_max));
			break;
		case REALSXP:
			do_quick_rank(INTEGER(rank), REAL(x), 0, XLENGTH(x),
				Rf_asLogical(ties_max));
			break;
		case STRSXP:
			do_quick_rank(INTEGER(rank), STRING_PTR(x), 0, XLENGTH(x),
				Rf_asLogical(ties_max));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return rank;
}

SEXP quickSelect(SEXP x, SEXP k)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(TYPEOF(x), LENGTH(k)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_quick_select(INTEGER(result), INTEGER(x), 0, XLENGTH(x),
				INTEGER(k), LENGTH(k));
			break;
		case REALSXP:
			do_quick_select(REAL(result), REAL(x), 0, XLENGTH(x),
				INTEGER(k), LENGTH(k));
			break;
		case STRSXP:
			do_quick_select(STRING_PTR(result), STRING_PTR(x), 0, XLENGTH(x),
				INTEGER(k), LENGTH(k));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP quickMedian(SEXP x)
{
	switch(TYPEOF(x)) {
		case INTSXP:
			return Rf_ScalarReal(quick_median(INTEGER(x), XLENGTH(x)));
		case REALSXP:
			return Rf_ScalarReal(quick_median(REAL(x), XLENGTH(x)));
		default:
			Rf_error("unsupported data type");
	}
}

SEXP quickMAD(SEXP x, SEXP center, SEXP constant)
{
	switch(TYPEOF(x)) {
		case INTSXP:
			return Rf_ScalarReal(quick_mad(INTEGER(x), XLENGTH(x),
				Rf_asReal(center), Rf_asReal(constant)));
		case REALSXP:
			return Rf_ScalarReal(quick_mad(REAL(x), XLENGTH(x),
				Rf_asReal(center), Rf_asReal(constant)));
		default:
			Rf_error("unsupported data type");
	}
}

SEXP binarySearch(SEXP x, SEXP table, SEXP tol,
	SEXP tol_ref, SEXP nomatch, SEXP nearest)
{
	SEXP pos;
	PROTECT(pos = Rf_allocVector(INTSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_binary_search(INTEGER(pos), INTEGER(x), LENGTH(x), INTEGER(table),
				0, LENGTH(table), Rf_asReal(tol), Rf_asInteger(tol_ref),
				Rf_asInteger(nomatch), Rf_asLogical(nearest), true);
			break;
		case REALSXP:
			do_binary_search(INTEGER(pos), REAL(x), LENGTH(x), REAL(table),
				0, LENGTH(table), Rf_asReal(tol), Rf_asInteger(tol_ref),
				Rf_asInteger(nomatch), Rf_asLogical(nearest), true);
			break;
		case STRSXP:
			do_binary_search(INTEGER(pos), STRING_PTR(x), LENGTH(x), STRING_PTR(table),
				0, LENGTH(table), Rf_asReal(tol), Rf_asInteger(tol_ref),
				Rf_asInteger(nomatch), Rf_asLogical(nearest), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return pos;
}

SEXP kdTree(SEXP x)
{
	size_t root;
	size_t n = Rf_nrows(x);
	size_t k = Rf_ncols(x);
	SEXP result, left_child, right_child;
	PROTECT(left_child = Rf_allocVector(INTSXP, n));
	PROTECT(right_child = Rf_allocVector(INTSXP, n));
	switch(TYPEOF(x)) {
		case INTSXP:
			root = kd_tree_build(INTEGER(x), k, n,
				INTEGER(left_child), INTEGER(right_child));
			break;
		case REALSXP:
			root = kd_tree_build(REAL(x), k, n,
				INTEGER(left_child), INTEGER(right_child));
			break;
		default:
			Rf_error("unsupported data type");
	}
	PROTECT(result = Rf_allocVector(VECSXP, 3));
	SET_VECTOR_ELT(result, 0, left_child);
	SET_VECTOR_ELT(result, 1, right_child);
	SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(root));
	UNPROTECT(3);
	return result;
}

SEXP kdSearch(SEXP x, SEXP data, SEXP left_child, SEXP right_child,
	SEXP root, SEXP tol, SEXP tol_ref)
{
	size_t k = Rf_ncols(data);
	size_t ndata = Rf_nrows(data);
	size_t nx = LENGTH(x) / k;
	SEXP result, buffer, neighbors;
	PROTECT(result = Rf_allocVector(VECSXP, nx));
	PROTECT(buffer = Rf_allocVector(INTSXP, ndata));
	for ( index_t i = 0; i < nx; i++ )
	{
		size_t nnb;
		switch(TYPEOF(x)) {
			case INTSXP:
			{
				int xi [k];
				for ( index_t j = 0; j < k; j++ )
					xi[j] = INTEGER_ELT(x, j * nx + i);
				nnb = kd_tree_search(INTEGER(buffer), xi, INTEGER(data), k, ndata,
					INTEGER(left_child), INTEGER(right_child), Rf_asInteger(root),
					REAL(tol), Rf_asInteger(tol_ref), true);
				break;
			}
			case REALSXP:
			{
				double xi [k];
				for ( index_t j = 0; j < k; j++ )
					xi[j] = REAL_ELT(x, j * nx + i);
				nnb = kd_tree_search(INTEGER(buffer), xi, REAL(data), k, ndata,
					INTEGER(left_child), INTEGER(right_child), Rf_asInteger(root),
					REAL(tol), Rf_asInteger(tol_ref), true);
				break;
			}
			default:
				Rf_error("unsupported data type");
		}
		PROTECT(neighbors = Rf_allocVector(INTSXP, nnb));
		for ( index_t j = 0; j < nnb; j++ )
			INTEGER(neighbors)[j] = INTEGER_ELT(buffer, j);
		SET_VECTOR_ELT(result, i, neighbors);
		UNPROTECT(1);
	}
	UNPROTECT(2);
	return result;
}

SEXP knnSearch(SEXP x, SEXP data, SEXP left_child, SEXP right_child,
	SEXP root, SEXP knn, SEXP metric, SEXP p)
{
	size_t k = Rf_ncols(data);
	size_t ndata = Rf_nrows(data);
	size_t nx = LENGTH(x) / k;
	SEXP result;
	PROTECT(result = Rf_allocMatrix(INTSXP, nx, Rf_asInteger(knn)));
	switch(TYPEOF(x)) {
		case INTSXP:
			do_knn_search(INTEGER(result), INTEGER(x), INTEGER(data), k, nx, ndata,
				INTEGER(left_child), INTEGER(right_child), Rf_asInteger(root),
				Rf_asInteger(knn), Rf_asInteger(metric), Rf_asReal(p), true);
			break;
		case REALSXP:
			do_knn_search(INTEGER(result), REAL(x), REAL(data), k, nx, ndata,
				INTEGER(left_child), INTEGER(right_child), Rf_asInteger(root),
				Rf_asInteger(knn), Rf_asInteger(metric), Rf_asReal(p), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

// Distance
//----------

SEXP rowDist(SEXP x, SEXP y, SEXP metric, SEXP p)
{
	if ( TYPEOF(x) != TYPEOF(y) )
		Rf_error("'x' and 'y' must have the same type");
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_nrows(y)));
	switch(TYPEOF(x)) {
		case INTSXP:
			row_dist(INTEGER(x), INTEGER(y), Rf_nrows(x), Rf_nrows(y), Rf_ncols(x),
				REAL(result), Rf_asInteger(metric), Rf_asReal(p));
			break;
		case REALSXP:
			row_dist(REAL(x), REAL(y), Rf_nrows(x), Rf_nrows(y), Rf_ncols(x),
				REAL(result), Rf_asInteger(metric), Rf_asReal(p));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP colDist(SEXP x, SEXP y, SEXP metric, SEXP p)
{
	if ( TYPEOF(x) != TYPEOF(y) )
		Rf_error("'x' and 'y' must have the same type");
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_ncols(x), Rf_ncols(y)));
	switch(TYPEOF(x)) {
		case INTSXP:
			col_dist(INTEGER(x), INTEGER(y), Rf_ncols(x), Rf_ncols(y), Rf_nrows(x),
				REAL(result), Rf_asInteger(metric), Rf_asReal(p));
			break;
		case REALSXP:
			col_dist(REAL(x), REAL(y), Rf_ncols(x), Rf_ncols(y), Rf_nrows(x),
				REAL(result), Rf_asInteger(metric), Rf_asReal(p));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP rowDistAt(SEXP x, SEXP y, SEXP xat, SEXP yat, SEXP metric, SEXP p)
{
	size_t nout = LENGTH(xat);
	SEXP result;
	PROTECT(result = Rf_allocVector(VECSXP, nout));
	for ( index_t i = 0; i < nout; i++ )
	{
		SEXP indx = VECTOR_ELT(xat, i);
		SEXP indy = VECTOR_ELT(yat, i);
		int ni = LENGTH(indx);
		int xrows [ni];
		int yrows [ni];
		for ( index_t j = 0; j < ni; j++ )
		{
			xrows[j] = INTEGER_ELT(indx, j) - 1;
			yrows[j] = INTEGER_ELT(indy, j) - 1;
		}
		SEXP dist;
		PROTECT(dist = Rf_allocVector(REALSXP, ni));
		switch(TYPEOF(x)) {
			case INTSXP:
				row_dist_at(INTEGER(x), INTEGER(y), xrows, yrows, Rf_nrows(x), Rf_nrows(y),
					ni, Rf_ncols(x), REAL(dist), Rf_asInteger(metric), Rf_asReal(p));
				break;
			case REALSXP:
				row_dist_at(REAL(x), REAL(y), xrows, yrows, Rf_nrows(x), Rf_nrows(y),
					ni, Rf_ncols(x), REAL(dist), Rf_asInteger(metric), Rf_asReal(p));
				break;
			default:
				Rf_error("unsupported data type");
		}
		SET_VECTOR_ELT(result, i, dist);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return result;
}

SEXP colDistAt(SEXP x, SEXP y, SEXP xat, SEXP yat, SEXP metric, SEXP p)
{
	size_t nout = LENGTH(xat);
	SEXP result;
	PROTECT(result = Rf_allocVector(VECSXP, nout));
	for ( index_t i = 0; i < nout; i++ )
	{
		SEXP indx = VECTOR_ELT(xat, i);
		SEXP indy = VECTOR_ELT(yat, i);
		int ni = LENGTH(indx);
		int xcols [ni];
		int ycols [ni];
		for ( index_t j = 0; j < ni; j++ )
		{
			xcols[j] = INTEGER_ELT(indx, j) - 1;
			ycols[j] = INTEGER_ELT(indy, j) - 1;
		}
		SEXP dist;
		PROTECT(dist = Rf_allocVector(REALSXP, ni));
		switch(TYPEOF(x)) {
			case INTSXP:
				col_dist_at(INTEGER(x), INTEGER(y), xcols, ycols, Rf_ncols(x), Rf_ncols(y),
					ni, Rf_nrows(x), REAL(dist), Rf_asInteger(metric), Rf_asReal(p));
				break;
			case REALSXP:
				col_dist_at(REAL(x), REAL(y), xcols, ycols, Rf_ncols(x), Rf_ncols(y),
					ni, Rf_nrows(x), REAL(dist), Rf_asInteger(metric), Rf_asReal(p));
				break;
			default:
				Rf_error("unsupported data type");
		}
		SET_VECTOR_ELT(result, i, dist);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return result;
}

SEXP inPoly(SEXP points, SEXP vertices)
{
	if ( TYPEOF(points) != TYPEOF(vertices) )
		Rf_error("'points' and 'vertices' must have the same type");
	SEXP result;
	PROTECT(result = Rf_allocVector(LGLSXP, Rf_nrows(points)));
	switch(TYPEOF(points)) {
		case INTSXP:
			do_in_poly(LOGICAL(result), INTEGER(points), Rf_nrows(points),
				INTEGER(vertices), Rf_nrows(vertices));
			break;
		case REALSXP:
			do_in_poly(LOGICAL(result), REAL(points), Rf_nrows(points),
				REAL(vertices), Rf_nrows(vertices));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
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

// 1D Signal processing
//----------------------

SEXP meanFilter(SEXP x, SEXP width)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			mean_filter(INTEGER(x), LENGTH(x),
				Rf_asInteger(width), REAL(result));
			break;
		case REALSXP:
			mean_filter(REAL(x), LENGTH(x),
				Rf_asInteger(width), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP linearFilter(SEXP x, SEXP weights)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			linear_filter(INTEGER(x), LENGTH(x), REAL(weights),
				LENGTH(weights), REAL(result));
			break;
		case REALSXP:
			linear_filter(REAL(x), LENGTH(x), REAL(weights),
				LENGTH(weights), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP bilateralFilter(SEXP x, SEXP width,
	SEXP sddist, SEXP sdrange, SEXP spar)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			bilateral_filter(INTEGER(x), LENGTH(x), Rf_asInteger(width),
				Rf_asReal(sddist), Rf_asReal(sdrange),
				Rf_asReal(spar), REAL(result));
			break;
		case REALSXP:
			bilateral_filter(REAL(x), LENGTH(x), Rf_asInteger(width),
				Rf_asReal(sddist), Rf_asReal(sdrange),
				Rf_asReal(spar), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP diffusionFilter(SEXP x, SEXP niter,
	SEXP kappa, SEXP rate, SEXP method)
{
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			diffusion_filter(INTEGER(x), LENGTH(x), Rf_asInteger(niter),
				Rf_asReal(kappa), Rf_asReal(rate), Rf_asInteger(method),
				REAL(result));
			break;
		case REALSXP:
			diffusion_filter(REAL(x), LENGTH(x), Rf_asInteger(niter),
				Rf_asReal(kappa), Rf_asReal(rate), Rf_asInteger(method),
				REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP guidedFilter(SEXP x, SEXP g, SEXP width,
	SEXP sdreg, SEXP ftol)
{
	SEXP result;
	if ( XLENGTH(x) != XLENGTH(g) )
		Rf_error("signal and guide must be the same length");
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			guided_filter(INTEGER(x), INTEGER(g), LENGTH(x), Rf_asInteger(width),
				Rf_asReal(sdreg), Rf_asReal(ftol), REAL(result));
			break;
		case REALSXP:
			guided_filter(REAL(x), REAL(g), LENGTH(x), Rf_asInteger(width),
				Rf_asReal(sdreg), Rf_asReal(ftol), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP iCorr(SEXP x, SEXP y)
{
	switch(TYPEOF(x)) {
		case INTSXP:
			return Rf_ScalarReal(icor(INTEGER(x), INTEGER(y),
				XLENGTH(x), XLENGTH(y)));
			break;
		case REALSXP:
			return Rf_ScalarReal(icor(REAL(x), REAL(y),
				XLENGTH(x), XLENGTH(y)));
			break;
		default:
			Rf_error("unsupported data type");
	}
}

SEXP warpDTW(SEXP x, SEXP y, SEXP tx, SEXP ty,
	SEXP tol, SEXP tol_ref)
{
	SEXP result;
	size_t n = LENGTH(x) + LENGTH(y) - 1;
	PROTECT(result = Rf_allocMatrix(INTSXP, n, 2));
	switch(TYPEOF(x)) {
		case INTSXP: {
				switch(TYPEOF(tx)) {
					case INTSXP:
						warp_dtwc(INTEGER(x), INTEGER(y), INTEGER(tx), INTEGER(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
					case REALSXP:
						warp_dtwc(INTEGER(x), INTEGER(y), REAL(tx), REAL(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
				}
			}
			break;
		case REALSXP: {
				switch(TYPEOF(tx)) {
					case INTSXP:
						warp_dtwc(REAL(x), REAL(y), INTEGER(tx), INTEGER(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
					case REALSXP:
						warp_dtwc(REAL(x), REAL(y), REAL(tx), REAL(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
				}
			}
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP warpCOW(SEXP x, SEXP y, SEXP tx, SEXP ty,
	SEXP x_nodes, SEXP y_nodes, SEXP tol, SEXP tol_ref)
{
	SEXP result;
	size_t n = LENGTH(x_nodes);
	PROTECT(result = Rf_allocMatrix(INTSXP, n, 2));
	for ( index_t i = 0; i < n; i++ )
	{
		INTEGER(result)[i] = INTEGER_ELT(x_nodes, i);
		INTEGER(result)[n + i] = INTEGER_ELT(y_nodes, i);
	}
	switch(TYPEOF(x)) {
		case INTSXP: {
				switch(TYPEOF(tx)) {
					case INTSXP:
						warp_cow(INTEGER(x), INTEGER(y), INTEGER(tx), INTEGER(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
					case REALSXP:
						warp_cow(INTEGER(x), INTEGER(y), REAL(tx), REAL(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, n , Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
				}
			}
			break;
		case REALSXP: {
				switch(TYPEOF(tx)) {
					case INTSXP:
						warp_cow(REAL(x), REAL(y), INTEGER(tx), INTEGER(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
					case REALSXP:
						warp_cow(REAL(x), REAL(y), REAL(tx), REAL(ty), LENGTH(x), LENGTH(y),
							INTEGER(result), INTEGER(result) + n, n, Rf_asReal(tol), Rf_asInteger(tol_ref));
						break;
				}
			}
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP binVector(SEXP x, SEXP lower, SEXP upper, SEXP stat, SEXP prob)
{
	SEXP ans;
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(REALSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			bin_vector(INTEGER(x), LENGTH(x), INTEGER(lower), INTEGER(upper),
				LENGTH(lower), REAL(ans), Rf_asInteger(stat), Rf_asReal(prob));
			break;
		case REALSXP:
			bin_vector(REAL(x), LENGTH(x), INTEGER(lower), INTEGER(upper),
				LENGTH(lower), REAL(ans), Rf_asInteger(stat), Rf_asReal(prob));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

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

SEXP downsampleLTOB(SEXP x, SEXP t, SEXP lower, SEXP upper)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(t) )
		Rf_error("x and t must have equal length");
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(INTSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			downsample_ltob(INTEGER(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		case REALSXP:
			downsample_ltob(REAL(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return ans;
}

SEXP downsampleLTTB(SEXP x, SEXP t, SEXP lower, SEXP upper)
{
	SEXP ans;
	if ( LENGTH(x) != LENGTH(t) )
		Rf_error("x and t must have equal length");
	if ( LENGTH(lower) != LENGTH(upper) )
		Rf_error("lower and upper bounds must have equal length");
	PROTECT(ans = Rf_allocVector(INTSXP, XLENGTH(lower)));
	switch(TYPEOF(x)) {
		case INTSXP:
			downsample_lttb(INTEGER(x), REAL(t), LENGTH(x),
				INTEGER(lower), INTEGER(upper), LENGTH(lower),
				INTEGER(ans), true);
			break;
		case REALSXP:
			downsample_lttb(REAL(x), REAL(t), LENGTH(x),
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

SEXP Approx1(SEXP xi, SEXP x, SEXP y,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP interp)
{
	if ( TYPEOF(xi) != TYPEOF(x) )
		Rf_error("'xi' and 'x' must have the same type");
	if ( Rf_asReal(tol) < 0 )
		Rf_error("'tol' must be non-negative");
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(xi)));
	switch(TYPEOF(y)) {
		case INTSXP:
			switch(TYPEOF(x)) {
				case INTSXP:
					do_approx1<int,int>(REAL(result), INTEGER(xi), LENGTH(xi),
						INTEGER(x), INTEGER(y), 0, LENGTH(y),
						Rf_asReal(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				case REALSXP:
					do_approx1<double,int>(REAL(result), REAL(xi), LENGTH(xi),
						REAL(x), INTEGER(y), 0, LENGTH(y),
						Rf_asReal(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				default:
					Rf_error("x has an unsupported data type");
			}
			break;
		case REALSXP:
			switch(TYPEOF(x)) {
				case INTSXP:
					do_approx1<int,double>(REAL(result), INTEGER(xi), LENGTH(xi),
						INTEGER(x), REAL(y), 0, LENGTH(y),
						Rf_asReal(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				case REALSXP:
					do_approx1<double,double>(REAL(result), REAL(xi), LENGTH(xi),
						REAL(x), REAL(y), 0, LENGTH(y),
						Rf_asReal(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				default:
					Rf_error("x has an unsupported data type");
			}
			break;
		default:
			Rf_error("y has an unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

// 2D Signal processing
//----------------------

SEXP meanFilter2(SEXP x, SEXP width)
{
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			mean_filter2(INTEGER(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), REAL(result));
			break;
		case REALSXP:
			mean_filter2(REAL(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP linearFilter2(SEXP x, SEXP weights)
{
	SEXP result;
	if ( Rf_nrows(weights) != Rf_ncols(weights) )
		Rf_error("weights must be a square matrix");
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			linear_filter2(INTEGER(x), Rf_nrows(x), Rf_ncols(x),
				REAL(weights), Rf_nrows(weights), REAL(result));
			break;
		case REALSXP:
			linear_filter2(REAL(x), Rf_nrows(x), Rf_ncols(x),
				REAL(weights), Rf_nrows(weights), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP bilateralFilter2(SEXP x, SEXP width,
	SEXP sddist, SEXP sdrange, SEXP spar)
{
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			bilateral_filter2(INTEGER(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), Rf_asReal(sddist), Rf_asReal(sdrange),
				Rf_asReal(spar), REAL(result));
			break;
		case REALSXP:
			bilateral_filter2(REAL(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), Rf_asReal(sddist), Rf_asReal(sdrange),
				Rf_asReal(spar), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP diffusionFilter2(SEXP x, SEXP niter,
	SEXP kappa, SEXP rate, SEXP method)
{
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			diffusion_filter2(INTEGER(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(niter), Rf_asReal(kappa), Rf_asReal(rate),
				Rf_asInteger(method), REAL(result));
			break;
		case REALSXP:
			diffusion_filter2(REAL(x), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(niter), Rf_asReal(kappa), Rf_asReal(rate),
				Rf_asInteger(method), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP guidedFilter2(SEXP x, SEXP g, SEXP width,
	SEXP sdreg)
{
	SEXP result;
	if ( Rf_nrows(x) != Rf_nrows(g) || Rf_ncols(x) != Rf_ncols(g) )
		Rf_error("signal and guide must have the same dimensions");
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			guided_filter2(INTEGER(x), INTEGER(g), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), Rf_asReal(sdreg), REAL(result));
			break;
		case REALSXP:
			guided_filter2(REAL(x), REAL(g), Rf_nrows(x), Rf_ncols(x),
				Rf_asInteger(width), Rf_asReal(sdreg), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP histEq(SEXP x, SEXP nbins)
{
	SEXP result;
	if ( Rf_isMatrix(x) )
		PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	else
		PROTECT(result = Rf_allocVector(REALSXP, XLENGTH(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			histeq(INTEGER(x), XLENGTH(x), Rf_asInteger(nbins), REAL(result));
			break;
		case REALSXP:
			histeq(REAL(x), XLENGTH(x), Rf_asInteger(nbins), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP adaptHistEq(SEXP x, SEXP width, SEXP clip, SEXP nbins)
{
	SEXP result;
	PROTECT(result = Rf_allocMatrix(REALSXP, Rf_nrows(x), Rf_ncols(x)));
	switch(TYPEOF(x)) {
		case INTSXP:
			adapt_histeq(INTEGER(x), Rf_nrows(x), Rf_ncols(x), Rf_asInteger(width),
				Rf_asReal(clip), Rf_asInteger(nbins), REAL(result));
			break;
		case REALSXP:
			adapt_histeq(REAL(x), Rf_nrows(x), Rf_ncols(x), Rf_asInteger(width),
				Rf_asReal(clip), Rf_asInteger(nbins), REAL(result));
			break;
		default:
			Rf_error("unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

SEXP Approx2(SEXP xi, SEXP yi, SEXP xy, SEXP z,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP interp)
{
	if ( TYPEOF(xi) != TYPEOF(xy) )
		Rf_error("'xi' and 'x' must have the same type");
	if ( TYPEOF(yi) != TYPEOF(xy) )
		Rf_error("'yi' and 'x' must have the same type");
	if ( Rf_asReal(tol) < 0 )
		Rf_error("'tol' must be non-negative");
	SEXP result;
	PROTECT(result = Rf_allocVector(REALSXP, LENGTH(xi)));
	switch(TYPEOF(z)) {
		case INTSXP:
			switch(TYPEOF(xy)) {
				case INTSXP:
					do_approx2<int,int>(REAL(result),
						INTEGER(xi), INTEGER(yi), LENGTH(xi),
						INTEGER(xy), INTEGER(z), LENGTH(z),
						REAL(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				case REALSXP:
					do_approx2<double,int>(REAL(result),
						REAL(xi), REAL(yi), LENGTH(xi),
						REAL(xy), INTEGER(z), LENGTH(z),
						REAL(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				default:
					Rf_error("x/y have an unsupported data type");
			}
			break;
		case REALSXP:
			switch(TYPEOF(xy)) {
				case INTSXP:
					do_approx2<int,double>(REAL(result),
						INTEGER(xi), INTEGER(yi), LENGTH(xi),
						INTEGER(xy), REAL(z), LENGTH(z),
						REAL(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				case REALSXP:
					do_approx2<double,double>(REAL(result),
						REAL(xi), REAL(yi), LENGTH(xi),
						REAL(xy), REAL(z), LENGTH(z),
						REAL(tol), Rf_asInteger(tol_ref),
						Rf_asReal(nomatch), Rf_asInteger(interp));
					break;
				default:
					Rf_error("x/y have an unsupported data type");
			}
			break;
		default:
			Rf_error("z has an unsupported data type");
	}
	UNPROTECT(1);
	return result;
}

} // extern "C"
