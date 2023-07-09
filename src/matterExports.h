
#ifndef MATTER_EXPORTS
#define MATTER_EXPORTS

#include "drle.h"
#include "atoms.h"
#include "matter.h"
#include "search.h"
#include "signal.h"
#include "signal2.h"
#include "sparse.h"

extern "C" {

// Search and selection
//----------------------
SEXP relativeDiff(SEXP x, SEXP y, SEXP ref);
SEXP quickOrder(SEXP x);
SEXP quickRank(SEXP x, SEXP ties_max);
SEXP quickSelect(SEXP x, SEXP k);
SEXP quickMedian(SEXP x);
SEXP quickMAD(SEXP x, SEXP center, SEXP constant);
SEXP binarySearch(SEXP x, SEXP table,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP nearest);
SEXP kdTree(SEXP x);
SEXP kdSearch(SEXP x, SEXP data, SEXP left_child, SEXP right_child,
	SEXP root, SEXP tol, SEXP tol_ref);

// Compression (delta run length encoding)
//-----------------------------------------

SEXP encodeDRLE(SEXP x, SEXP cr);
SEXP recodeDRLE(SEXP x, SEXP i);
SEXP decodeDRLE(SEXP x, SEXP i);

// Internal testing for Atoms
//---------------------------

SEXP readAtom(SEXP x, SEXP i, SEXP type);
SEXP writeAtom(SEXP x, SEXP i, SEXP value);
SEXP readAtoms(SEXP x, SEXP indx, SEXP type, SEXP grp);
SEXP writeAtoms(SEXP x, SEXP indx, SEXP value, SEXP grp);
SEXP subsetAtoms(SEXP x, SEXP indx);
SEXP regroupAtoms(SEXP x, SEXP n);
SEXP ungroupAtoms(SEXP x);

// Matter data structures
//-----------------------

SEXP getMatterArray(SEXP x, SEXP i);
SEXP setMatterArray(SEXP x, SEXP i, SEXP value);
SEXP getMatterMatrix(SEXP x, SEXP i, SEXP j);
SEXP setMatterMatrix(SEXP x, SEXP i, SEXP j, SEXP value);
SEXP getMatterListElt(SEXP x, SEXP i, SEXP j);
SEXP setMatterListElt(SEXP x, SEXP i, SEXP j, SEXP value);
SEXP getMatterListSubset(SEXP x, SEXP i, SEXP j);
SEXP setMatterListSubset(SEXP x, SEXP i, SEXP j, SEXP value);
SEXP getMatterStrings(SEXP x, SEXP i, SEXP j);
SEXP setMatterStrings(SEXP x, SEXP i, SEXP j, SEXP value);

// Sparse data structures
//-----------------------

SEXP getSparseArray(SEXP x, SEXP i);
SEXP getSparseMatrix(SEXP x, SEXP i, SEXP j);

// 1D Signal processing
//----------------------

SEXP meanFilter(SEXP x, SEXP width);
SEXP linearFilter(SEXP x, SEXP weights);
SEXP bilateralFilter(SEXP x, SEXP width,
	SEXP sddist, SEXP sdrange, SEXP spar);
SEXP diffusionFilter(SEXP x, SEXP niter,
	SEXP kappa, SEXP rate, SEXP method);
SEXP guidedFilter(SEXP x, SEXP g, SEXP width,
	SEXP sdreg, SEXP ftol);
SEXP warpDTW(SEXP x, SEXP y, SEXP tx, SEXP ty,
	SEXP tol, SEXP tol_ref);
SEXP warpCOW(SEXP x, SEXP y, SEXP tx, SEXP ty,
	SEXP x_nodes, SEXP y_nodes, SEXP tol, SEXP tol_ref);
SEXP iCorr(SEXP x, SEXP y);
SEXP binVector(SEXP x, SEXP lower, SEXP upper, SEXP stat, SEXP prob);
SEXP binUpdate(SEXP score, SEXP lower, SEXP upper);
SEXP downsampleLTOB(SEXP x, SEXP t, SEXP lower, SEXP upper);
SEXP downsampleLTTB(SEXP x, SEXP t, SEXP lower, SEXP upper);
SEXP convexHull(SEXP x, SEXP y, SEXP upper);
SEXP smoothSNIP(SEXP x, SEXP m, SEXP decreasing);
SEXP localMaxima(SEXP x, SEXP width);
SEXP peakBoundaries(SEXP x, SEXP peaks);
SEXP peakBases(SEXP x, SEXP peaks);
SEXP peakWidths(SEXP x, SEXP peaks, SEXP domain,
	 SEXP left_limits, SEXP right_limits, SEXP heights);
SEXP peakAreas(SEXP x, SEXP peaks, SEXP domain,
	 SEXP left_limits, SEXP right_limits);
SEXP Approx1(SEXP xi, SEXP x, SEXP y,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP interp);

// 2D Signal processing
//----------------------

SEXP meanFilter2(SEXP x, SEXP width);
SEXP linearFilter2(SEXP x, SEXP width);
SEXP bilateralFilter2(SEXP x, SEXP width,
	SEXP sddist, SEXP sdrange, SEXP spar);
SEXP diffusionFilter2(SEXP x, SEXP niter,
	SEXP kappa, SEXP rate, SEXP method);
SEXP guidedFilter2(SEXP x, SEXP g, SEXP width,
	SEXP sdreg);
SEXP Approx2(SEXP xi, SEXP yi, SEXP xy, SEXP z,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP interp);

} // extern "C"

#endif // MATTER_EXPORTS
