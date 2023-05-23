
#ifndef MATTER_EXPORTS
#define MATTER_EXPORTS

#include "drle.h"
#include "atoms.h"
#include "matter.h"
#include "search.h"
#include "sparse.h"

extern "C" {

// Select (k-th order and median)
//--------------------------------

SEXP quickSelect(SEXP x, SEXP k);
SEXP quickMedian(SEXP x);

// Search (binary and approximate)
//--------------------------------

SEXP relativeDiff(SEXP x, SEXP y, SEXP ref);

SEXP binarySearch(SEXP x, SEXP table,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP nearest);
SEXP approxSearch(SEXP x, SEXP keys, SEXP values,
	SEXP tol, SEXP tol_ref, SEXP nomatch, SEXP interp);

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

// Signal processing
//------------------

SEXP linearFilter(SEXP x, SEXP weights);
SEXP bilateralFilter(SEXP x, SEXP width, SEXP sddist, SEXP sdrange);
SEXP binUpdate(SEXP score, SEXP lower, SEXP upper);
SEXP binVector(SEXP x, SEXP lower, SEXP upper, SEXP stat);
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

} // extern "C"

#endif // MATTER_EXPORTS
