#include <R_ext/Rdynload.h>

#include "matterExports.h"
#include "altrep.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

extern "C" {

static const R_CallMethodDef callMethods[] = {
	// search and select
	CALLDEF(relativeDiff, 3),
	CALLDEF(quickOrder, 1),
	CALLDEF(quickRank, 2),
	CALLDEF(quickSelect, 2),
	CALLDEF(quickMedian, 1),
	CALLDEF(quickMAD, 3),
	CALLDEF(binarySearch, 6),
	CALLDEF(kdTree, 1),
	CALLDEF(kdSearch, 7),
	CALLDEF(knnSearch, 8),
	// compression
	CALLDEF(encodeDRLE, 2),
	CALLDEF(decodeDRLE, 2),
	CALLDEF(recodeDRLE, 2),
	// matter i/o
	CALLDEF(readAtom, 3),
	CALLDEF(writeAtom, 3),
	CALLDEF(readAtoms, 4),
	CALLDEF(writeAtoms, 4),
	CALLDEF(subsetAtoms, 2),
	CALLDEF(regroupAtoms, 2),
	CALLDEF(ungroupAtoms, 1),
	// matter data structures
	CALLDEF(getMatterArray, 2),
	CALLDEF(setMatterArray, 3),
	CALLDEF(getMatterMatrix, 3),
	CALLDEF(setMatterMatrix, 4),
	CALLDEF(getMatterListElt, 3),
	CALLDEF(setMatterListElt, 4),
	CALLDEF(getMatterListSubset, 3),
	CALLDEF(setMatterListSubset, 4),
	CALLDEF(getMatterStrings, 3),
	CALLDEF(setMatterStrings, 4),
	// sparse data structures
	CALLDEF(getSparseArray, 2),
	CALLDEF(getSparseMatrix, 3),
	// 1d signal processing
	CALLDEF(meanFilter, 2),
	CALLDEF(linearFilter, 2),
	CALLDEF(bilateralFilter, 5),
	CALLDEF(diffusionFilter, 5),
	CALLDEF(guidedFilter, 5),
	CALLDEF(warpDTW, 6),
	CALLDEF(warpCOW, 8),
	CALLDEF(iCorr, 2),
	CALLDEF(binVector, 5),
	CALLDEF(binUpdate, 3),
	CALLDEF(downsampleLTOB, 4),
	CALLDEF(downsampleLTTB, 4),
	CALLDEF(convexHull, 3),
	CALLDEF(smoothSNIP, 3),
	CALLDEF(localMaxima, 2),
	CALLDEF(peakBoundaries, 2),
	CALLDEF(peakBases, 2),
	CALLDEF(peakWidths, 6),
	CALLDEF(peakAreas, 5),
	CALLDEF(Approx1, 7),
	// 2d signal processing
	CALLDEF(meanFilter2, 2),
	CALLDEF(linearFilter2, 2),
	CALLDEF(bilateralFilter2, 5),
	CALLDEF(diffusionFilter2, 5),
	CALLDEF(guidedFilter2, 4),
	CALLDEF(histEq, 2),
	CALLDEF(adaptHisteq, 4),
	CALLDEF(Approx2, 8),
	// spatial
	CALLDEF(inPoly, 2),
	CALLDEF(rowDist, 4),
	CALLDEF(colDist, 4),
	CALLDEF(rowDistAt, 5),
	CALLDEF(colDistAt, 5),
	// matter altrep
	CALLDEF(newMatterAltrep, 6),
	{NULL, NULL, 0}
};

void R_init_matter(DllInfo * info)
{
	init_matter_altraw(info);
	init_matter_altlogical(info);
	init_matter_altinteger(info);
	init_matter_altreal(info);
	init_matter_altstring(info);
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

} // extern "C"
