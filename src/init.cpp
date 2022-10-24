#include <R_ext/Rdynload.h>

#include "matterExports.h"
#include "altrep.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

extern "C" {

static const R_CallMethodDef callMethods[] = {
	// search
	CALLDEF(relativeDiff, 3),
	CALLDEF(binarySearch, 6),
	CALLDEF(approxSearch, 8),
	// compression
	CALLDEF(encodeDRLE, 2),
	CALLDEF(decodeDRLE, 2),
	CALLDEF(recodeDRLE, 2),
	// internal
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
	// signal processing
	CALLDEF(binVector, 4),
	CALLDEF(localMaxima, 2),
	CALLDEF(peakBoundaries, 3),
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
