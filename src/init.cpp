#include <R_ext/Rdynload.h>

#include "matter.h"
#include "matterExports.h"
#include "altrep.h"

extern "C" {

	static const R_CallMethodDef callMethods[] = {
		// search
		{"C_relativeDiff", (DL_FUNC) &relativeDiff, 3},
		{"C_binarySearch", (DL_FUNC) &binarySearch, 6},
		{"C_approxSearch", (DL_FUNC) &approxSearch, 8},
		// compression
		{"C_encodeDRLE", (DL_FUNC) &encodeDRLE, 2},
		{"C_decodeDRLE", (DL_FUNC) &decodeDRLE, 2},
		{"C_recodeDRLE", (DL_FUNC) &recodeDRLE, 2},
		// internal
		{"C_readAtom", (DL_FUNC) &readAtom, 3},
		{"C_writeAtom", (DL_FUNC) &writeAtom, 3},
		{"C_readAtoms", (DL_FUNC) &readAtoms, 4},
		{"C_writeAtoms", (DL_FUNC) &writeAtoms, 4},
		{"C_subsetAtoms", (DL_FUNC) &subsetAtoms, 2},
		{"C_regroupAtoms", (DL_FUNC) &regroupAtoms, 2},
		// matter data structures
		{"C_getMatterArray", (DL_FUNC) &getMatterArray, 2},
		{"C_setMatterArray", (DL_FUNC) &setMatterArray, 3},
		{"C_getMatterListElt", (DL_FUNC) &getMatterListElt, 3},
		{"C_setMatterListElt", (DL_FUNC) &setMatterListElt, 4},
		{"C_getMatterListSubset", (DL_FUNC) &getMatterListSubset, 3},
		{"C_setMatterListSubset", (DL_FUNC) &setMatterListSubset, 4},
		{"C_getMatterStrings", (DL_FUNC) &getMatterStrings, 3},
		{"C_setMatterStrings", (DL_FUNC) &setMatterStrings, 4},
		// sparse data structures
		{"C_getSparseVector", (DL_FUNC) &getSparseVector, 2},
		{"C_getSparseMatrixC", (DL_FUNC) &getSparseMatrixC, 3},
		{"C_getSparseMatrixR", (DL_FUNC) &getSparseMatrixR, 3},
		// matter altrep
		{"C_newMatterAltrep", (DL_FUNC) &newMatterAltrep, 6},
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

}
