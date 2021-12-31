#include <R_ext/Rdynload.h>

#include "bsearch.h"
#include "matter.h"
#include "altrep.h"
#include "vectools.h"

extern "C" {

    static const R_CallMethodDef callMethods[] = {
        {"C_relativeDiff", (DL_FUNC) &relativeDiff, 3},
        {"C_binarySearch", (DL_FUNC) &binarySearch, 6},
        {"C_keyvalSearch", (DL_FUNC) &keyvalSearch, 8},
        {"C_createAtoms", (DL_FUNC) &createAtoms, 5},
        {"C_getVector", (DL_FUNC) &getVector, 1},
        {"C_setVector", (DL_FUNC) &setVector, 2},
        {"C_getVectorElements", (DL_FUNC) &getVectorElements, 2},
        {"C_setVectorElements", (DL_FUNC) &setVectorElements, 3},
        {"C_getList", (DL_FUNC) &getList, 1},
        {"C_setList", (DL_FUNC) &setList, 2},
        {"C_getListElements", (DL_FUNC) &getListElements, 3},
        {"C_setListElements", (DL_FUNC) &setListElements, 4},
        {"C_getMatrix", (DL_FUNC) &getMatrix, 1},
        {"C_setMatrix", (DL_FUNC) &setMatrix, 2},
        {"C_getMatrixRows", (DL_FUNC) &getMatrixRows, 2},
        {"C_setMatrixRows", (DL_FUNC) &setMatrixRows, 3},
        {"C_getMatrixCols", (DL_FUNC) &getMatrixCols, 2},
        {"C_setMatrixCols", (DL_FUNC) &setMatrixCols, 3},
        {"C_getMatrixElements", (DL_FUNC) &getMatrixElements, 3},
        {"C_setMatrixElements", (DL_FUNC) &setMatrixElements, 4},
        {"C_getString", (DL_FUNC) &getString, 1},
        {"C_getStringElements", (DL_FUNC) &getStringElements, 2},
        {"C_getRange", (DL_FUNC) &getRange, 2},
        {"C_getProd", (DL_FUNC) &getProd, 2},
        {"C_getSum", (DL_FUNC) &getSum, 2},
        {"C_getMean", (DL_FUNC) &getMean, 2},
        {"C_getVar", (DL_FUNC) &getVar, 2},
        {"C_getAny", (DL_FUNC) &getAny, 2},
        {"C_getAll", (DL_FUNC) &getAll, 2},
        {"C_getColSums", (DL_FUNC) &getColSums, 2},
        {"C_getColMeans", (DL_FUNC) &getColMeans, 2},
        {"C_getColVars", (DL_FUNC) &getColVars, 2},
        {"C_getRowSums", (DL_FUNC) &getRowSums, 2},
        {"C_getRowMeans", (DL_FUNC) &getRowMeans, 2},
        {"C_getRowVars", (DL_FUNC) &getRowVars, 2},
        {"C_rightMatrixMult", (DL_FUNC) &rightMatrixMult, 2},
        {"C_leftMatrixMult", (DL_FUNC) &leftMatrixMult, 2},
        {"C_getWhich", (DL_FUNC) &getWhich, 1},
        {"C_countRuns", (DL_FUNC) &countRuns, 2},
        {"C_createDRLE", (DL_FUNC) &createDRLE, 3},
        {"C_getDRLE", (DL_FUNC) &getDRLE, 1},
        {"C_getDRLEElements", (DL_FUNC) &getDRLEElements, 2},
        {"C_makeAltrep", (DL_FUNC) &makeAltrep, 6},
        {"C_localMaxima", (DL_FUNC) &localMaxima, 2},
        {"C_regionMaxima", (DL_FUNC) &regionMaxima, 3},
        {"C_binMeans", (DL_FUNC) &binMeans, 3},
        {"C_binSums", (DL_FUNC) &binSums, 3},
        {"C_binMins", (DL_FUNC) &binMins, 3},
        {"C_binMaxs", (DL_FUNC) &binMaxs, 3},
        {"C_groupMeans", (DL_FUNC) &groupMeans, 4},
        {"C_groupSums", (DL_FUNC) &groupSums, 4},
        {"C_groupMins", (DL_FUNC) &groupMins, 4},
        {"C_groupMaxs", (DL_FUNC) &groupMaxs, 4},
        {NULL, NULL, 0}
    };

    void R_init_matter(DllInfo * info)
    {
        init_MatterAlt_raw(info);
        init_MatterAlt_logical(info);
        init_MatterAlt_integer(info);
        init_MatterAlt_real(info);
        init_MatterAlt_string(info);
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    }

}
