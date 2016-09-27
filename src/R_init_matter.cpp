#include <R_ext/Rdynload.h>
#include "matter.h"

static const R_CallMethodDef callMethods[] = {
    {"getVector", (DL_FUNC) &getVector, 1},
    {"setVector", (DL_FUNC) &setVector, 2},
    {"getVectorElements", (DL_FUNC) &getVectorElements, 2},
    {"setVectorElements", (DL_FUNC) &setVectorElements, 3},
    {"getMatrix", (DL_FUNC) &getMatrix, 1},
    {"setMatrix", (DL_FUNC) &setMatrix, 2},
    {"getMatrixRows", (DL_FUNC) &getMatrixRows, 2},
    {"setMatrixRows", (DL_FUNC) &setMatrixRows, 3},
    {"getMatrixCols", (DL_FUNC) &getMatrixCols, 2},
    {"setMatrixCols", (DL_FUNC) &setMatrixCols, 3},
    {"getMatrixElements", (DL_FUNC) &getMatrixElements, 3},
    {"setMatrixElements", (DL_FUNC) &setMatrixElements, 4},
    {"getSum", (DL_FUNC) &getSum, 2},
    {"getMean", (DL_FUNC) &getMean, 2},
    {"getVar", (DL_FUNC) &getVar, 2},
    {"getColSums", (DL_FUNC) &getColSums, 2},
    {"getColVar", (DL_FUNC) &getColVar, 2},
    {"getRowSums", (DL_FUNC) &getRowSums, 2},
    {"getRowMeans", (DL_FUNC) &getRowMeans, 2},
    {"getRowVar", (DL_FUNC) &getRowVar, 2},
    {"rightMultRMatrix", (DL_FUNC) &rightMultRMatrix, 2},
    {"leftMultRMatrix", (DL_FUNC) &leftMultRMatrix, 2},
    {NULL, NULL, 0}
};

void R_init_matter(DllInfo * info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
