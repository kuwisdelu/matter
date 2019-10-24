#ifndef VECTOOLS
#define VECTOOLS

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "matterDefines.h"
#include "utils.h"

extern "C" {

	SEXP localMaxima(SEXP x, SEXP halfWindow);

	SEXP regionMaxima(SEXP x, SEXP m, SEXP halfWindow);

	SEXP binMeans(SEXP x, SEXP lower, SEXP upper);

    SEXP binSums(SEXP x, SEXP lower, SEXP upper);

    SEXP binMins(SEXP x, SEXP lower, SEXP upper);

    SEXP binMaxs(SEXP x, SEXP lower, SEXP upper);

	SEXP groupMeans(SEXP x, SEXP group, SEXP ngroup, SEXP init);

    SEXP groupSums(SEXP x, SEXP group, SEXP ngroup, SEXP init);

    SEXP groupMins(SEXP x, SEXP group, SEXP ngroup, SEXP init);

    SEXP groupMaxs(SEXP x, SEXP group, SEXP ngroup, SEXP init);

}

//// Local extrama
//-----------------

template<typename T>
SEXP local_maxima(T * x, int halfWindow, int length);

template<typename T>
SEXP region_maxima(T * x, int * m, int halfWindow, int length, int nmax);

//// Binning
//------------

template<typename T>
SEXP bin_means(T * x, int * lower, int * upper, int length, int nbin);

template<typename T>
SEXP bin_sums(T * x, int * lower, int * upper, int length, int nbin);

template<typename T>
SEXP bin_maxs(T * x, int * lower, int * upper, int length, int nbin);

template<typename T>
SEXP bin_mins(T * x, int * lower, int * upper, int length, int nbin);

//// Grouped summaries
//---------------------

template<typename T>
SEXP group_means(T * x, int * group, int ngroup, int length, double init);

template<typename T>
SEXP group_sums(T * x, int * group, int ngroup, int length, double init);

template<typename T>
SEXP group_maxs(T * x, int * group, int ngroup, int length, double init);

template<typename T>
SEXP group_mins(T * x, int * group, int ngroup, int length, double init);

#endif // VECTOOLS
