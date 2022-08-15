#ifndef VECTOOLS
#define VECTOOLS

#define R_NO_REMAP

#include <cmath>

#include <R.h>
#include <Rinternals.h>

#include "matterDefines.h"
#include "utils.h"

//// Kernels
//-----------

inline double sinc(double x)
{
    if ( x == 0 )
        return 1;
    else
        return sin(x) / x;
}

inline double kgaussian(double x, double sd)
{
    return exp(-(x * x) / (2 * (sd * sd)));
}

inline double klanczos(double x, double a)
{
    return sinc(M_PI * x) * sinc(M_PI * x / a);
}

//// Interpolation
//-----------------

inline double lerp(double y0, double y1, double t)
{
    return y0 + t * (y1 - y0);
}

inline double cubic(double * y, double * dx, double t)
{
    double dy[] = {y[1] - y[0], y[2] - y[1], y[3] - y[2]};
    double m1, m2;
    if ( dx[0] > 0 )
        m1 = 0.5 * ((dy[0] / dx[0]) + (dy[1] / dx[1]));
    else
        m1 = 0.5 * (dy[0] + dy[1]) / (dx[0] + dx[1]);
    if ( dx[2] > 0 )
        m2 = 0.5 * ((dy[1] / dx[1]) + (dy[2] / dx[2]));
    else
        m2 = 0.5 * (dy[1] + dy[2]) / (dx[1] + dx[2]);
    double p00 = (2 * (t * t * t) - 3 * (t * t) + 1) * y[1];
    double p10 = ((t * t * t) - 2 * (t * t) + t) * dx[1] * m1;
    double p01 = (-2 * (t * t * t) + 3 * (t * t)) * y[2];
    double p11 = ((t * t * t) - (t * t)) * dx[1] * m2;
    return p00 + p10 + p01 + p11;
}

//// Local extrema
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

//// R-level exports
//--------------------

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

#endif // VECTOOLS
