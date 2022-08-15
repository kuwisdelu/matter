#ifndef SIGTOOLS
#define SIGTOOLS

#define R_NO_REMAP

#include <R.h>

#include <cmath>

#define ABS_DIFF    1
#define REL_DIFF_X  2
#define REL_DIFF_Y  3

//// Relative differences
//------------------------

// return the absolute or relative (signed) change between two values
template<typename T>
double rel_change(T x, T y, int ref = ABS_DIFF)
{
    switch(ref) {
        case ABS_DIFF:
            return static_cast<double>(x - y);
        case REL_DIFF_X:
            return static_cast<double>(x - y) / x;
        case REL_DIFF_Y:
            return static_cast<double>(x - y) / y;
        default:
            return NA_REAL;
    }
}

template<> inline
double rel_change<const char *>(const char * x, const char * y, int ref)
{
    int i = -1, sign = 1;
    int n = 0, nx = 0, ny = 0;
    // count number of initial matched characters
    while ( x[nx] != '\0' || y[ny] != '\0' ) {
        if ( x[nx] != y[ny] && i < 0 ) {
            i = nx > ny ? nx : ny;
            sign = x[nx] < y[ny] ? -1 : 1;
        }
        if ( x[nx] != '\0' )
            nx++;
        if ( y[ny] != '\0' )
            ny++;
    }
    // check string lengths
    n = nx > ny ? nx : ny;
    i = i < 0 ? n : i;
    // return character-based difference
    switch(ref) {
        case ABS_DIFF:
            return sign * static_cast<double>(n - i);
        case REL_DIFF_X:
            return sign * static_cast<double>(n - i) / nx;
        case REL_DIFF_Y:
            return sign * static_cast<double>(n - i) / ny;
        default:
            return NA_REAL;
    }
}

template<> inline
double rel_change<SEXP>(SEXP x, SEXP y, int ref)
{
    if ( TYPEOF(x) != TYPEOF(y) )
        Rf_error("'x' and 'y' must have the same type");
    switch(TYPEOF(x)) {
        case CHARSXP:
            return rel_change(CHAR(x), CHAR(y), ref);
        case STRSXP:
            return rel_change(CHAR(Rf_asChar(x)), CHAR(Rf_asChar(y)), ref);
        case INTSXP:
            return rel_change(Rf_asInteger(x), Rf_asInteger(y), ref);
        case REALSXP:
            return rel_change(Rf_asReal(x), Rf_asReal(y), ref);
        default:
            Rf_error("supported types are 'integer', 'double', or 'character'");
    }
}

// return the absolute or relative (unsigned) difference between two values
template<typename T>
double rel_diff(T x, T y, int ref = ABS_DIFF)
{
    return fabs(rel_change<T>(x, y, ref));
}

//// Kernels
//-----------

inline double sinc(double x)
{
    if ( x == 0 )
        return 1;
    else
        return sin(x) / x;
}

// Lanczos kernel
inline double klanczos(double x, double a)
{
    return sinc(M_PI * x) * sinc(M_PI * x / a);
}

// Gaussian kernel
inline double kgaussian(double x, double sd)
{
    return exp(-(x * x) / (2 * (sd * sd)));
}

//// Interpolation
//-----------------

// linear interpolation
inline double lerp(double y0, double y1, double t)
{
    return y0 + t * (y1 - y0);
}

// cubic hermite spline interpolation
inline double cubic(double y[4], double dx[3], double t)
{
    double dy[] = {y[1] - y[0], y[2] - y[1], y[3] - y[2]};
    double m1, m2;
    // calculate slope m1 using finite difference method
    if ( dx[0] > 0 )
        m1 = 0.5 * ((dy[0] / dx[0]) + (dy[1] / dx[1]));
    else
        m1 = 0.5 * (dy[0] + dy[1]) / (dx[0] + dx[1]);
    // calculate slope m2 using finite difference method
    if ( dx[2] > 0 )
        m2 = 0.5 * ((dy[1] / dx[1]) + (dy[2] / dx[2]));
    else
        m2 = 0.5 * (dy[1] + dy[2]) / (dx[1] + dx[2]);
    // calculate hermite spline basis functions
    double p00 = (2 * (t * t * t) - 3 * (t * t) + 1) * y[1];
    double p10 = ((t * t * t) - 2 * (t * t) + t) * dx[1] * m1;
    double p01 = (-2 * (t * t * t) + 3 * (t * t)) * y[2];
    double p11 = ((t * t * t) - (t * t)) * dx[1] * m2;
    // return interpolated value
    return p00 + p10 + p01 + p11;
}

#endif // SIGTOOLS