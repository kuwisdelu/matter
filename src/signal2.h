#ifndef SIGNAL2
#define SIGNAL2

#include "matterDefines.h"
#include "search.h"
#include "signal.h"

//// Resampling with interpolation
//---------------------------------

// statistic interpolation using x/y at indx
template<typename Txy, typename Tz>
double interp2_stat(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	int * indx, size_t n, int interp = EST_NEAR)
{
	// TODO
	return 0;
}

// kernel interpolation using x/y at indx
template<typename Txy, typename Tz>
double interp2_kern(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	int * indx, size_t n, int interp = EST_NEAR)
{
	// TODO
	return 0;
}

// interpolate at xi using x/y and indx
template<typename Txy, typename Tz>
double interp2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	int * indx, size_t n, double tol, int tol_ref, int interp = EST_NEAR)
{
	// TODO
	return 0;
}

// approximate z ~ (x, y) at (xi, yi) with interpolation
template<typename Txy, typename Tz, typename Tout>
Tout approx1(Txy xi, Txy yi, Txy * data, Tz * z, int * indx,
	size_t n, double tol, int tol_ref, Tout nomatch, size_t root,
	int * left_child, int * right_child, int interp = EST_NEAR)
{
	// TODO
	return 0;
}

// approximate z ~ (x, y) at (xi, yi) with interpolation
template<typename Txy, typename Tz, typename Tout>
index_t do_approx1(Tout * ptr, Txy xi, Txy yi, Txy * data, Tz * z,
	size_t n, double tol, int tol_ref, Tout nomatch, size_t root,
	int * left_child, int * right_child, int interp = EST_NEAR, int stride = 1)
{
	// TODO
	return 0;
}

#endif // SIGNAL2
