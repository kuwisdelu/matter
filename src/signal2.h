#ifndef SIGNAL2
#define SIGNAL2

#include "matterDefines.h"
#include "search.h"
#include "signal.h"

//// Summarize via statistic 
//---------------------------

template<typename T>
double do_sum(T * x, int * indx, size_t n)
{
	double sx = 0;
	for ( index_t i = 0; i < n; i++ )
	{
		if ( isNA(x[indx[i]]) )
			return NA_REAL;
		sx += x[indx[i]];
	}
	return sx;
}

template<typename T>
double do_mean(T * x, int * indx, size_t n)
{
	double sx = do_sum(x, indx, n);
	if ( isNA(sx) )
		return NA_REAL;
	return sx / n;
}

template<typename T>
double do_max(T * x, int * indx, size_t n)
{
	if ( n <= 0 )
		return NA_REAL;
	T mx = x[indx[0]];
	for ( index_t i = 0; i < n; i++ )
	{
		if ( isNA(x[indx[i]]) )
			return NA_REAL;
		else if ( x[indx[i]] > mx )
			mx = x[indx[i]];
	}
	return static_cast<double>(mx);
}

template<typename T>
double do_min(T * x, int * indx, size_t n)
{
	if ( n <= 0 )
		return NA_REAL;
	T mx = x[indx[0]];
	for ( index_t i = 0; i < n; i++ )
	{
		if ( isNA(x[indx[i]]) )
			return NA_REAL;
		else if ( x[indx[i]] < mx )
			mx = x[indx[i]];
	}
	return static_cast<double>(mx);
}

//// Summarize via kernel 
//-------------------------

template<typename Txy, typename Tz>
double do_klinear2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	double rx, double ry, int * indx, size_t n)
{
	double zi = 0, K0 = 0, ki, kj;
	for ( index_t i = 0; i < n; i++ )
	{
		ki = klinear(udiff(x[indx[i]], xi) / rx);
		kj = klinear(udiff(y[indx[i]], yi) / ry);
		zi += ki * kj * z[indx[i]];
		K0 += ki * kj;
	}
	return zi / K0;
}

template<typename Txy, typename Tz>
double do_kcubic2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	double rx, double ry, int * indx, size_t n)
{
	double zi = 0, K0 = 0, ki, kj;
	double r2x = 0.5 * rx, r2y = 0.5 * ry;
	for ( index_t i = 0; i < n; i++ )
	{
		ki = kcubic(udiff(x[indx[i]], xi) / r2x);
		kj = kcubic(udiff(y[indx[i]], yi) / r2y);
		zi += ki * kj * z[indx[i]];
		K0 += ki * kj;
	}
	return zi / K0;
}

template<typename Txy, typename Tz>
double do_kgaussian2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	double sdx, double sdy, int * indx, size_t n)
{
	double zi = 0, K0 = 0, ki, kj;
	for ( index_t i = 0; i < n; i++ )
	{
		ki = kgaussian(udiff(x[indx[i]], xi), sdx);
		kj = kgaussian(udiff(y[indx[i]], yi), sdy);
		zi += ki * kj * z[indx[i]];
		K0 += ki * kj;
	}
	return zi / K0;
}

template<typename Txy, typename Tz>
double do_klanczos2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	double ax, double ay, int * indx, size_t n)
{
	double zi = 0, K0 = 0, ki, kj;
	for ( index_t i = 0; i < n; i++ )
	{
		ki = klanczos(udiff(x[indx[i]], xi), ax);
		kj = klanczos(udiff(y[indx[i]], yi), ay);
		zi += ki * kj * z[indx[i]];
		K0 += ki * kj;
	}
	return zi / K0;
}

//// Resampling with interpolation
//---------------------------------

// statistic interpolation over indx
template<typename T>
double interp2_stat(T * z, int * indx, size_t n, int stat = EST_AVG)
{
	switch(stat) {
		case EST_AVG:
			return do_mean(z, indx, n);
		case EST_SUM:
			return do_sum(z, indx, n);
		case EST_MAX:
			return do_max(z, indx, n);
		case EST_MIN:
			return do_min(z, indx, n);
		default:
			return NA_REAL;
	}
}

// kernel interpolation using x/y at indx
template<typename Txy, typename Tz>
double interp2_kern(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	int * indx, size_t n, double tol[2], int tol_ref, int kernel = EST_LERP)
{
	double ax = (tol_ref == ABS_DIFF) ? tol[0] : xi * tol[0];
	double ay = (tol_ref == ABS_DIFF) ? tol[1] : yi * tol[1];
	switch(kernel) {
		case EST_LERP:
			return do_klinear2(xi, yi, x, y, z, ax, ay, indx, n);
		case EST_CUBIC:
			return do_kcubic2(xi, yi, x, y, z, ax, ay, indx, n);
		case EST_GAUS:
		{
			double sdx = 0.5 * ax, sdy = 0.5 * ay;
			return do_kgaussian2(xi, yi, x, y, z, sdx, sdy, indx, n);
		}
		case EST_SINC:
			return do_klanczos2(xi, yi, x, y, z, ax, ay, indx, n);
		default:
			return NA_REAL;
	}
}

// interpolate at xi using x/y and indx
template<typename Txy, typename Tz>
double interp2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	int * indx, size_t n, double tol[2], int tol_ref, int interp = EST_NEAR)
{
	double dx, dy;
	switch(interp)
	{
		case EST_NEAR:
		{
			double d2 [n];
			for ( index_t i = 0; i < n; i++ )
			{
				dx = sdiff(xi, x[indx[i]], tol_ref);
				dy = sdiff(yi, y[indx[i]], tol_ref);
				d2[i] = (dx * dx) + (dy * dy);
			}
			return z[indx[argmin(d2, n)]];
		}
		case EST_AVG:
		case EST_SUM:
		case EST_MAX:
		case EST_MIN:
			return interp2_stat(z, indx, n, interp);
		case EST_AREA:
			Rf_error("interp = 'area' not supported for 2D signals");
		case EST_LERP:
		case EST_CUBIC:
		case EST_GAUS:
		case EST_SINC:
			return interp2_kern(xi, yi, x, y, z, indx, n, tol, tol_ref, interp);
		default:
			Rf_error("unrecognized interpolation method");
	}
}

// approximate z ~ (x, y) at (xi, yi) with interpolation
template<typename Txy, typename Tz, typename Tout>
Tout approx2(Txy xi, Txy yi, Txy * xy, Tz * z, int * indx, size_t n,
	double tol[2], int tol_ref, Tout nomatch, int * left_child, int * right_child,
	index_t root, int interp = EST_NEAR)
{
	if ( isNA(xi) || isNA(yi) )
		return NA<Tout>();
	Tout zi = nomatch;
	Txy xyi[] = {xi, yi};
	index_t knn = kd_tree_search(indx, xyi, xy, 2, n,
		left_child, right_child, root, tol, tol_ref);
	if ( knn > 0 )
	{
		Txy * x = xy;
		Txy * y = xy + n;
		zi = interp2(xi, yi, x, y, z, indx, knn, tol, tol_ref, interp);
	}
	return zi;
}

// approximate z ~ (x, y) at (xi, yi) with interpolation
template<typename Txy, typename Tz, typename Tout>
index_t do_approx2(Tout * ptr, Txy * xi, Txy * yi, size_t ni, Txy * xy, Tz * z, size_t n,
	double tol[2], int tol_ref, Tout nomatch, int interp = EST_NEAR, int stride = 1)
{
	for ( index_t i = 0; i < ni; i++ )
		ptr[i * stride] = nomatch;
	if ( n == 0 )
		return 0;
	index_t num_matches = 0;
	int * indx = R_Calloc(n, int);
	int * left_child = R_Calloc(n, int);
	int * right_child = R_Calloc(n, int);
	index_t root = kd_tree_build(xy, 2, n, left_child, right_child);
	for ( index_t i = 0; i < ni; i++ )
	{
		if ( isNA(xi[i]) || isNA(yi[i]) )
			continue;
		Tout zi = approx2(xi[i], yi[i], xy, z, indx, n,
			tol, tol_ref, NA<Tout>(), left_child, right_child,
			root, interp);
		if ( !isNA(zi) )
		{
			ptr[i * stride] = zi;
			num_matches++;
		}
	}
	Free(right_child);
	Free(left_child);
	Free(indx);
	return num_matches;
}

#endif // SIGNAL2
