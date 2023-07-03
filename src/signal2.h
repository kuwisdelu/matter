#ifndef SIGNAL2
#define SIGNAL2

#include "matterDefines.h"
#include "search.h"
#include "signal.h"

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

//// Filtering and smoothing
//---------------------------

template<typename T>
void mean_filter2(T * x, int nr, int nc, int width, double * buffer)
{
	int r = width / 2;
	double vprev, vcurr;
	double y[nr * nc];
	double * z = buffer;
	// horizontal filter pass
	for ( index_t i = 0; i < nr; i++ )
	{
		y[i] = r * x[i];
		for ( index_t j = 0; j <= r && j < nc; j++ )
			y[i] += x[j * nr + i];
		for ( index_t j = 1; j < nc; j++ )
		{
			vprev = x[wrap_ind(j - r - 1, nc) * nr + i];
			vcurr = x[wrap_ind(j + r, nc) * nr + i];
			y[j * nr + i] = y[(j - 1) * nr + i] - vprev + vcurr;
		}
		for ( index_t j = 0; j < nc; j++ )
			y[j * nr + i] /= width;
	}
	// vertical filter pass
	for ( index_t j = 0; j < nc; j++ )
	{
		z[j * nr] = r * y[j * nr];
		for ( index_t i = 0; i <= r && i < nr; i++ )
			z[j * nr] += y[j * nr + i];
		for ( index_t i = 1; i < nr; i++ )
		{
			vprev = y[j * nr + wrap_ind(i - r - 1, nr)];
			vcurr = y[j * nr + wrap_ind(i + r, nr)];
			z[j * nr + i] = z[j * nr + i - 1] - vprev + vcurr;
		}
		for ( index_t i = 0; i < nr; i++ )
			z[j * nr + i] /= width;
	}
}

template<typename T>
void linear_filter2(T * x, int nr, int nc,
	double * weights, int width, double * buffer)
{
	int r = width / 2;
	index_t ii, jj;
	for ( index_t i = 0; i < nr; i++ )
	{
		for ( index_t j = 0; j < nc; j++ )
		{
			double xij, wij, W = 0;
			buffer[j * nr + i] = 0;
			for (index_t ki = 0; ki < width; ki++ )
			{
				for (index_t kj = 0; kj < width; kj++ )
				{
					ii = wrap_ind(i + ki - r, nr);
					jj = wrap_ind(j + kj - r, nc);
					xij = x[jj * nr + ii];
					wij = weights[kj * width + ki];
					buffer[j * nr + i] += wij * xij;
					W += wij;
				}
			}
			buffer[j * nr + i] /= W;
		}
	}
}

template<typename T>
void bilateral_filter2(T * x, int nr, int nc, int width,
	double sddist, double sdrange, double spar, double * buffer)
{
	int r = width / 2;
	int n = nr * nc;
	index_t ii, jj;
	bool xnan = false, outnan = false;
	double sdd = sddist, sdr = sdrange;
	double mad, xrange;
	if ( !isNA(spar) )
	{
		// get MAD if using adaptive parameters
		mad = quick_mad(x, n);
		double xmin = n > 0 ? x[0] : NA_REAL;
		double xmax = n > 0 ? x[0] : NA_REAL;;
		for ( index_t i = 1; i < n; i++ )
		{
			if ( x[i] > xmax )
				xmax = x[i];
			if ( x[i] < xmin )
				xmin = x[i];
		}
		xrange = xmax - xmin;
	}
	for ( index_t i = 0; i < nr; i++ )
	{
		for ( index_t j = 0; j < nc; j++ )
		{
			double xij, dmean = 0, W = 0;
			buffer[j * nr + i] = 0;
			if ( !isNA(spar) )
			{
				// modified version of Joseph & Periyasamy (2018)
				for ( index_t ki = 0; ki < width; ki++ )
				{
					for ( index_t kj = 0; kj < width; kj++ )
					{
						// find mean of local differences
						ii = wrap_ind(i + ki - r, nr);
						jj = wrap_ind(j + kj - r, nc);
						xij = x[jj * nr + ii];
						dmean += std::fabs(xij - x[j * nr + i]) / width;
					}
				}
				// calculate adaptive parameters
				double z = std::fabs(dmean - mad) / spar;
				if ( isNA(sddist) )
					sdd = r * std::exp(-z) / std::sqrt(2);
				if ( isNA(sdrange) )
					sdr = xrange * std::exp(-z) / std::sqrt(2);
			}
			if ( sdd <= DBL_EPSILON || sdr <= DBL_EPSILON )
			{
				// avoid singularities
				buffer[j * nr + i] = x[j * nr + i];
				continue;
			}
			for ( index_t ki = 0; ki < width; ki++ )
			{
				for ( index_t kj = 0; kj < width; kj++ )
				{
					// standard bilateral filter
					ii = wrap_ind(i + ki - r, nr);
					jj = wrap_ind(j + kj - r, nc);
					xij = x[jj * nr + ii];
					double wtdist = kgaussian(i - r, sdd) * kgaussian(j - r, sdd);
					double wtrange = kgaussian(xij - x[j * nr + i], sdr);
					buffer[j * nr + i] += wtdist * wtrange * xij;
					W += wtdist * wtrange;
				}
			}
			buffer[j * nr + i] /= W;
			if ( !xnan && isNA(x[j * nr + i]) )
				xnan = true;
			if ( !outnan && isNA(buffer[j * nr + i]) )
				outnan = true;
		}
	}
	if ( outnan && !xnan )
	{
		if ( isNA(spar) )
			Rf_warning("NAs introduced; try larger values of sddist or sdrange");
		else
			Rf_warning("NAs introduced; try a larger value of spar");
	}
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
