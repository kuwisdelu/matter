#ifndef SIGNAL
#define SIGNAL

#include "Rutils.h"

#define EST_NEAR	1
#define EST_AVG		2
#define EST_SUM		3
#define EST_MAX		4
#define EST_MIN		5
#define EST_AREA	6
#define EST_LERP	7
#define EST_CUBIC	8
#define EST_GAUS	9
#define EST_SINC	10

//// Kernels
//-----------

inline double sinc(double x)
{
	if ( x == 0 )
		return 1;
	else
		return std::sin(x) / x;
}

// Lanczos kernel
inline double klanczos(double x, double a)
{
	return sinc(M_PI * x) * sinc(M_PI * x / a);
}

// Gaussian kernel
inline double kgaussian(double x, double sd)
{
	return std::exp(-(x * x) / (2 * (sd * sd)));
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

template<typename Tx, typename Ty>
Ty interp1(Tx xi, Tx * x, Ty * y, size_t start, size_t end,
	double tol, int tol_ref, int interp = EST_NEAR, bool sorted = true)
{
	// Rprintf("inside interp(%f)\n", xi);
	double delta, diff, diff_min = DBL_MAX;
	index_t pos = NA_INTEGER;
	index_t pj[] = {NA_INTEGER, NA_INTEGER, NA_INTEGER, NA_INTEGER}; // knots
	double pdiff[] = {DBL_MAX, DBL_MAX, DBL_MAX, DBL_MAX}; // dists to knots
	double wt = 1, wtnorm = 0; // kernel weights and normalizing constant
	size_t nxi = 0; // count of xi - x <= tol
	Ty val = NA<Ty>();
	// go right, then left
	for ( int k = 1; k == 1 || k == -1; k -= 2 )
	{
		// search for xi - x <= tol
		index_t init = k > 0 ? start : start - 1;
		for ( index_t i = init; i < end && i >= 0; i += k )
		{
			delta = rel_change<Tx>(xi, x[i], tol_ref);
			diff = std::fabs(delta);
			if ( diff > tol ) {
				if ( i > 0 && delta < 0 && sorted )
					break;
				if ( i < 0 && delta > 0 && sorted )
					break;
			}
			else
			{
				if ( diff < diff_min )
				{
					diff_min = diff;
					pos = i;
				}
				nxi++;
				// update interpolant
				switch(interp)
				{
					case EST_NEAR:
						if ( diff <= diff_min ) {
							val = y[i];
							if ( diff == 0 )
								return val;
						}
						break;
					case EST_SUM:
					case EST_AVG:
					case EST_GAUS:
					case EST_SINC:
						if ( interp == EST_GAUS )
							wt = kgaussian(diff, (2 * tol + 1) / 4);
						if ( interp == EST_SINC )
							Rf_error("interp = 'lanczos' not implemented yet");
						if ( nxi == 1 )
							val = wt * y[i];
						else
							val += wt * y[i];
						wtnorm += wt;
						break;
					case EST_AREA:
						Rf_error("interp = 'area' not implemented yet");
					case EST_MAX:
						if ( nxi == 1 || y[i] > val )
							val = y[i];
						break;
					case EST_MIN:
						if ( nxi == 1 || y[i] < val )
							val = y[i];
						break;
					case EST_LERP:
					case EST_CUBIC:
					{
						if ( delta > 0 )
						{
							if ( diff < pdiff[1] ) {
								pdiff[1] = diff;
								pj[1] = i;
							}
							if ( (diff > pdiff[1] && diff < pdiff[0]) ||
								(pj[1] == pj[0] || isNA(pj[0])) )
							{
								pdiff[0] = diff;
								pj[0] = i;
							}
						}
						if ( delta < 0 )
						{
							if ( diff < pdiff[2] ) {
								pdiff[2] = diff;
								pj[2] = i;
							}
							if ( (diff > pdiff[2] && diff < pdiff[3]) ||
								(pj[2] == pj[3] || isNA(pj[3])) )
							{
								pdiff[3] = diff;
								pj[3] = i;
							}
						}
						break;
					}
				}
			}
		}
	}
	if ( !isNA(pos) )
	{
		// additional calculation for interpolant
		switch(interp)
		{
			case EST_AVG:
			case EST_GAUS:
			case EST_SINC:
				val = val / wtnorm;
				break;
			case EST_LERP:
			case EST_CUBIC:
			{
				double tx, dxs[3], ys[4];
				pj[1] = !isNA(pj[1]) ? pj[1] : pos;
				pj[0] = !isNA(pj[0]) ? pj[0] : pj[1];
				pj[2] = !isNA(pj[2]) ? pj[2] : pos;
				pj[3] = !isNA(pj[3]) ? pj[3] : pj[2];
				tx = rel_change(xi, x[pj[1]]);
				dxs[0] = rel_change(x[pj[1]], x[pj[0]]);
				dxs[1] = rel_change(x[pj[2]], x[pj[1]]);
				dxs[2] = rel_change(x[pj[3]], x[pj[2]]);
				ys[0] = static_cast<double>(y[pj[0]]);
				ys[1] = static_cast<double>(y[pj[1]]);
				ys[2] = static_cast<double>(y[pj[2]]);
				ys[3] = static_cast<double>(y[pj[3]]);
				if ( interp == EST_LERP )
				{
					if ( dxs[1] > 0 && diff_min > 0 )
						val = lerp(ys[1], ys[2], tx / dxs[1]);
					else
						val = y[pos];
				}
				if ( interp == EST_CUBIC )
				{
					if ( dxs[1] > 0 && diff_min > 0 )
						val = cubic(ys, dxs, tx / dxs[1]);
					else
						val = y[pos];	
				}
				break;
			}
		}
	}
	// Rprintf("interp(%f) = %d | n = %d\n", xi, val, nxi);
	return val;
}

//// Peak detection
//------------------

// template<typename Tx, typename Ty>
// double peakwidth(Ty * y, Tx * x, index_t i, size_t len)
// {
// 	index_t pj[] = {i, i};
// 	double pH[] = {1, 1};
// 	double H, dy, dx;
// 	for ( index_t j = i; j < len; j++ )
// 	{
// 		H = y[j] / y[i];
// 		if ( rel_diff(H, 0.5) < pH[0] ) {
// 			pj[0] = j;
// 			pH[0] = H;
// 			if ( H < 0.5 )
// 				break;
// 		}
// 		else if ( j + 1 < len ) {
// 			dy = rel_change(y[j + 1], y[j]);
// 			dx = rel_change(x[j + 1], x[j]);
// 			if ( dy / dx >= 0 )
// 				break;
// 		}
// 	}
// 	for ( index_t j = i - 1; j >= 0; j-- )
// 	{
// 		H = y[j] / y[i];
// 		if ( rel_diff(H, 0.5) < pH[1] ) {
// 			pj[1] = j;
// 			pH[1] = H;
// 			if ( H < 0.5 )
// 				break;
// 		}
// 		else if ( j - 1 >= 0 ) {
// 			dy = rel_change(y[j], y[j - 1]);
// 			dx = rel_change(x[j], x[j - 1]);
// 			if ( dy / dx <= 0 )
// 				break;
// 		}
// 	}
// 	return rel_diff(x[pj[0]], x[pj[1]]);
// }

// template<typename Tx, typename Tout>
// SEXP locmax(Tx * x, int width, int len, Tout * buffer) {
// 	size_t n = 0, a = 0, b = len, r = abs(width / 2);
// 	for ( int i = 0; i < len; i++ )
// 	{
// 		buffer[i] = FALSE;
// 		a = (i - r) > 0 ? (i - r) : 0;
// 		b = (i + r) < (len - 1) ? (i + r) : (len - 1);
// 		for ( size_t j = a; j >= 0 && j <= b; j++ )
// 		{
// 			buffer[i] = true;
// 			if ( j < i )
// 			{
// 				if ( x[j] >= x[i] )
// 				{
// 					buffer[i] = FALSE;
// 					break;
// 				}
// 			}
// 			if ( j > i )
// 			{
// 				if ( x[j] > x[i] )
// 				{
// 					buffer[i] = FALSE;
// 					break;
// 				}
// 			}
// 			n++;
// 		}
// 	}
// 	return n;
// }

#endif // SIGNAL
