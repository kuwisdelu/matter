#ifndef SIGNAL
#define SIGNAL

#include "matterDefines.h"

// interpolation scheme
// (must match factor levels)
#define EST_NEAR	1
#define EST_AVG		2
#define EST_SUM		3
#define EST_MAX		4
#define EST_MIN		5
#define EST_AUC		6
#define EST_LERP	7
#define EST_CUBIC	8
#define EST_GAUS	9
#define EST_SINC	10 // Lanczos

// binning scheme
#define BIN_SUM		1
#define BIN_AVG		2
#define BIN_MAX		3
#define BIN_MIN		4
#define BIN_SD		5
#define BIN_VAR		6
#define BIN_SSE		7

//// Kernels
//-----------

inline double sinc(double x)
{
	if ( x )
		return std::sin(x) / x;
	else
		return 1;
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
					case EST_AUC:
						Rf_error("interp = 'auc' not implemented yet");
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
		// additional calculations
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
				else if ( interp == EST_CUBIC )
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
	return val;
}

//// Binning
//------------

template<typename T>
void bin_vector(T * x, int n, int * lower, int * upper,
	double * buffer, int nbin, int stat = BIN_SUM)
{
	double * buf = buffer;
	for ( size_t i = 0; i < nbin; i++ ) {
		buf[i] = NA_REAL;
		if ( lower[i] < 1 || lower[i] > n )
			Rf_error("lower bin limit out of range");
		if ( upper[i] < 1 || upper[i] > n )
			Rf_error("upper bin limit out of range");
		int size = (upper[i] - lower[i]) + 1;
		double sumx = 0;
		for ( size_t j = lower[i] - 1; j < upper[i] && j < n; j++ )
		{
			// sum, mean, min, max
			switch(stat) {
				case BIN_MAX:
					buf[i] = isNA(buf[i]) ? x[j] : (x[j] > buf[i] ? x[j] : buf[i]);
					break;
				case BIN_MIN:
					buf[i] = isNA(buf[i]) ? x[j] : (x[j] < buf[i] ? x[j] : buf[i]);
					break;
				default:
					sumx += x[j];
					break;
			}
		}
		if ( stat == BIN_SD || stat == BIN_VAR || stat == BIN_SSE )
		{
			index_t start = lower[i] - 1, end = upper[i];
			if ( stat == BIN_SSE )
			{
				// expand to adjacent bins
				start = start - 1 < 0 ? 0 : start - 1;
				end = end + 1 > n ? n : end + 1;
			}
			double ux = sumx / size, ut = (start + end) / 2;
			double Sxx = 0, Stt = 0, Sxt = 0;
			for ( index_t j = start; j < end; j++ )
			{
				// calculate linear regression
				Sxx += (ux - x[j]) * (ux - x[j]);
				Stt += (ut - j) * (ut - j);
				Sxt += (ux - x[j]) * (ut - j);
			}
			if ( stat == BIN_SSE )
				// sum of squared errors
				buf[i] = Sxx * (1 - (Sxt * Sxt) / (Stt * Sxx));
			else if ( size > 1 )
				// variance
				buf[i] = Sxx / (size - 1);
		}
		switch(stat) {
			case BIN_SUM:
				buf[i] = sumx;
				break;
			case BIN_AVG:
				buf[i] = sumx / size;
				break;
			case BIN_SD:
				buf[i] = std::sqrt(buf[i]);
				break;
		}
	}
}

//// Peak detection
//------------------

template<typename T>
size_t local_maxima(T * x, size_t n, int width, int * peaks)
{
	int nmax = 0, a = 0, b = n, r = abs(width / 2);
	for ( int i = 0; i < n; i++ )
	{
		peaks[i] = false;
		if ( i < r || i > n - r )
			continue;
		a = (i - r) > 0 ? (i - r) : 0;
		b = (i + r) < n - 1 ? (i + r) : n - 1;
		for ( int j = a; j <= b; j++ )
		{
			peaks[i] = true;
			if ( j < i && x[j] >= x[i] )
			{
				peaks[i] = false;
				break;
			}
			if ( j > i && x[j] > x[i] )
			{
				peaks[i] = false;
				break;
			}
			nmax++;
		}
	}
	return nmax;
}

// find local boundaries (minima) of peaks
template<typename T>
size_t peak_boundaries(T * x, size_t n,
	int width, int * peaks, size_t npeaks,
	int * left_bounds, int * right_bounds)
{
	int r = abs(width / 2);
	for ( int i = 0; i < npeaks; i++ )
	{
		left_bounds[i] = peaks[i];
		for ( int j = peaks[i] - 1; j >= 0; j-- )
		{
			if ( x[j] < x[left_bounds[i]] )
				left_bounds[i] = j;
			else
			{
				int lmin = left_bounds[i];
				int lwindow = (lmin - r) > 0 ? (lmin - r) : 0;
				j--;
				while ( j >= lwindow )
				{
					if ( x[j] < x[lmin] )
					{
						left_bounds[i] = j;
						break;
					}
					j--;
				}
				if ( lmin == left_bounds[i] )
					break;
			}
		}
		right_bounds[i] = peaks[i];
		for ( int j = peaks[i] + 1; j < n; j++ )
		{
			if ( x[j] < x[right_bounds[i]] )
				right_bounds[i] = j;
			else
			{
				int rmin = right_bounds[i];
				int rwindow = (rmin + r) < n - 1 ? (rmin + r) : n - 1;
				j++;
				while ( j <= rwindow )
				{
					if ( x[j] < x[rmin] )
					{
						right_bounds[i] = j;
						break;
					}
					j++;
				}
				if ( rmin == right_bounds[i] )
					break;
			}
		}
	}
	return npeaks;
}

// find baselines of peaks (relative to higher peaks)
template<typename T>
size_t peak_bases(T * x, size_t n,
	int wlen, int * peaks, size_t npeaks,
	int * left_bases, int * right_bases)
{
	int r = abs(wlen / 2);
	for ( int i = 0; i < npeaks; i++ )
	{
		left_bases[i] = peaks[i];
		for ( int j = peaks[i] - 1; j >= 0; j-- )
		{
			if ( x[j] > x[peaks[i]] || j < peaks[i] - r )
				break;
			if ( x[j] < x[left_bases[i]] )
				left_bases[i] = j;
		}
		right_bases[i] = peaks[i];
		for ( int j = peaks[i] + 1; j < n; j++ )
		{
			if ( x[j] > x[peaks[i]] || j > peaks[i] + r )
				break;
			if ( x[j] < x[right_bases[i]] )
				right_bases[i] = j;
		}
	}
	return npeaks;
}

#endif // SIGNAL
