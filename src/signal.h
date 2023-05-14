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

//// Numeric Integration
//-----------------------

template<typename Tx, typename Ty>
double trapz(Tx * x, Ty * y, size_t lower, size_t upper)
{
	double sum = 0;
	for ( size_t i = lower + 1; i <= upper; i++ )
		sum += 0.5 * (x[i] - x[i - 1]) * (y[i] + y[i - 1]);
	return sum;
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
	// knots
	index_t pj[] = {NA_INTEGER, NA_INTEGER, NA_INTEGER, NA_INTEGER};
	// dists to knots
	double pdiff[] = {DBL_MAX, DBL_MAX, DBL_MAX, DBL_MAX};
	// kernel weights and normalizing constant
	double wt = 1, wtnorm = 0;
	// count of xi - x <= tol
	size_t nxi = 0;
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

//// Binning and resampling
//-------------------------

inline void bin_update(double * score, int * lower, int * upper,
	int nbin, int * lower_buffer, int * upper_buffer)
{
	int merge = 0, split = 0;
	double merge_score = DBL_MAX;
	double split_score = score[0];
	bool did_merge = false, did_split = false;
	// find which bins to merge and split
	for ( size_t i = 1; i < nbin; i++ )
	{
		if ( score[i - 1] + score[i] < merge_score )
		{
			merge = i - 1;
			merge_score = score[i - 1] + score[i];
		}
		if ( score[i] > split_score )
		{
			split = i;
			split_score = score[i];
		}
	}
	// update the bins
	for ( size_t i = 0; i < nbin; i++ )
	{
		if ( i != merge && i != split )
		{
			if ( did_merge && did_split )
			{
				lower_buffer[i] = lower[i];
				upper_buffer[i] = upper[i];
			}
			else if ( did_merge && !did_split )
			{
				lower_buffer[i - 1] = lower[i];
				upper_buffer[i - 1] = upper[i];
			}
			else if ( !did_merge && did_split )
			{
				lower_buffer[i + 1] = lower[i];
				upper_buffer[i + 1] = upper[i];
			}
			else if ( !did_merge && !did_split )
			{
				lower_buffer[i] = lower[i];
				upper_buffer[i] = upper[i];
			}
		}
		else if ( i == split )
		{
			int at = (lower[i] + upper[i]) / 2;
			if ( did_merge )
			{
				lower_buffer[i - 1] = lower[i];
				upper_buffer[i - 1] = at;
				lower_buffer[i] = at + 1;
				upper_buffer[i] = upper[i];
			}
			else
			{
				lower_buffer[i] = lower[i];
				upper_buffer[i] = at;
				lower_buffer[i + 1] = at + 1;
				upper_buffer[i + 1] = upper[i];
			}
			did_split = true;
		}
		else if ( i == merge )
		{
			if ( did_split ) {
				lower_buffer[i + 1] = lower[i];
				upper_buffer[i + 1] = upper[i + 1];
			}
			else {
				lower_buffer[i] = lower[i];
				upper_buffer[i] = upper[i + 1];
			}
			did_merge = true;
			i++;
		}
	}
}

template<typename T>
void bin_vector(T * x, int n, int * lower, int * upper,
	int nbin, double * buffer, int stat = BIN_SUM)
{
	double * buf = buffer;
	for ( size_t i = 0; i < nbin; i++ )
	{
		buf[i] = NA_REAL;
		if ( lower[i] < 0 || lower[i] >= n )
			Rf_error("lower bin limit out of range");
		if ( upper[i] < 0 || upper[i] >= n )
			Rf_error("upper bin limit out of range");
		int size = (upper[i] - lower[i]) + 1;
		double sumx = 0;
		for ( size_t j = lower[i]; j <= upper[i] && j < n; j++ )
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
			index_t a = lower[i], b = upper[i];
			if ( stat == BIN_SSE )
			{
				// expand to adjacent bins
				a = a - 1 < 0 ? 0 : a - 1;
				b = b + 1 > n - 1 ? n - 1 : b + 1;
			}
			double ux = sumx / size, ut = (a + b) / 2;
			double Sxx = 0, Stt = 0, Sxt = 0;
			for ( index_t j = a; j <= b; j++ )
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

template<typename Tx, typename Tt>
void sample_ltob(Tx * x, Tt * t, int n, int * lower, int * upper,
	int nbin, int * buffer, bool ind1 = false)
{
	double area;
	Tx xj[3];
	Tt tj[3];
	for ( size_t i = 0; i < nbin; i++ )
	{
		buffer[i] = lower[i] + ind1;
		double max_area = 0;
		for ( size_t j = lower[i]; j <= upper[i]; j++ )
		{
			xj[0] = j > 0 ? x[j - 1] : x[0];
			tj[0] = j > 0 ? t[j - 1] : t[0];
			xj[1] = x[j];
			tj[1] = t[j];
			xj[2] = j < n - 1 ? x[j + 1] : x[n - 1];
			tj[2] = j < n - 1 ? t[j + 1] : t[n - 1];
			area = 0;
			area += tj[0] * (xj[1] - xj[2]);
			area += tj[1] * (xj[2] - xj[0]);
			area += tj[2] * (xj[0] - xj[1]);
			area *= 0.5;
			area = std::fabs(area);
			if ( area > max_area ) {
				max_area = area;
				buffer[i] = j + ind1;
			}
		}
	}
}

template<typename Tx, typename Tt>
void sample_lttb(Tx * x, Tt * t, int n, int * lower, int * upper,
	int nbin, int * buffer, bool ind1 = false)
{
	double area;
	Tx xj[3];
	Tt tj[3];
	for ( size_t i = 0; i < nbin; i++ )
	{
		buffer[i] = lower[i] + ind1;
		// select left point from previous bucket
		if ( i == 0 ) {
			xj[0] = x[0];
			tj[0] = t[0];
		}
		else {
			xj[0] = x[buffer[i - 1] - ind1];
			tj[0] = t[buffer[i - 1] - ind1];
		}
		// select right point from average of next bucket
		if ( i == nbin - 1 ) {
			xj[2] = x[n - 1];
			tj[2] = t[n - 1];
		}
		else {
			xj[2] = 0;
			tj[2] = 0;
			for ( size_t j = lower[i + 1]; j <= upper[i + 1]; j++ )
			{
				xj[2] += x[j];
				tj[2] += t[j];
			}
			xj[2] /= (upper[i + 1] - lower[i + 1] + 1);
			tj[2] /= (upper[i + 1] - lower[i + 1] + 1);
		}
		// rank points in current bucket
		double max_area = 0;
		for ( size_t j = lower[i]; j <= upper[i]; j++ )
		{
			xj[1] = x[j];
			tj[1] = t[j];
			area = 0;
			area += tj[0] * (xj[1] - xj[2]);
			area += tj[1] * (xj[2] - xj[0]);
			area += tj[2] * (xj[0] - xj[1]);
			area *= 0.5;
			area = std::fabs(area);
			if ( area > max_area ) {
				max_area = area;
				buffer[i] = j + ind1;
			}
		}
	}
}

//// Continuum estimation
//-----------------------

// add functions here

//// Peak detection
//------------------

template<typename T>
size_t local_maxima(T * x, size_t n, int * buffer, int width = 5)
{
	int nmax = 0, a = 0, b = n, r = abs(width / 2);
	for ( int i = 0; i < n; i++ )
	{
		buffer[i] = false;
		if ( i < r || i > n - r )
			continue;
		a = (i - r) > 0 ? (i - r) : 0;
		b = (i + r) < n - 1 ? (i + r) : n - 1;
		for ( int j = a; j <= b; j++ )
		{
			buffer[i] = true;
			if ( j < i && x[j] >= x[i] )
			{
				buffer[i] = false;
				break;
			}
			if ( j > i && x[j] > x[i] )
			{
				buffer[i] = false;
				break;
			}
			nmax++;
		}
	}
	return nmax;
}

// find boundaries (local minima) of peaks
template<typename T>
void peak_boundaries(T * x, size_t n, int * peaks, size_t npeaks,
	int * left_buffer, int * right_buffer)
{
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		left_buffer[i] = peaks[i];
		// find left boundary
		for ( int j = peaks[i] - 1; j >= 0; j-- )
		{
			
			if ( x[j] > x[left_buffer[i]] )
			{
				int cand = left_buffer[i];
				int lwindow = (cand - 2) > 0 ? (cand - 2) : 0;
				j--;
				// check if candidate is local minimum
				while ( j >= lwindow )
				{
					if ( x[j] < x[cand] )
					{
						left_buffer[i] = j;
						break;
					}
					j--;
				}
				if ( cand == left_buffer[i] )
					break;
			}
			else if ( x[j] < x[left_buffer[i]] )
				left_buffer[i] = j;
		}
		right_buffer[i] = peaks[i];
		// find right boundary
		for ( int j = peaks[i] + 1; j < n; j++ )
		{
			if ( x[j] > x[right_buffer[i]] )
			{
				int cand = right_buffer[i];
				int rwindow = (cand + 2) < n - 1 ? (cand + 2) : n - 1;
				j++;
				// check if candidate is local minimum
				while ( j <= rwindow )
				{
					if ( x[j] < x[cand] )
					{
						right_buffer[i] = j;
						break;
					}
					j++;
				}
				if ( cand == right_buffer[i] )
					break;
			}
			else if ( x[j] < x[right_buffer[i]] )
				right_buffer[i] = j;
		}
	}
}

// find baselines of peaks (relative to higher peaks)
template<typename T>
void peak_bases(T * x, size_t n, int * peaks, size_t npeaks,
	int * left_buffer, int * right_buffer)
{
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		left_buffer[i] = peaks[i];
		// find left base of peak
		for ( int j = peaks[i] - 1; j >= 0; j-- )
		{
			if ( x[j] > x[peaks[i]] )
				break;
			if ( x[j] < x[left_buffer[i]] )
				left_buffer[i] = j;
		}
		right_buffer[i] = peaks[i];
		// find right base of peak
		for ( int j = peaks[i] + 1; j < n; j++ )
		{
			if ( x[j] > x[peaks[i]] )
				break;
			if ( x[j] < x[right_buffer[i]] )
				right_buffer[i] = j;
		}
	}
}

// find peak widths (where signal crosses cutoff heights)
template<typename Tx, typename Tt>
void peak_widths(Tx * x, Tt * t, size_t n, int * peaks, size_t npeaks,
	int * left_end, int * right_end, double * heights,
	double * left_buffer, double * right_buffer)
{
	double pt;
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		if ( left_end[i] < 0 || right_end[i] >= n )
			Rf_error("search limits out of range");
		// find where signal crosses height to left of peak
		for ( int j = peaks[i] - 1; j >= 0 && j >= left_end[i]; j-- )
		{
			if ( x[j] < heights[i] )
			{
				pt = (heights[i] - x[j]) / (x[j + 1] - x[j]);
				left_buffer[i] = t[j] + pt * (t[j + 1] - t[j]);
				break;
			}
			else
				left_buffer[i] = t[j];
		}
		// find where signal crosses height to right of peak
		for ( int j = peaks[i] + 1; j < n && j <= right_end[i]; j++ )
		{
			if ( x[j] < heights[i] )
			{
				pt = (heights[i] - x[j - 1]) / (x[j] - x[j - 1]);
				right_buffer[i] = t[j - 1] + pt * (t[j] - t[j - 1]);
				break;
			}
			else
				right_buffer[i] = t[j];
		}
	}
}

// find peak areas by numeric integration (trapezoid rule)
template<typename Tx, typename Tt>
void peak_areas(Tx * x, Tt * t, size_t n, int * peaks, size_t npeaks,
	int * left_end, int * right_end, double * buffer)
{
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		if ( left_end[i] > peaks[i] || peaks[i] > right_end[i] )
			Rf_error("peak index outside of search limits");
		if ( left_end[i] < 0 || right_end[i] >= n )
			Rf_error("search limits out of range");
		buffer[i] = trapz(t, x, left_end[i], right_end[i]);
	}
}

#endif // SIGNAL
