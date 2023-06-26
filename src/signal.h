#ifndef SIGNAL
#define SIGNAL

#include "matterDefines.h"
#include "coerce.h"
#include "search.h"

// interpolation scheme
// (must match factor levels)
#define EST_NEAR	1 	
#define EST_AVG		2
#define EST_SUM		3
#define EST_MAX		4
#define EST_MIN		5
#define EST_AREA	6  // Peak area
#define EST_LERP	7  // Linear interpolation
#define EST_CUBIC	8  // Cubic Hermite spline
#define EST_GAUS	9  // Gaussian filter
#define EST_SINC	10 // Lanczos

// binning scheme
#define BIN_SUM		1
#define BIN_AVG		2
#define BIN_MAX		3
#define BIN_MIN		4
#define BIN_SD		5
#define BIN_VAR		6
#define BIN_SSE		7

#define min2(x, y) ((x < y) ? (x) : (y))
#define min3(x, y, z) (min2(min2((x), (y)), (z)))
#define max2(x, y) ((x > y) ? (x) : (y))
#define max3(x, y, z) (max2(max2((x), (y)), (z)))
#define wrap_ind(i, n) (max2(min2((i), (n - 1)), 0))

//// Numeric Integration
//-----------------------

template<typename Tx, typename Ty>
double trapz(Tx * x, Ty * y, size_t lower, size_t upper)
{
	double sum = 0, dx;
	for ( size_t i = lower + 1; i <= upper; i++ )
	{
		dx = sdiff(x[i], x[i - 1]);
		sum += 0.5 * (y[i] + y[i - 1]) * dx;
	}
	return sum;
}

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
inline double chip(double y[4], double dx[3], double t)
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

//// Filtering and smoothing
//---------------------------

template<typename T>
void mean_filter(T * x, int n, int width, double * buffer)
{
	index_t ij, ik, r = width / 2;
	buffer[0] = r * x[0];
	for ( index_t i = 0; i <= r && i < n; i++ )
		buffer[0] += x[i];
	for ( index_t i = 1; i < n; i++ )
	{
		ij = wrap_ind(i - r - 1, n);
		ik = wrap_ind(i + r, n);
		buffer[i] = buffer[i - 1] - x[ij] + x[ik];
	}
	for ( index_t i = 0; i < n; i++ )
		buffer[i] /= width;
}

template<typename T>
void linear_filter(T * x, int n, T * weights, int width, double * buffer)
{
	index_t ij, r = width / 2;
	for ( index_t i = 0; i < n; i++ )
	{
		double W = 0;
		buffer[i] = 0;
		for (index_t j = 0; j < width; j++ )
		{
			ij = wrap_ind(i + j - r, n);
			buffer[i] += weights[j] * x[ij];
			W += weights[j];
		}
		buffer[i] /= W;
	}
}

template<typename T>
void bilateral_filter(T * x, int n, int width,
	double sddist, double sdrange, double spar, double * buffer)
{
	index_t ij, r = width / 2;
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
	for ( index_t i = 0; i < n; i++ )
	{
		double dmean = 0, W = 0;
		buffer[i] = 0;
		if ( !isNA(spar) )
		{
			// modified version of Joseph & Periyasamy (2018)
			for ( index_t j = 0; j < width; j++ )
			{
				// find mean of local differences
				ij = wrap_ind(i + j - r, n);
				dmean += std::fabs(x[ij] - x[i]) / width;
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
			buffer[i] = x[i];
			continue;
		}
		for ( index_t j = 0; j < width; j++ )
		{
			// standard bilateral filter
			ij = wrap_ind(i + j - r, n);
			double wtdist = kgaussian(j - r, sdd);
			double wtrange = kgaussian(x[ij] - x[i], sdr);
			buffer[i] += wtdist * wtrange * x[ij];
			W += wtdist * wtrange;
		}
		buffer[i] /= W;
	}
}

template<typename T>
void guided_filter(T * x, T * g, int n, int width,
	double sdreg, double ftol, double * buffer)
{
	double ug [n], ux [n], gmax;
	double tmp1 [n], tmp2 [n], tmp3 [n], tmp4 [n];
	// find maximum of guidance signal
	if ( !isNA(ftol) ) {
		gmax = n > 0 ? g[0] : NA_REAL;
		for ( index_t i = 1; i < n; i++ )
			if ( g[i] > gmax )
				gmax = g[i];
	}
	// calculate means
	mean_filter(g, n, width, ug);
	mean_filter(x, n, width, ux);
	// calculate variances and covariances
	double * gg = tmp1;
	double * gx = tmp2;
	for ( index_t i = 0; i < n; i++ ) {
		gg[i] = g[i] * g[i];
		gx[i] = g[i] * x[i];
	}
	double * sg = tmp3;
	double * sgx = tmp4;
	mean_filter(gg, n, width, sg);
	mean_filter(gx, n, width, sgx);
	for ( index_t i = 0; i < n; i++ ) {
		sg[i] = sg[i] - ug[i] * ug[i];
		sgx[i] = sgx[i] - ug[i] * ux[i];
	}
	// calculate coefficients a and b
	double * a = tmp1;
	double * b = tmp2;
	for ( index_t i = 0; i < n; i++ ) {
		double s0 = sdreg * sdreg;
		if ( !isNA(ftol) ) {
			// peak-aware regularization
			double k2 = (ftol * ftol) * (gmax * gmax);
			s0 *= std::exp(-(g[i] * g[i]) / k2);
		}
		a[i] = sgx[i] / (sg[i] + s0);
		b[i] = ux[i] - a[i] * ug[i];
	}
	double * ua = tmp3;
	double * ub = tmp4;
	mean_filter(a, n, width, ua);
	mean_filter(b, n, width, ub);
	// calculate output signal
	for ( index_t i = 0; i < n; i++ )
		buffer[i] = ua[i] * g[i] + ub[i];
}

//// Warping and alignment
//--------------------------

template<typename Tx, typename Tt>
void warp_dtw(Tx * x, Tx * y, Tt * tx, Tt * ty, int nx, int ny,
	int * i_buffer, int * j_buffer)
{
	// initialize output
	for ( index_t k = 0; k < nx + ny - 1; k++ )
	{
		i_buffer[k] = NA_INTEGER;
		j_buffer[k] = NA_INTEGER;
	}
	// fill cost matrix
	double * D = R_Calloc((nx + 1) * (ny + 1), double);
	for ( index_t i = 0; i <= nx; i++ )
	{
		for (index_t j = 0; j <= ny; j++ )
			D[j * nx + i] = R_PosInf;
	}
	D[0] = 0;
	double d, d00, d01, d10;
	for ( index_t i = 1; i <= nx; i++ )
	{
		for (index_t j = 1; j <= ny; j++ )
		{
			d = udiff(x[i - 1], y[j - 1]);
			d01 = D[j * nx + (i - 1)];
			d10 = D[(j - 1) * nx + i];
			d00 = D[(j - 1) * nx + (i - 1)];
			if ( d01 < d00 && d01 < d10 )
				D[j * nx + i] = (d * d) + d01;
			else if ( d10 < d00 && d10 < d01 )
				D[j * nx + i] = (d * d) + d10;
			else
				D[j * nx + i] = (d * d) + d00;
		}
	}
	// find optimal path
	index_t i = nx - 1, j = ny - 1, k = 0;
	while ( i >= 0 && j >= 0 && k < nx + ny - 1 )
	{
		i_buffer[k] = i;
		j_buffer[k] = j;
		d01 = D[(j + 1) * nx + i];
		d10 = D[j * nx + (i + 1)];
		d00 = D[j * nx + i];
		if ( d01 < d00 && d01 < d10 )
			i--;
		else if ( d10 < d00 && d10 < d01 )
			j--;
		else
		{
			i--;
			j--;
		}
		k++;
	}
	Free(D);
}

template<typename Tx, typename Tt>
void warp_dtwc(Tx * x, Tx * y, Tt * tx, Tt * ty, int nx, int ny,
	int * i_buffer, int * j_buffer, double tol, int tol_ref = ABS_DIFF)
{
	// check tolerance window
	if ( tol >= udiff(tx[0], ty[ny - 1], tol_ref) ||
		tol >= udiff(tx[nx - 1], ty[0], tol_ref) )
	{
		return warp_dtw(x, y, tx, ty, nx, ny, i_buffer, j_buffer);
	}
	tol = max2(tol, udiff(tx[nx - 1], ty[ny - 1], tol_ref));
	// initialize output
	for ( index_t k = 0; k < nx + ny - 1; k++ )
	{
		i_buffer[k] = NA_INTEGER;
		j_buffer[k] = NA_INTEGER;
	}
	int ptrD[nx + 1], wa[nx + 1], wb[nx + 1], nD;
	// find windows where |tx - ty| <= tol
	ptrD[0] = 0, wa[0] = 0, wb[0] = 1, nD = 1;
	for ( index_t i = 0; i < nx; i++ )
	{
		index_t j = binary_search(tx[i], ty,
			0, ny, tol, tol_ref, NA_INTEGER);
		wa[i + 1] = j;
		wb[i + 1] = j;
		if ( isNA(j) )
			Rf_error("tolerance window too small");
		for ( index_t k = j - 1; k >= 0; k-- ) {
			if ( udiff(tx[i], ty[k], tol_ref) > tol )
				break;
			wa[i + 1] = k;
		}
		for ( index_t k = j; k < ny; k++ ) {
			if ( udiff(tx[i], ty[k], tol_ref) > tol )
				break;
			wb[i + 1] = k + 1;
		}
		wa[i + 1]++;
		wb[i + 1]++;
		ptrD[i + 1] = nD;
		nD += wb[i + 1] - wa[i + 1];
	}
	// fill (sparse) cost matrix
	double * D = R_Calloc(nD, double);
	D[0] = 0;
	double d, d00, d01, d10, dmin;
	for ( index_t i = 1; i <= nx; i++ )
	{
		for (index_t j = wa[i]; j < wb[i]; j++ )
		{
			d = udiff(x[i - 1], y[j - 1]);
			d01 = R_PosInf, d10 = R_PosInf, d00 = R_PosInf;
			// D[i-1, j]
			if ( j >= wa[i - 1] && j < wb[i - 1] )
				d01 = D[ptrD[i - 1] + (j - wa[i - 1])];
			// D[i, j-1]
			if ( j - 1 >= wa[i] && j - 1 < wb[i] )
				d10 = D[ptrD[i] + (j - 1 - wa[i])];
			// D[i-1, j-1]
			if ( j - 1 >= wa[i - 1] && j - 1 < wb[i - 1] )
				d00 = D[ptrD[i - 1] + (j - 1 - wa[i - 1])];
			// min(D[i-1, j], D[i, j-1], D[i-1, j-1])
			dmin = min3(d01, d10, d00);
			D[ptrD[i] + (j - wa[i])] = (d * d) + dmin;
		}
	}
	// trace (constrained) optimal path
	index_t i = nx, j = ny, k = 0;
	while ( i > 0 && j > 0 && k < nx + ny - 1 )
	{
		i_buffer[k] = i - 1;
		j_buffer[k] = j - 1;
		d01 = R_PosInf, d10 = R_PosInf, d00 = R_PosInf;
		// D[i-1, j]
		if ( j >= wa[i - 1] && j < wb[i - 1] )
			d01 = D[ptrD[i - 1] + (j - wa[i - 1])];
		// D[i, j-1]
		if ( j - 1 >= wa[i] && j - 1 < wb[i] )
			d10 = D[ptrD[i] + (j - 1 - wa[i])];
		// D[i-1, j-1]
		if ( j - 1 >= wa[i - 1] && j - 1 < wb[i - 1] )
			d00 = D[ptrD[i - 1] + (j - 1 - wa[i - 1])];
		// trackback
		if ( d01 < d00 && d01 < d10 )
			i--;
		else if ( d10 < d00 && d10 < d01 )
			j--;
		else
		{
			i--;
			j--;
		}
		k++;
	}
	Free(D);
}

// correlation between x and y (w/ interpolation)
template<typename T>
double icor(T * x, T * y, size_t nx, size_t ny)
{
	if ( nx <= 1 || ny <= 1 )
		return 0;
	double xi [ny], ti0, ti1, tj, t;
	double Lx = nx - 1, Ly = ny - 1;
	for ( index_t i = 0, j = 0; i < nx - 1; i++ )
	{
		while ( (j / Ly) <= ((i + 1) / Lx) )
		{
			ti0 = i / Lx;
			ti1 = (i + 1) / Lx;
			tj = j / Ly;
			t = (tj - ti0) / (ti1 - ti0);
			xi[j] = lerp(x[i], x[i + 1], t);
			j++;
		}
	}
	double ux = 0, uy = 0;
	for ( index_t i = 0; i < ny; i++ )
	{
		ux += xi[i];
		uy += y[i];
	}
	ux /= ny;
	uy /= ny;
	double Sxx = 0, Syy = 0, Sxy = 0;
	for ( index_t i = 0; i < ny; i++ )
	{
		Sxx += (ux - xi[i]) * (ux - xi[i]);
		Syy += (uy - y[i]) * (uy - y[i]);
		Sxy += (ux - xi[i]) * (uy - y[i]);
	}
	return Sxy / std::sqrt(Sxx * Syy);
}

template<typename Tx, typename Tt>
void warp_cow(Tx * x, Tx * y, Tt * tx, Tt * ty, int nx, int ny,
	int * x_nodes, int * y_nodes, int n, double tol, int tol_ref = ABS_DIFF)
{
	// find node candidates where |tx - ty| <= tol
	if ( n < 3 )
		Rf_error("need at least 3 nodes");
	int ptrW [n], wa [n], wb [n], nW;
	ptrW[0] = 0, ptrW[1] = 1, nW = 1;
	wa[0] = 0, wb[0] = 1;
	wa[n - 1] = nx - 1, wb[n - 1] = nx;
	for ( index_t i = 1; i < n - 1; i++ )
	{
		Tt tyi = ty[y_nodes[i]];
		index_t j = x_nodes[i];
		wa[i] = j;
		wb[i] = j;
		for ( index_t k = j - 1; k >= 0; k-- ) {
			if ( udiff(tx[k], tyi, tol_ref) > tol )
				break;
			wa[i] = k;
		}
		for ( index_t k = j; k < nx; k++ ) {
			if ( udiff(tx[k], tyi, tol_ref) > tol )
				break;
			wb[i] = k + 1;
		}
		nW += wb[i] - wa[i];
		ptrW[i + 1] = nW;
	}
	nW++;
	// fill (sparse) benefit/warp matrices
	int * W = R_Calloc(nW, int);
	double * F = R_Calloc(nW, double);
	W[0] = x_nodes[0];
	W[nW - 1] = x_nodes[n - 1];
	F[nW - 1] = 0;
	// loop through nodes
	for ( index_t i = n - 2; i >= 0; i-- )
	{
		Tx * yw = y + y_nodes[i];
		size_t nyw = y_nodes[i + 1] - y_nodes[i];
		// loop through candidate node positions
		for (index_t j = wa[i]; j < wb[i]; j++ )
		{
			index_t jj = ptrW[i] + (j - wa[i]);
			F[jj] = R_NegInf;
			// find optimal warp given previous/next node
			for ( index_t k = wa[i + 1]; k < wb[i + 1]; k++ )
			{
				if ( k - j < 3 )
					continue;
				index_t kk = ptrW[i + 1] + (k - wa[i + 1]);
				double f = icor(x + j, yw, k - j, nyw);
				if ( f + F[kk] > F[jj] )
				{
					F[jj] = f + F[kk];
					W[jj] = k;
				}
			}
		}
	}
	// trace correlation-optimized path
	for ( index_t i = 0; i < n - 1; i++ )
	{
		index_t j = (x_nodes[i] - wa[i]);
		x_nodes[i + 1] = W[ptrW[i] + j];
	}
	Free(W);
	Free(F);
}

//// Binning and downsampling
//----------------------------

inline void bin_update(double * score, int * lower, int * upper,
	int nbin, int * lower_buffer, int * upper_buffer)
{
	int merge = 0, split = 0;
	double merge_score = R_PosInf;
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
void downsample_ltob(Tx * x, Tt * t, int n, int * lower, int * upper,
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
void downsample_lttb(Tx * x, Tt * t, int n, int * lower, int * upper,
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

// cross > 0 is ccw; cross < 0 is cw; 0 is collinear
template<typename T>
T cross(T ox, T oy, T ax, T ay, T bx, T by)
{
	return (ax - ox) * (by - oy) - (ay - oy) * (bx - ox);
}

// monotonic chain algorithm by A. M. Andrew (1979)
template<typename T>
size_t convex_hull(T * x, T * y, size_t n, int * buffer, bool upper = false)
{
	size_t h = 0;
	int * b = buffer;
	if ( upper )
	{
		for ( index_t i = n - 1; i >= 0; i-- )
		{
			while ( h >= 2 && cross(x[b[h - 2]], y[b[h - 2]],
				x[b[h - 1]], y[b[h - 1]], x[i], y[i]) <= 0 )
			{
				h--;
			}
			b[h] = i;
			h++;
		}
	}
	else
	{
		for ( index_t i = 0; i < n; i++ )
		{
			while ( h >= 2 && cross(x[b[h - 2]], y[b[h - 2]],
				x[b[h - 1]], y[b[h - 1]], x[i], y[i]) <= 0 )
			{
				h--;
			}
			b[h] = i;
			h++;
		}
	}
	return h;
}

// SNIP with adaptive clipping window by M. Morhac (2009)
template<typename T>
void smooth_snip(T * x, size_t n, T * buffer, int m, bool decreasing = true)
{
	T a1, a2;
	T * y = buffer;
	std::memcpy(y, x, n * sizeof(T));
	T z [n];
	if ( decreasing )
	{
		for ( size_t p = m; p >= 1; p-- )
		{
			for ( size_t i = p; i < n - p; i++ )
			{
				a1 = y[i];
				a2 = (y[i - p] + y[i + p]) / 2;
				z[i] = a1 < a2 ? a1 : a2;
			}
			for ( size_t i = p; i < n - p; i++ )
				y[i] = z[i];
		}
	}
	else
	{
		for ( size_t p = 1; p <= m; p++ )
		{
			for ( size_t i = p; i < n - p; i++ )
			{
				a1 = y[i];
				a2 = (y[i - p] + y[i + p]) / 2;
				z[i] = a1 < a2 ? a1 : a2;
			}
			for ( size_t i = p; i < n - p; i++ )
				y[i] = z[i];
		}
	}
}

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

// find left boundary of a peak
template<typename T>
index_t peak_lbound(T * x, index_t peak, size_t n)
{
	index_t lbound = peak;
	// account for mis-centered peaks
	bool is_left_of_peak = false;
	// find left boundary of peak (first local minimum)
	for ( index_t i = peak - 1; i >= 0; i-- )
	{
		if ( x[i] < x[lbound] )
		{
			// update left boundary
			lbound = i;
			is_left_of_peak = true;
		}
		else if ( x[i] > x[lbound] && is_left_of_peak )
		{
			index_t cand = lbound;
			index_t lwindow = (cand - 2) > 0 ? (cand - 2) : 0;
			i--;
			// check if candidate is local minimum
			while ( i >= lwindow )
			{
				if ( x[i] < x[cand] )
				{
					lbound = i;
					break;
				}
				i--;
			}
			if ( cand == lbound )
				break;
		}
	}
	return lbound;
}

// find right boundary of a peak
template<typename T>
index_t peak_rbound(T * x, index_t peak, size_t n)
{
	index_t rbound = peak;
	// account for mis-centered peaks
	bool is_right_of_peak = false;
	// find right boundary of peak (first local minimum)
	for ( index_t i = peak + 1; i < n; i++ )
	{
		if ( x[i] < x[rbound] )
		{
			// update right boundary
			rbound = i;
			is_right_of_peak = true;
		}
		else if ( x[i] > x[rbound] && is_right_of_peak )
		{
			index_t cand = rbound;
			index_t rwindow = (cand + 2) < n - 1 ? (cand + 2) : n - 1;
			i++;
			// check if candidate is local minimum
			while ( i <= rwindow )
			{
				if ( x[i] < x[cand] )
				{
					rbound = i;
					break;
				}
				i++;
			}
			if ( cand == rbound )
				break;
		}
	}
	return rbound;
}

// find boundaries (nearest local minima) of peaks
template<typename T>
void peak_boundaries(T * x, size_t n, int * peaks, size_t npeaks,
	int * left_buffer, int * right_buffer)
{
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		left_buffer[i] = peak_lbound(x, peaks[i], n);
		right_buffer[i] = peak_rbound(x, peaks[i], n);
	}
}

// find baselines of peaks (minima from next higher peaks)
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
	int * left_limit, int * right_limit, double * heights,
	double * left_buffer, double * right_buffer)
{
	double pt;
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		if ( left_limit[i] < 0 || right_limit[i] >= n )
			Rf_error("search limits out of range");
		// find where signal crosses height to left of peak
		for ( int j = peaks[i] - 1; j >= 0 && j >= left_limit[i]; j-- )
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
		for ( int j = peaks[i] + 1; j < n && j <= right_limit[i]; j++ )
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
	int * left_limit, int * right_limit, double * buffer)
{
	for ( int i = 0; i < npeaks; i++ )
	{
		if ( peaks[i] < 0 || peaks[i] >= n )
			Rf_error("peak index out of range");
		if ( left_limit[i] > peaks[i] || peaks[i] > right_limit[i] )
			Rf_error("peak index outside of integration limits");
		if ( left_limit[i] < 0 || right_limit[i] >= n )
			Rf_error("integration limits out of range");
		buffer[i] = trapz(t, x, left_limit[i], right_limit[i]);
	}
}

//// Resampling with interpolation
//---------------------------------

// linear interpolation in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_linear(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	double y0, y1, t;
	if ( sdiff(x[i], xi, tol_ref) < 0 ) {
		// x[i] < xi < x[i + 1]
		y0 = static_cast<double>(y[i]);
		if ( i + 1 < n && udiff(x[i + 1], xi, tol_ref) <= tol ) {
			y1 = static_cast<double>(y[i + 1]);
			t = sdiff(xi, x[i]) / sdiff(x[i + 1], x[i]);
		}
		else
			return y0;
	}
	else {
		// x[i - 1] < xi < x[i]
		y1 = static_cast<double>(y[i]);
		if ( i - 1 >= 0 && udiff(x[i - 1], xi, tol_ref) <= tol ) {
			y0 = static_cast<double>(y[i - 1]);
			t = sdiff(xi, x[i - 1]) / sdiff(x[i], x[i - 1]);
		}
		else
			return y1;
	}
	return lerp(y0, y1, t);
}

// cubic interpolation in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_cubic(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	index_t p[] = {NA_INTEGER, NA_INTEGER, NA_INTEGER, NA_INTEGER};
	double t, ys[4], dx[3];
	double d = sdiff(x[i], xi, tol_ref);
	if ( d < 0 ) {
		// x[i] < xi < x[i + 1]
		p[1] = i;
		if ( i + 1 < n && udiff(x[i + 1], xi, tol_ref) <= tol )
			p[2] = i + 1;
		else
			return y[i];
		if ( i + 2 < n && udiff(x[i + 2], xi, tol_ref) <= tol )
			p[3] = i + 2;
		if ( i - 1 >= 0 && udiff(x[i - 1], xi, tol_ref) <= tol )
			p[0] = i - 1;
	}
	else if ( d > 0 ) {
		// x[i - 1] < xi < x[i]
		p[2] = i;
		if ( i - 1 >= 0 && udiff(x[i - 1], xi, tol_ref) <= tol )
			p[1] = i - 1;
		else
			return y[i];
		if ( i - 2 >= 0 && udiff(x[i - 2], xi, tol_ref) <= tol )
			p[0] = i - 2;
		if ( i + 1 < n && udiff(x[i + 1], xi, tol_ref) <= tol )
			p[3] = i + 1;
	}
	else {
		return y[i];
	}
	if ( isNA(p[0]) )
		p[0] = p[1];
	if ( isNA(p[3]) )
		p[3] = p[2];
	ys[0] = static_cast<Ty>(y[p[0]]);
	ys[1] = static_cast<Ty>(y[p[1]]);
	ys[2] = static_cast<Ty>(y[p[2]]);
	ys[3] = static_cast<Ty>(y[p[3]]);
	dx[0] = sdiff(x[p[1]], x[p[0]]);
	dx[1] = sdiff(x[p[2]], x[p[1]]);
	dx[2] = sdiff(x[p[3]], x[p[2]]);
	t = sdiff(xi, x[p[1]]) / sdiff(x[p[2]], x[p[1]]);
	return chip(ys, dx, t);
}

// mean in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_mean(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = 0;
	size_t ni = 0;
	for ( index_t j = i; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi += y[j];
		ni++;
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi += y[j];
		ni++;
	}
	return yi / ni;
}

// sum in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_sum(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = 0;
	for ( index_t j = i; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi += y[j];
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi += y[j];
	}
	return yi;
}

// maximum in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_max(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = y[i];
	for ( index_t j = i + 1; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi = y[j] > yi ? y[j] : yi;
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi = y[j] > yi ? y[j] : yi;
	}
	return yi;
}

// minimum in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_min(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = y[i];
	for ( index_t j = i + 1; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi = y[j] < yi ? y[j] : yi;
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		yi = y[j] < yi ? y[j] : yi;
	}
	return yi;
}

// gaussian kernel in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_gaussian(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double sd, double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = 0;
	double K = 0, ki;
	for ( index_t j = i; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		ki = kgaussian(sdiff(x[j], xi), sd);
		yi += ki * y[j];
		K += ki;
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		ki = kgaussian(sdiff(x[j], xi), sd);
		yi += ki * y[j];
		K += ki;
	}
	return yi / K;
}

// lanczos kernel in interval defined by |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1_lanczos(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double a, double tol, int tol_ref = ABS_DIFF)
{
	Ty yi = 0;
	double K = 0, ki;
	for ( index_t j = i; j < n; j++ ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		ki = klanczos(sdiff(x[j], xi), a);
		yi += ki * y[j];
		K += ki;
	}
	for ( index_t j = i - 1; j >= 0; j-- ) {
		if ( udiff(x[j], xi, tol_ref) > tol )
			break;
		ki = klanczos(sdiff(x[j], xi), a);
		yi += ki * y[j];
		K += ki;
	}
	return yi / K;
}

// interpolate in interval |x[i] - xi| <= tol
template<typename Tx, typename Ty>
Ty interp1(Tx xi, Tx * x, Ty * y, index_t i, size_t n,
	double tol, int tol_ref, int interp = EST_NEAR)
{
	switch(interp)
	{
		case EST_NEAR:
			if ( sdiff(xi, x[i], tol_ref) <= tol )
				return y[i];
			else
				return NA<Ty>();
		case EST_AVG:
			return interp1_mean(xi, x, y, i, n, tol, tol_ref);
		case EST_SUM:
			return interp1_sum(xi, x, y, i, n, tol, tol_ref);
		case EST_MAX:
			return interp1_max(xi, x, y, i, n, tol, tol_ref);
		case EST_MIN:
			return interp1_min(xi, x, y, i, n, tol, tol_ref);
		case EST_AREA: {
			index_t lbound = peak_lbound(y, i, n);
			index_t rbound = peak_rbound(y, i, n);
			return trapz(x, y, lbound, rbound);
		}
		case EST_LERP:
			return interp1_linear(xi, x, y, i, n, tol, tol_ref);
		case EST_CUBIC:
			return interp1_cubic(xi, x, y, i, n, tol, tol_ref);
		case EST_GAUS: {
			double xf = coerce_cast<double>(xi);
			double sd = (tol_ref == ABS_DIFF) ? (tol / 2) : (xf * tol / 2);
			return interp1_gaussian(xi, x, y, i, n, sd, tol, tol_ref);
		}
		case EST_SINC: {
			double xf = coerce_cast<double>(xi);
			double a = (tol_ref == ABS_DIFF) ? tol : xf * tol;
			return interp1_lanczos(xi, x, y, i, n, a, tol, tol_ref);
		}
		default:
			return NA<Ty>();
	}
}

// approximate y ~ x at xi with interpolation
template<typename Tx, typename Ty>
Ty approx1(Tx xi, Tx * x, Ty * y, size_t start, size_t end,
	double tol, int tol_ref, Ty nomatch, int interp = EST_NEAR)
{
	if ( isNA(xi) )
		return NA<Ty>();
	index_t i = NA_INTEGER;
	Ty yi = nomatch;
	i = binary_search(xi, x, start, end,
		tol, tol_ref, NA_INTEGER);
	if ( !isNA(i) && i >= 0 )
	{
		if ( tol > 0 && interp != EST_NEAR )
			yi = interp1(xi, x, y, i, end,
				tol, tol_ref, interp);
		else
			yi = y[i];
	}
	return yi;
}

// approximate y ~ x at xi with interpolation
template<typename Tx, typename Ty>
index_t do_approx1(Ty * ptr, Tx * xi, size_t ni, Tx * x, Ty * y,
	size_t start, size_t end, double tol, int tol_ref, Ty nomatch,
	int interp = EST_NEAR, int stride = 1)
{
	// initialize ptr
	bool processed[ni];
	for ( size_t i = 0; i < ni; i++ )
	{
		if ( isNA(xi[i]) )
		{
			ptr[i * stride] = NA<Ty>();
			processed[i] = true;
		}
		else
		{
			ptr[i * stride] = nomatch;
			processed[i] = false;
		}
	}
	if ( start >= end )
		return 0;
	index_t num_matches = 0;
	Tx * xs = x;
	Ty * ys = y;
	// check if x (and therefore y) are unsorted
	bool need_sort = !is_sorted(x + start, end - start);
	if ( need_sort )
	{
		xs = R_Calloc(end, Tx);
		ys = R_Calloc(end, Ty);
		std::memcpy(xs, x, end * sizeof(Tx));
		std::memcpy(ys, y, end * sizeof(Ty));
		quick_sort(xs, start, end, ys);
	}
	// do the resampling
	if ( ni < (end - start) || !is_sorted(xi, ni) )
	{
		// if len(xi) << len(x) then iterate xi (downsampling)
		for ( size_t i = 0; i < ni; i++ )
		{
			if ( isNA(xi[i]) )
				continue;
			Ty yi = approx1(xi[i], xs, ys, start, end,
				tol, tol_ref, NA<Ty>(), interp);
			if ( !isNA(yi) )
			{
				num_matches++;
				ptr[i * stride] = yi;
			}
			processed[i] = true;
		}
	}
	else
	{
		// if len(xi) >> len(x) then iterate x (upsampling)
		int indx[end], new_ref;
		switch(tol_ref) {
			case REL_DIFF_X:
				new_ref = REL_DIFF_Y;
			case REL_DIFF_Y:
				new_ref = REL_DIFF_X;
			default:
				new_ref = tol_ref;
		}
		// find initial search positions in xi
		do_binary_search(indx, xs, end - start, xi, 0, ni,
			tol, new_ref, NA_INTEGER);
		for ( size_t j = start; j < end; j++ )
		{
			if ( isNA(indx[j]) )
				continue;
			// iterate to left along xi
			for ( index_t i = indx[j]; i < ni; i++ )
			{
				if ( processed[i] )
					break;
				if ( udiff(xi[i], xs[j], tol_ref) > tol )
					break;
				Ty yi = approx1(xi[i], xs, ys, j, end,
					tol, tol_ref, NA<Ty>(), interp);
				if ( !isNA(yi) )
				{
					num_matches++;
					ptr[i * stride] = yi;
				}
				processed[i] = true;
			}
			// iterate to right along xi
			for ( index_t i = indx[j] - 1; i >= 0; i-- )
			{
				if ( processed[i] )
					break;
				if ( udiff(xi[i], xs[j], tol_ref) > tol )
					break;
				Ty yi = approx1(xi[i], xs, ys, j, end,
					tol, tol_ref, NA<Ty>(), interp);
				if ( !isNA(yi) )
				{
					num_matches++;
					ptr[i * stride] = yi;
				}
				processed[i] = true;
			}
		}
	}
	if ( need_sort )
	{
		Free(xs);
		Free(ys);
	}
	return num_matches;
}

#endif // SIGNAL
