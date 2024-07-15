#ifndef SIGNAL2
#define SIGNAL2

#include "matterDefines.h"
#include "search.h"
#include "signal.h"

//// Summarize via statistic 
//---------------------------

template<typename T>
size_t do_len_at(T * x, int * indx, size_t n)
{
	size_t len = 0;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( !isNA(x[indx[i]]) )
			len++;
	}
	return len;
}

template<typename T>
double do_sum_at(T * x, int * indx, size_t n)
{
	double sx = 0;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( !isNA(x[indx[i]]) )
			sx += x[indx[i]];
	}
	return coerce_cast<double>(sx);
}

template<typename T>
double do_mean_at(T * x, int * indx, size_t n)
{
	double sx = do_sum_at(x, indx, n);
	return sx / do_len_at(x, indx, n);
}

template<typename T>
double do_max_at(T * x, int * indx, size_t n)
{
	if ( n <= 0 )
		return NA_REAL;
	T mx = x[indx[0]];
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(x[indx[i]]) )
			continue;
		if ( x[indx[i]] > mx || isNA(mx) )
			mx = x[indx[i]];
	}
	return coerce_cast<double>(mx);
}

template<typename T>
double do_min_at(T * x, int * indx, size_t n)
{
	if ( n <= 0 )
		return NA_REAL;
	T mx = x[indx[0]];
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(x[indx[i]]) )
			continue;
		if ( x[indx[i]] < mx || isNA(mx) )
			mx = x[indx[i]];
	}
	return coerce_cast<double>(mx);
}

//// Summarize via kernel 
//-------------------------

template<typename Txy, typename Tz>
double do_klinear2(Txy xi, Txy yi, Txy * x, Txy * y, Tz * z,
	double rx, double ry, int * indx, size_t n)
{
	double zi = 0, K0 = 0, ki, kj;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(z[indx[i]]) )
			continue;
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
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(z[indx[i]]) )
			continue;
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
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(z[indx[i]]) )
			continue;
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
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(z[indx[i]]) )
			continue;
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
	index_t ii, jj, prev, next;
	double * y = R_Calloc(nr * nc, double);
	double * z = buffer;
	// horizontal filter pass
	for ( index_t i = 0; i < nr; i++ )
	{
		for ( index_t j = 0; j < nc; j++ )
		{
			prev = norm_ind(j - r - 1, nc);
			next = norm_ind(j + r, nc);
			if ( isNA(x[j * nr + i]) )
			{
				// handle missing pixel
				y[j * nr + i] = NA_REAL;
			}
			else if ( j == 0 || isNA(y[(j - 1) * nr + i]) ||
				isNA(x[prev * nr + i]) || isNA(x[next * nr + i]) )
			{
				// handle missing neighborhood
				double xs = 0;
				size_t len = 0;
				for ( index_t k = -r; k <= r; k++ )
				{
					jj = norm_ind(j + k, nc);
					if ( !isNA(x[jj * nr + i]) )
					{
						xs += x[jj * nr + i];
						len++;
					}
				}
				y[j * nr + i] = width * (xs / len);
			}
			else
			{
				// fast O(n) sliding sum
				double xprev = x[prev * nr + i];
				double xnext = x[next * nr + i];
				y[j * nr + i] = y[(j - 1) * nr + i] - xprev + xnext;
			}
		}
		// calculate means
		for ( index_t j = 0; j < nc; j++ )
		{
			if ( !isNA(y[j * nr + i]) )
				y[j * nr + i] /= width;
		}
	}
	// vertical filter pass
	for ( index_t j = 0; j < nc; j++ )
	{
		for ( index_t i = 0; i < nr; i++ )
		{
			prev = norm_ind(i - r - 1, nr);
			next = norm_ind(i + r, nr);
			if ( isNA(y[j * nr + i]) )
			{
				// handle missing pixel
				z[j * nr + i] = NA_REAL;
			}
			else if ( i == 0 || isNA(z[j * nr + i - 1]) ||
				isNA(y[j * nr + prev]) || isNA(y[j * nr + next]) )
			{
				// handle missing neighborhood
				double ys = 0;
				size_t len = 0;
				for ( index_t k = -r; k <= r; k++ )
				{
					ii = norm_ind(i + k, nr);
					if ( !isNA(y[j * nr + ii]) )
					{
						ys += y[j * nr + ii];
						len++;
					}
				}
				z[j * nr + i] = width * (ys / len);
			}
			else
			{
				// fast O(n) sliding sum
				double yprev = y[j * nr + prev];
				double ynext = y[j * nr + next];
				z[j * nr + i] = z[j * nr + i - 1] - yprev + ynext;
			}
		}
		// calculate means
		for ( index_t i = 0; i < nr; i++ )
		{
			if ( !isNA(z[j * nr + i]) )
				z[j * nr + i] /= width;
		}
	}
	Free(y);
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
			if ( isNA(x[j * nr + i]) )
			{
				buffer[j * nr + i] = NA_REAL;
				continue;
			}
			double xij, wij, W = 0;
			buffer[j * nr + i] = 0;
			for (index_t ki = -r; ki <= r; ki++ )
			{
				for (index_t kj = -r; kj <= r; kj++ )
				{
					ii = norm_ind(i + ki, nr);
					jj = norm_ind(j + kj, nc);
					if ( isNA(x[jj * nr + ii]) )
						continue;
					xij = x[jj * nr + ii];
					wij = weights[(kj + r) * width + (ki + r)];
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
	size_t n = nr * nc;
	index_t ii, jj;
	double sdd = sddist, sdr = sdrange;
	double xmedian, xmad, xrange;
	double D = std::sqrt(r * r + r * r);
	if ( !isNA(spar) )
	{
		// get MAD if using adaptive parameters
		xmedian = quick_median(x, n);
		xmad = quick_mad(x, n);
		double xmin = do_min(x, 0, n - 1);
		double xmax = do_max(x, 0, n - 1);
		xrange = xmax - xmin;
	}
	for ( index_t i = 0; i < nr; i++ )
	{
		for ( index_t j = 0; j < nc; j++ )
		{
			if ( isNA(x[j * nr + i]) )
			{
				buffer[j * nr + i] = NA_REAL;
				continue;
			}
			double xij, dx = 0, W = 0;
			buffer[j * nr + i] = 0;
			if ( !isNA(spar) )
			{
				// modified version of Joseph & Periyasamy (2018)
				for ( index_t ki = -r; ki <= r; ki++ )
				{
					for ( index_t kj = -r; kj <= r; kj++ )
					{
						// find mean of local differences
						ii = norm_ind(i + ki, nr);
						jj = norm_ind(j + kj, nc);
						if ( !isNA(x[jj * nr + ii]) )
						{
							xij = x[jj * nr + ii];
							dx += std::fabs(xij - xmedian);
						}
					}
				}
				dx /= (width * width);
				// calculate adaptive parameters
				double z = std::fabs(dx - xmad) / spar;
				if ( isNA(sddist) )
					sdd = D * std::exp(-z) / std::sqrt(2);
				if ( isNA(sdrange) )
					sdr = xrange * std::exp(-z) / std::sqrt(2);
			}
			if ( sdd <= DBL_EPSILON || sdr <= DBL_EPSILON )
			{
				// avoid singularities
				buffer[j * nr + i] = x[j * nr + i];
				continue;
			}
			for ( index_t ki = -r; ki <= r; ki++ )
			{
				for ( index_t kj = -r; kj <= r; kj++ )
				{
					// standard bilateral filter
					ii = norm_ind(i + ki, nr);
					jj = norm_ind(j + kj, nc);
					if ( isNA(x[jj * nr + ii]) )
						continue;
					xij = x[jj * nr + ii];
					double wtdist = kgaussian(ki, sdd) * kgaussian(kj, sdd);
					double wtrange = kgaussian(xij - x[j * nr + i], sdr);
					buffer[j * nr + i] += wtdist * wtrange * xij;
					W += wtdist * wtrange;
				}
			}
			if ( !isNA(buffer[j * nr + i]) )
				buffer[j * nr + i] /= W;
		}
	}
}

template<typename T>
void diffusion_filter2(T * x, int nr, int nc, int niter,
	double K, double rate, int method, double * buffer)
{
	size_t n = nr * nc;
	index_t N, S, E, W;
	double dN, dS, dE, dW, cN, cS, cE, cW, dx;
	double * tmp = R_Calloc(n, double);
	double * x0 = tmp;
	double * x1 = buffer;
	// initialize buffer
	for ( index_t i = 0; i < n; i++ )
		buffer[i] = coerce_cast<double>(x[i]);
	// iterate
	for ( int iter = 0; iter < niter; iter++ )
	{
		std::memcpy(x0, x1, n * sizeof(double));
		for ( index_t i = 0; i < nr; i++ )
		{
			for ( index_t j = 0; j < nc; j++ )
			{
				if ( isNA(x0[j * nr + i]) )
				{
					x1[j * nr + i] = NA_REAL;
					continue;
				}
				// calculate gradients
				N = norm_ind(i - 1, nr);
				S = norm_ind(i + 1, nr);
				E = norm_ind(j + 1, nc);
				W = norm_ind(j - 1, nc);
				dN = isNA(x0[j * nr + N]) ? 0 : sdiff(x0[j * nr + N], x0[j * nr + i]);
				dS = isNA(x0[j * nr + S]) ? 0 : sdiff(x0[j * nr + S], x0[j * nr + i]);
				dE = isNA(x0[E * nr + i]) ? 0 : sdiff(x0[E * nr + i], x0[j * nr + i]);
				dW = isNA(x0[W * nr + i]) ? 0 : sdiff(x0[W * nr + i], x0[j * nr + i]);
				// calculate conduction
				switch(method) {
					case DIFF_EQ1:
					{
						cN = std::exp(-(dN / K) * (dN / K));
						cS = std::exp(-(dS / K) * (dS / K));
						cE = std::exp(-(dE / K) * (dE / K));
						cW = std::exp(-(dW / K) * (dN / K));
						break;
					}
					case DIFF_EQ2:
					{
						cN = 1 / (1 + (dN / K) * (dN / K));
						cS = 1 / (1 + (dS / K) * (dS / K));
						cE = 1 / (1 + (dE / K) * (dE / K));
						cW = 1 / (1 + (dW / K) * (dN / K));
						break;
					}
					default:
						Rf_error("unrecognized diffusivity");
				}
				// update image
				dx = (cN * dN) + (cS * dS) + (cE * dE) + (cW * dW);
				x1[j * nr + i] = x0[j * nr + i] + rate * dx;
			}
		}
	}
	Free(tmp);
}

template<typename T>
void guided_filter2(T * x, T * g, int nr, int nc, int width,
	double sdreg, double * buffer)
{
	size_t n = nr * nc;
	// allocate buffers for mean filter results
	double * u = R_Calloc(2 * n, double);
	double * ug = u;
	double * ux = u + n;
	// allocate buffers for intermediate results
	double * tmp = R_Calloc(4 * n, double);
	double * ptr1 = tmp;
	double * ptr2 = tmp + n;
	double * ptr3 = tmp + 2 * n;
	double * ptr4 = tmp + 3 * n;
	// calculate means
	mean_filter2(g, nr, nc, width, ug);
	mean_filter2(x, nr, nc, width, ux);
	// calculate variances and covariances
	double * gg = ptr1;
	double * gx = ptr2;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(g[i]) || isNA(x[i]) )
		{
			gg[i] = NA_REAL;
			gx[i] = NA_REAL;
		}
		else
		{
			gg[i] = g[i] * g[i];
			gx[i] = g[i] * x[i];
		}
	}
	double * sg = ptr3;
	double * sgx = ptr4;
	mean_filter2(gg, nr, nc, width, sg);
	mean_filter2(gx, nr, nc, width, sgx);
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(g[i]) || isNA(x[i]) )
		{
			sg[i] = NA_REAL;
			sgx[i] = NA_REAL;
		}
		else
		{
			sg[i] = sg[i] - ug[i] * ug[i];
			sgx[i] = sgx[i] - ug[i] * ux[i];
		}
	}
	// calculate coefficients a and b
	double * a = ptr1;
	double * b = ptr2;
	double s0 = sdreg * sdreg;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(g[i]) || isNA(x[i]) )
		{
			a[i] = NA_REAL;
			b[i] = NA_REAL;
		}
		else
		{
			a[i] = sgx[i] / (sg[i] + s0);
			b[i] = ux[i] - a[i] * ug[i];
		}
	}
	double * ua = ptr3;
	double * ub = ptr4;
	mean_filter2(a, nr, nc, width, ua);
	mean_filter2(b, nr, nc, width, ub);
	// calculate output signal
	for ( size_t i = 0; i < n; i++ )
		buffer[i] = ua[i] * g[i] + ub[i];
	Free(tmp);
	Free(u);
}

//// Contrast enhancement
//------------------------

template<typename T>
void histeq(T * x, size_t n, int nbins, double * buffer)
{
	index_t * v = R_Calloc(n, index_t);
	int * hist = R_Calloc(nbins, int);
	int * ecdf = R_Calloc(nbins, int);
	T xmin = do_min(x, 0, n - 1);
	T xmax = do_max(x, 0, n - 1);
	// scale x to count of bins and store in v
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(x[i]) )
		{
			v[i] = NA_INTEGER;
			continue;
		}
		double a = static_cast<double>(x[i] - xmin);
		double b = static_cast<double>(xmax - xmin);
		v[i] = std::lrint((nbins - 1) * (a / b));
		// just for safety (we index with these)
		v[i] = norm_ind(v[i], nbins);
	}
	// calculate histogram
	for ( index_t i = 0; i < nbins; i++ )
		hist[i] = 0;
	for ( size_t i = 0; i < n; i++ )
	{
		if ( !isNA(v[i]) )
			hist[v[i]]++;
	}
	// calculate empirical cdf
	for ( index_t i = 0; i < nbins; i++ )
	{
		if ( i == 0 )
			ecdf[i] = hist[i];
		else
			ecdf[i] = ecdf[i - 1] + hist[i];
	}
	// transform pixels
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(v[i]) )
			buffer[i] = NA_REAL;
		else
			buffer[i] = ecdf[v[i]];
	}
	// normalize to [0, 1]
	double vmax = do_max(buffer, 0, n - 1);
	for ( size_t i = 0; i < n; i++ )
	{
		if ( !isNA(buffer[i]) )
			buffer[i] /= vmax;
	}
	Free(hist);
	Free(ecdf);
	Free(v);
}

template<typename T>
void adapt_histeq(T * x, int nr, int nc, int width,
	double clip, int nbins, double * buffer)
{
	int r = width / 2;
	size_t n = nr * nc;
	index_t * v = R_Calloc(n, index_t);
	int * hist = R_Calloc(nbins, int);
	int * clhist = R_Calloc(nbins, int);
	T xmin = do_min(x, 0, n - 1);
	T xmax = do_max(x, 0, n - 1);
	// scale x to count of bins and store in v
	for ( size_t i = 0; i < n; i++ )
	{
		if ( isNA(x[i]) )
		{
			v[i] = NA_INTEGER;
			continue;
		}
		double a = static_cast<double>(x[i] - xmin);
		double b = static_cast<double>(xmax - xmin);
		v[i] = std::lrint((nbins - 1) * (a / b));
		// just for safety (we index with these)
		v[i] = norm_ind(v[i], nbins);
	}
	// calculate the clip limit
	double hsize = width * width;
	int minclip = std::ceil(hsize / nbins);
	int cliplim = minclip + std::lrint(clip * (hsize - minclip));
	// initialize histogram
	index_t ii, jj, iprev, jprev;
	for ( index_t i = 0; i < nbins; i++ )
		hist[i] = 0;
	for ( index_t ki = -r; ki <= r; ki++ )
	{
		for ( index_t kj = -r; kj <= r; kj++ )
		{
			ii = wrap_ind(ki, nr);
			jj = wrap_ind(kj, nc);
			if ( !isNA(v[jj * nr + ii]) )
				hist[v[jj * nr + ii]]++;
		}
	}
	if ( isNA(x[0]) )
		buffer[0] = NA_REAL;
	else
		buffer[0] = do_sum(hist, 0, v[0]);
	// sliding window
	for ( index_t i = 0; i < nr; i++ )
	{
		if ( i != 0 )
		{
			// remove previous row from histogram
			// and add current row (skip for first row)
			ii = wrap_ind(i, nr);
			iprev = wrap_ind(i - 1, nr);
			for ( index_t kj = -r; kj <= r; kj++ )
			{
				jj = wrap_ind(kj, nc);
				if ( !isNA(v[jj * nr + iprev]) )
					hist[v[jj * nr + iprev]]--;
				if ( !isNA(v[jj * nr + ii]) )
					hist[v[jj * nr + ii]]++;
			}
		}
		for ( index_t j = 0; j < nc; j++ )
		{
			if ( i == 0 && j == 0 )
				continue;
			// remove previous column from histogram
			// and add current column (skip for first pixel)
			jj = wrap_ind(j, nc);
			jprev = wrap_ind(j - 1, nc);
			for ( index_t k = -r; k <= r; k++ )
			{
				ii = wrap_ind(i + k, nr);
				if ( !isNA(v[jprev * nr + ii]) )
					hist[v[jprev * nr + ii]]--;
				if ( !isNA(v[jj * nr + ii]) )
					hist[v[jj * nr + ii]]++;
			}
			// clip the histogram
			std::memcpy(clhist, hist, nbins * sizeof(int));
			int excess = 0;
			for ( index_t k = 0; k < nbins; k++ )
			{
				if ( clhist[k] > cliplim )
					excess += (clhist[k] - cliplim);
			}
			int binincr = excess / nbins;
			int binlimit = cliplim - binincr;
			for ( index_t k = 0; k < nbins; k++ )
			{
				if ( clhist[k] > cliplim )
					clhist[k] = cliplim;
				else if ( clhist[k] >= binlimit )
				{
					excess -= (binlimit - clhist[k]);
					clhist[k] = cliplim;
				}
				else
				{
					excess -= binincr;
					clhist[k] += binincr;
				}
			}
			// distribute leftover excess
			index_t hstart = 0, k = 0;
			while ( excess > 0 && k < nbins )
			{
				k = hstart;
				index_t step = nbins / excess;
				if ( step < 1 )
					step = 1;
				while ( k < nbins && excess > 0 )
				{
					clhist[k]++;
					excess--;
					k += step;
				}
				hstart++;
			}
			// transform pixels
			if ( isNA(v[j * nr + i]) )
				buffer[j * nr + i] = NA_REAL;
			else
				buffer[j * nr + i] = do_sum(clhist, 0, v[j * nr + i]);
		}
	}
	// normalize to [0, 1]
	double vmax = do_max(buffer, 0, n - 1);
	for ( size_t i = 0; i < n; i++ )
	{
		if ( !isNA(buffer[i]) )
			buffer[i] /= vmax;
	}
	Free(clhist);
	Free(hist);
	Free(v);
}

//// Peak detection
//------------------

template<typename T>
size_t local_maxima_knn(T * x, size_t n, size_t k,
	int * neighbors, int * buffer)
{
	int nmax = 0;
	for ( index_t i = 0; i < n; i++ )
	{
		buffer[i] = false;
		for ( index_t j = 0; j < k; j++ )
		{
			index_t ki = neighbors[j * n + i];
			if ( isNA(ki) )
			{
				buffer[i] = false;
				break;
			}
			else
			{
				ki = ki - 1; // adjust 1-based index
			}
			if ( x[i] > x[ki] )
				buffer[i] = true;
			if ( ki < i && x[ki] >= x[i] )
			{
				buffer[i] = false;
				break;
			}
			if ( ki > i && x[ki] > x[i] )
			{
				buffer[i] = false;
				break;
			}
		}
		if ( buffer[i] )
			nmax++;
	}
	return nmax;
}

//// Resampling with interpolation
//---------------------------------

// statistic interpolation over indx
template<typename T>
double interp2_stat(T * z, int * indx, size_t n, int stat = EST_AVG)
{
	switch(stat) {
		case EST_SUM:
			return do_sum_at(z, indx, n);
		case EST_AVG:
			return do_mean_at(z, indx, n);
		case EST_MAX:
			return do_max_at(z, indx, n);
		case EST_MIN:
			return do_min_at(z, indx, n);
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
			for ( size_t i = 0; i < n; i++ )
			{
				dx = sdiff(xi, x[indx[i]], tol_ref);
				dy = sdiff(yi, y[indx[i]], tol_ref);
				d2[i] = (dx * dx) + (dy * dy);
			}
			return z[indx[argmin(d2, n)]];
		}
		case EST_SUM:
		case EST_AVG:
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
size_t do_approx2(Tout * ptr, Txy * xi, Txy * yi, size_t ni, Txy * xy, Tz * z, size_t n,
	double tol[2], int tol_ref, Tout nomatch, int interp = EST_NEAR, int stride = 1)
{
	for ( size_t i = 0; i < ni; i++ )
		ptr[i * stride] = nomatch;
	if ( n == 0 )
		return 0;
	size_t num_matches = 0;
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
