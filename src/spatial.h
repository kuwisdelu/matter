#ifndef SPATIAL
#define SPATIAL

#include "matterDefines.h"
#include "search.h"
#include "signal.h"

//// Point in polygon
//---------------------

template<typename T>
bool in_poly(T x, T y, T * vx, T * vy, size_t nvert)
{
	bool c = false;
	index_t i, j;
	T x0, x1, y0, y1;
	// loop through all edges (i, j)
	// true is (x, y) is inside, an edges, or a vertex
	for ( i = 0, j = nvert - 1; i < nvert; j = i++)
	{
		// coordinates of current edge
		x0 = vx[i], x1 = vx[j];
		y0 = vy[i], y1 = vy[j];
		// check if (x, y) is a vertex
		if ( equal(x, x0) && equal(y, y0) )
			return true;
		if ( equal(x, x1) && equal(y, y1) )
			return true;
		// extend a ray horizontally from (-inf, y) to (x, y)
		// check if it crosses the edge line anywhere
		if ( ((y0 <= y) && (y1 >= y)) || ((y1 <= y) && (y0 >= y)) )
		{
			// check where it crosses
			double cross = ((x1 - x0) * (y - y0) / (y1 - y0)) + x0;
			if ( x > cross)
				c = !c;
		}
	}
	return c;
}

template<typename T>
index_t do_in_poly(int * ptr, T * points, size_t n,
	T * vertices, size_t nvert)
{
	T * vx = vertices;
	T * vy = vertices + nvert;
	T * x = points;
	T * y = points + n;
	index_t num_contained = 0;
	for ( index_t i = 0; i < n; i++ )
	{
		ptr[i] = in_poly(x[i], y[i], vy, vx, nvert);
		num_contained += ptr[i];
	}
	return num_contained;
}

//// Distance
//------------

template<typename T>
void row_dist(T * x, T * y, size_t nx, size_t ny, size_t k,
	double * buffer, int metric = DIST_EUC, double p = 2)
{
	for ( index_t ix = 0; ix < nx; ix++ )
	{
		for ( index_t iy = 0; iy < ny; iy++ )
		{
			T * xx = x + ix;
			T * yy = y + iy;
			buffer[iy * nx + ix] = do_dist(xx, yy, k, nx, ny, metric, p);
		}
	}
}

template<typename T>
void col_dist(T * x, T * y, size_t nx, size_t ny, size_t k,
	double * buffer, int metric = DIST_EUC, double p = 2)
{
	for ( index_t ix = 0; ix < nx; ix++ )
	{
		for ( index_t iy = 0; iy < ny; iy++ )
		{
			T * xx = x + (ix * k);
			T * yy = y + (iy * k);
			buffer[iy * nx + ix] = do_dist(xx, yy, k, 1, 1, metric, p);
		}
	}
}

template<typename T>
void row_dist_at(T * x, T * y, int * indx, int * indy, size_t nx, size_t ny,
	size_t ni, size_t k, double * buffer, int metric = DIST_EUC, double p = 2)
{
	for ( index_t i = 0; i < ni; i++ )
	{
		if ( indx[i] < 0 || indx[i] >= nx )
			Rf_error("subscript out of bounds");
		if ( indy[i] < 0 || indy[i] >= ny )
			Rf_error("subscript out of bounds");
		T * xx = x + indx[i];
		T * yy = y + indy[i];
		buffer[i] = do_dist(xx, yy, k, nx, ny, metric, p);
	}
}

template<typename T>
void col_dist_at(T * x, T * y, int * indx, int * indy, size_t nx, size_t ny,
	size_t ni, size_t k, double * buffer, int metric = DIST_EUC, double p = 2)
{
	for ( index_t i = 0; i < ni; i++ )
	{
		if ( indx[i] < 0 || indx[i] >= nx )
			Rf_error("subscript out of bounds");
		if ( indy[i] < 0 || indy[i] >= ny )
			Rf_error("subscript out of bounds");
		T * xx = x + (indx[i] * k);
		T * yy = y + (indy[i] * k);
		buffer[i] = do_dist(xx, yy, k, 1, 1, metric, p);
	}
}

#endif // SPATIAL
