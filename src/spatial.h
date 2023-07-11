#ifndef SPATIAL
#define SPATIAL

#include "matterDefines.h"

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

#endif // SPATIAL
