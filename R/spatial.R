
#### Point in polygon ####
## ------------------------

inpoly <- function(points, poly)
{
	poly <- as.matrix(poly)
	points <- as.matrix(points)
	if ( nrow(poly) < 3L )
		stop("poly must have at least 3 vertices")
	if ( ncol(points) != 2L || ncol(points) != 2L )
		stop("points and poly must have 2 columns")
	if ( is.integer(points) && is.double(poly) )
		storage.mode(points) <- "double"
	if ( is.double(points) && is.integer(poly) )
		storage.mode(poly) <- "double"
	.Call(C_inPoly, points, poly, PACKAGE="matter")
}

#### Distance ####
## ---------------

rowdist <- function(x, y, metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( ncol(x) != ncol(y) )
		stop("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_rowDist, x, y, as_metric(metric), p, PACKAGE="matter")
}

coldist <- function(x, y, metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( nrow(x) != nrow(y) )
		stop("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_colDist, x, y, as_metric(metric), p, PACKAGE="matter")
}

#### Spatial distance (for signals) ####
## -------------------------------------

sp_rowdist <- function(x, y, i, j, xcoord, ycoord, tol = 0, tol.ref = "abs",
	weights = "gaussian", metric = "euclidean", p = 2, scale = 1)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( ncol(x) != ncol(y) )
		stop("x and y must have equal number of columns")
	if ( nrow(x) != nrow(xcoord) )
		stop("x and xcoord must have an equal number of rows")
	if ( nrow(y) != nrow(ycoord) )
		stop("y and ycoord must have an equal number of rows")
	if ( ncol(xcoord) != ncol(ycoord) )
		stop("xcoord and ycoord must have an equal number of columns")
	scale <- rep_len(scale, ncol(x))
	# TODO
}

sp_rowdist_int <- function(x, y, i, j, xcoord, ycoord, nbi = NULL, nbj = NULL,
	weights = "gaussian", metric = "euclidean", p = 2, scale = 1)
{
	# TODO
}

sp_coldist <- function(x, y, i, j, xcoord, ycoord, tol = 0, tol.ref = "abs",
	weights = "gaussian", metric = "euclidean", p = 2, scale = 1)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( nrow(x) != nrow(y) )
		stop("x and y must have equal number of rows")
	if ( ncol(x) != nrow(xcoord) )
		stop("xcoord must have a number of rows equal to columns of x")
	if ( ncol(y) != nrow(ycoord) )
		stop("ycoord must have a number of rows equal to columns of y")
	if ( ncol(xcoord) != ncol(ycoord) )
		stop("xcoord and ycoord must have an equal number of columns")
	scale <- rep_len(scale, nrow(x))
	# TODO
}

sp_coldist_int <- function(x, y, i, j, xcoord, ycoord, nbi = NULL, nbj = NULL,
	weights = "gaussian", metric = "euclidean", p = 2, scale = 1)
{
	# TODO
}
