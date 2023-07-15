
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

rowdist <- function(x, y = x, metric = "euclidean", p = 2)
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

coldist <- function(x, y = x, metric = "euclidean", p = 2)
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

rowdist_at <- function(x, i, list, metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	if ( any(i < 1L | i > nrow(x)) )
		stop("subscript out of bounds")
	if ( length(i) != length(list) )
		stop("list and i must have the same length")
	list <- lapply(list, as.integer)
	.Call(C_rowDistAt, x, as.integer(i), list,
		as_metric(metric), p, PACKAGE="matter")
}

coldist_at <- function(x, i, list, metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	if ( any(i < 1L | i > ncol(x)) )
		stop("subscript out of bounds")
	if ( length(i) != length(list) )
		stop("list and i must have the same length")
	list <- lapply(list, as.integer)
	.Call(C_colDistAt, x, as.integer(i), list,
		as_metric(metric), p, PACKAGE="matter")
}
