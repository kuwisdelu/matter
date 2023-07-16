
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

rowdist_at <- function(x, y = x, xat, yat,
	metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( length(xat) != length(yat) & length(xat) != 1L & length(yat) != 1L )
		stop("lengths of xat and yat must be 1 or equal")
	if ( any(lengths(xat) != lengths(yat) & lengths(xat) != 1L & lengths(yat) != 1L) )
		stop("lengths of xat and yat components must be 1 or equal")
	ind <- normalize_lengths2(xat, yat)
	xat <- lapply(ind[[1L]], as.integer)
	yat <- lapply(ind[[2L]], as.integer)
	if ( ncol(x) != ncol(y) )
		stop("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_rowDistAt, x, y, xat, yat,
		as_metric(metric), p, PACKAGE="matter")
}

coldist_at <- function(x, y = x, xat, yat,
	metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( length(xat) != length(yat) & length(xat) != 1L & length(yat) != 1L )
		stop("lengths of xat and yat must be 1 or equal")
	if ( any(lengths(xat) != lengths(yat) & lengths(xat) != 1L & lengths(yat) != 1L) )
		stop("lengths of xat and yat components must be 1 or equal")
	ind <- normalize_lengths2(xat, yat)
	xat <- lapply(ind[[1L]], as.integer)
	yat <- lapply(ind[[2L]], as.integer)
	if ( nrow(x) != nrow(y) )
		stop("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_colDistAt, x, y, xat, yat,
		as_metric(metric), p, PACKAGE="matter")
}

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
