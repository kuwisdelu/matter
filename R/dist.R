
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

rowdist_at <- function(x, ix, y = x, iy = list(1L:nrow(y)),
	metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( length(ix) != length(iy) && length(ix) != 1L && length(iy) != 1L )
		stop("length of ix [", length(ix),
			"] and iy [", length(iy), "] must be equal or 1")
	if ( any(lengths(ix) != lengths(iy) & lengths(ix) != 1L & lengths(iy) != 1L) )
		stop("lengths of ix [", paste_head(lengths(ix), collapse=", "),
			"] and iy [", paste_head(lengths(iy), collapse=", "),
			"] components must all be equal or 1")
	ind <- normalize_lengths2(ix, iy)
	ix <- lapply(ind[[1L]], as.integer)
	iy <- lapply(ind[[2L]], as.integer)
	if ( ncol(x) != ncol(y) )
		stop("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_rowDistAt, x, y, ix, iy,
		as_metric(metric), p, PACKAGE="matter")
}

coldist_at <- function(x, ix, y = x, iy = list(1L:ncol(y)),
	metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( length(ix) != length(iy) && length(ix) != 1L && length(iy) != 1L )
		stop("length of ix [", length(ix),
			"] and iy [", length(iy), "] must be equal or 1")
	if ( any(lengths(ix) != lengths(iy) & lengths(ix) != 1L & lengths(iy) != 1L) )
		stop("lengths of ix [", paste_head(lengths(ix), collapse=", "),
			"] and iy [", paste_head(lengths(iy), collapse=", "),
			"] components must all be equal or 1")
	ind <- normalize_lengths2(ix, iy)
	ix <- lapply(ind[[1L]], as.integer)
	iy <- lapply(ind[[2L]], as.integer)
	if ( nrow(x) != nrow(y) )
		stop("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	.Call(C_colDistAt, x, y, ix, iy,
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
