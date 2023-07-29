
#### Distances for matter matrices ####
## ------------------------------------

setMethod("rowDists", c("matrix", "missing"),
	function(x, ..., BPPARAM = bpparam()) {
		rowDists_int(x, x, ..., BPPARAM = BPPARAM)
	})

setMethod("colDists", c("matrix", "missing"),
	function(x, ..., BPPARAM = bpparam()) {
		colDists_int(x, x, ..., BPPARAM = BPPARAM)
	})

setMethod("rowDists", c("matrix", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		rowDists_int(x, y, ..., BPPARAM = BPPARAM)
	})

setMethod("colDists", c("matrix", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		colDists_int(x, y, ..., BPPARAM = BPPARAM)
	})

# matter matrices
setMethod("rowDists", c("matter_mat", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			rowDists_int(x, y, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			rowDists_int(x, y, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("rowDists", c("matrix", "matter_mat"),
	function(x, y, ..., BPPARAM = bpparam()) {
		t(rowDists(y, x, ..., BPPARAM = BPPARAM))
	})

setMethod("colDists", c("matter_mat", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			colDists_int(x, y, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			colDists_int(x, y, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("colDists", c("matrix", "matter_mat"),
	function(x, y, ..., BPPARAM = bpparam()) {
		t(colDists(y, x, ..., BPPARAM = BPPARAM))
	})

# sparse matrices
setMethod("rowDists", c("sparse_mat", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			rowDists_int(x, y, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			rowDists_int(x, y, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("rowDists", c("matrix", "sparse_mat"),
	function(x, y, ..., BPPARAM = bpparam()) {
		t(rowDists(y, x, ..., BPPARAM = BPPARAM))
	})

setMethod("colDists", c("sparse_mat", "matrix"),
	function(x, y, ..., BPPARAM = bpparam()) {
		if ( x@transpose ) {
			colDists_int(x, y, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			colDists_int(x, y, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("colDists", c("matrix", "sparse_mat"),
	function(x, y, ..., BPPARAM = bpparam()) {
		t(colDists(y, x, ..., BPPARAM = BPPARAM))
	})

rowDists_int <- function(x, y, metric = "euclidean", p = 2,
	weights = NULL, iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( iter.dim == 1L ) {
		ans <- chunk_rowapply(x, rowdist, y=y, metric=metric, p=p,
			weights=weights, simplify=rbind, BPPARAM=BPPARAM, ...)
	} else {
		BIND <- function(x, ...) dist_c(x, ..., metric=metric, p=p)
		ans <- chunk_colapply(x,
			function(xi) {
				if ( !is.null(weights) ){
					wi <- weights[attr(xi, "index")]
				} else {
					wi <- NULL
				}
				yi <- y[,attr(xi, "index"),drop=FALSE]
				rowdist(xi, yi, metric=metric, p=p, weights=wi)
			}, simplify=BIND, BPPARAM=BPPARAM, ...)
	}
	if ( !is.null(rownames(x)) || !is.null(rownames(y)) )
		dimnames(ans) <- list(rownames(x), rownames(y))
	ans
}

colDists_int <- function(x, y, metric = "euclidean", p = 2,
	weights = NULL, iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		stop("iter.dim must be 1 or 2")
	if ( iter.dim == 1L ) {
		BIND <- function(x, ...) dist_c(x, ..., metric=metric, p=p)
		ans <- chunk_rowapply(x,
			function(xi) {
				if ( !is.null(weights) ){
					wi <- weights[attr(xi, "index")]
				} else {
					wi <- NULL
				}
				yi <- y[attr(xi, "index"),,drop=FALSE]
				coldist(xi, yi, metric=metric, p=p, weights=wi)
			}, simplify=BIND, BPPARAM=BPPARAM, ...)
	} else {
		ans <- chunk_colapply(x, coldist, y=y, metric=metric, p=p,
			weights=weights, simplify=rbind, BPPARAM=BPPARAM, ...)
	}
	if ( !is.null(rownames(x)) || !is.null(rownames(y)) )
		dimnames(ans) <- list(rownames(x), rownames(y))
	ans
}

#### Distances ####
## ----------------

rowdist <- function(x, y = x, metric = "euclidean", p = 2, weights = NULL)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( ncol(x) != ncol(y) )
		stop("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != ncol(x) )
			stop("length of weights must match number of columns")
		weights <- as.double(weights)
	}
	.Call(C_rowDist, x, y, as_dist(metric), p, weights, PACKAGE="matter")
}

coldist <- function(x, y = x, metric = "euclidean", p = 2, weights = NULL)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( nrow(x) != nrow(y) )
		stop("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != nrow(x) )
			stop("length of weights must match number of rows")
		weights <- as.double(weights)
	}
	.Call(C_colDist, x, y, as_dist(metric), p, weights, PACKAGE="matter")
}

dist_c <- function(x, y, ..., metric = "euclidean", p = 2)
{
	metric <- as_dist(metric)
	if ( missing(y) )
		return(x)
	if ( ...length() > 0L )
		y <- do.call(dist_c, list(y, ..., metric=metric, p=p))
	switch(as.character(metric),
		euclidean = sqrt(x^2 + y^2),
		maximum = pmax(x, y),
		manhattan = x + y,
		minkowski = (x^p + y^p)^(1/p))
}

#### Distances at specific indices ####
## ------------------------------------

rowdist_at <- function(x, ix, y = x, iy = list(1L:nrow(y)),
	metric = "euclidean", p = 2, weights = NULL)
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
	if ( !is.null(weights) ) {
		if ( length(weights) != ncol(x) )
			stop("length of weights must match number of columns")
		weights <- as.double(weights)
	}
	.Call(C_rowDistAt, x, y, ix, iy,
		as_dist(metric), p, weights, PACKAGE="matter")
}

coldist_at <- function(x, ix, y = x, iy = list(1L:ncol(y)),
	metric = "euclidean", p = 2, weights = NULL)
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
	if ( !is.null(weights) ) {
		if ( length(weights) != nrow(x) )
			stop("length of weights must match number of rows")
		weights <- as.double(weights)
	}
	.Call(C_colDistAt, x, y, ix, iy,
		as_dist(metric), p, weights, PACKAGE="matter")
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
