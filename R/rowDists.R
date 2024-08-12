
#### Distances for matter matrices ####
## ------------------------------------

setMethod("rowDists", c("ANY", "missing"),
	function(x, at, ..., BPPARAM = bpparam()) {
		if ( missing(at) ) {
			rowDists(x, x, ..., BPPARAM = BPPARAM)
		} else {
			rowDistsAt_int(x, at, ..., BPPARAM = BPPARAM)
		}
	})

setMethod("colDists", c("ANY", "missing"),
	function(x, at, ..., BPPARAM = bpparam()) {
		if ( missing(at) ) {
			colDists(x, x, ..., BPPARAM = BPPARAM)
		} else {
			colDistsAt_int(x, at, ..., BPPARAM = BPPARAM)
		}
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
		if ( rowMaj(x) ) {
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
		if ( rowMaj(x) ) {
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
		if ( rowMaj(x) ) {
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
		if ( rowMaj(x) ) {
			colDists_int(x, y, ..., iter.dim=1L, BPPARAM = BPPARAM)
		} else {
			colDists_int(x, y, ..., iter.dim=2L, BPPARAM = BPPARAM)
		}
	})

setMethod("colDists", c("matrix", "sparse_mat"),
	function(x, y, ..., BPPARAM = bpparam()) {
		t(colDists(y, x, ..., BPPARAM = BPPARAM))
	})

rowDists_fun <- function(iter.dim)
{
	switch(iter.dim,
		`1`=rowdist,
		`2`=isofun(function(x, y, metric, p, weights)
			{
				if ( is.null(weights) ){
					w <- NULL
				} else {
					w <- weights[attr(x, "index")]
				}
				y <- y[,attr(x, "index"),drop=FALSE]
				matter::rowdist(x, y,
					metric=metric, p=p, weights=w)
			}, matter_env()))
}

rowDists_int <- function(x, y, metric = "euclidean", p = 2,
	weights = NULL, iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		matter_error("iter.dim must be 1 or 2")
	FUN <- rowDists_fun(iter.dim)
	if ( iter.dim == 1L ) {
		ans <- chunk_rowapply(x, FUN, y=y,
			metric=metric, p=p, weights=weights,
			simplify=rbind, BPPARAM=BPPARAM, ...)
	} else {
		BIND <- function(...) dist_c(..., metric=metric, p=p)
		ans <- chunk_colapply(x, FUN, y=y,
			metric=metric, p=p, weights=weights,
			simplify=BIND, BPPARAM=BPPARAM, ...)
	}
	if ( !is.null(rownames(x)) || !is.null(rownames(y)) )
		dimnames(ans) <- list(rownames(x), rownames(y))
	ans
}

colDists_fun <- function(iter.dim)
{
	switch(iter.dim,
		`1`=isofun(function(x, y, metric, p, weights)
			{
				if ( is.null(weights) ){
					w <- NULL
				} else {
					w <- weights[attr(x, "index")]
				}
				y <- y[attr(x, "index"),,drop=FALSE]
				matter::coldist(x, y,
					metric=metric, p=p, weights=w)
			}, matter_env()),
		`2`=coldist)
}

colDists_int <- function(x, y, metric = "euclidean", p = 2,
	weights = NULL, iter.dim = 1L, BPPARAM = bpparam(), ...)
{
	if ( !iter.dim %in% c(1L, 2L) )
		matter_error("iter.dim must be 1 or 2")
	FUN <- colDists_fun(iter.dim)
	if ( iter.dim == 1L ) {
		BIND <- function(...) dist_c(..., metric=metric, p=p)
		ans <- chunk_rowapply(x, FUN, y=y,
			metric=metric, p=p, weights=weights,
			simplify=BIND, BPPARAM=BPPARAM, ...)
	} else {
		ans <- chunk_colapply(x, FUN, y=y,
			metric=metric, p=p, weights=weights,
			simplify=rbind, BPPARAM=BPPARAM, ...)
	}
	if ( !is.null(colnames(x)) || !is.null(colnames(y)) )
		dimnames(ans) <- list(colnames(x), colnames(y))
	ans
}

rowDistsAt_int <- function(x, at, metric = "euclidean", p = 2,
	weights = NULL, BPPARAM = bpparam(), ...)
{
	if ( "iter.dim" %in% ...names() )
		matter_error("iter.dim will be ignored when 'at' is used")
	if ( is.matrix(at) )
		at <- array2list(at, 1L)
	if ( length(at) != nrow(x) )
		at <- rep_len(at, nrow(x))
	FUN <- isofun(function(xi, metric, p, weights)
		{
			di <- attr(xi, "depends")
			i <- which(!vapply(di, is.null, logical(1L)))
			j <- di[i]
			matter::rowdist_at(xi, ix=i, iy=j,
				metric=metric, p=p, weights=weights)
		})
	ans <- chunk_rowapply(x, FUN, depends=at,
		metric=metric, p=p, weights=weights,
		BPPARAM=BPPARAM, ...)
	names(ans) <- rownames(x)
	ans
}

colDistsAt_int <- function(x, at, metric = "euclidean", p = 2,
	weights = NULL, BPPARAM = bpparam(), ...)
{
	if ( "iter.dim" %in% ...names() )
		matter_error("iter.dim will be ignored when 'at' is used")
	if ( is.matrix(at) )
		at <- array2list(at, 1L)
	if ( length(at) != ncol(x) )
		at <- rep_len(at, ncol(x))
	FUN <- isofun(function(xi, metric, p, weights)
		{
			di <- attr(xi, "depends")
			i <- which(!vapply(di, is.null, logical(1L)))
			j <- di[i]
			matter::coldist_at(xi, ix=i, iy=j,
				metric=metric, p=p, weights=weights)
		})
	ans <- chunk_colapply(x, FUN, depends=at,
		metric=metric, p=p, weights=weights,
		BPPARAM=BPPARAM, ...)
	names(ans) <- colnames(x)
	ans
}

#### Distances ####
## ----------------

rowdist <- function(x, y = x, metric = "euclidean", p = 2, weights = NULL)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( ncol(x) != ncol(y) )
		matter_error("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != ncol(x) )
			matter_error("length of weights must match number of columns")
		weights <- as.double(weights)
	}
	.Call(C_rowDist, x, y, as_dist(metric), p, weights, PACKAGE="matter")
}

coldist <- function(x, y = x, metric = "euclidean", p = 2, weights = NULL)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if ( nrow(x) != nrow(y) )
		matter_error("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != nrow(x) )
			matter_error("length of weights must match number of rows")
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
	if ( is.matrix(ix) )
		ix <- array2list(ix, 1L)
	if ( is.matrix(iy) )
		iy <- array2list(iy, 1L)
	if ( length(ix) != length(iy) && length(ix) != 1L && length(iy) != 1L )
		matter_error("length of ix [", length(ix),
			"] and iy [", length(iy), "] must be equal or 1")
	if ( any(lengths(ix) != lengths(iy) & lengths(ix) != 1L & lengths(iy) != 1L) )
		matter_error("lengths of ix [", paste_head(lengths(ix), collapse=", "),
			"] and iy [", paste_head(lengths(iy), collapse=", "),
			"] components must all be equal or 1")
	ind <- normalize_lengths2(ix, iy)
	ix <- lapply(ind[[1L]], as.integer)
	iy <- lapply(ind[[2L]], as.integer)
	if ( ncol(x) != ncol(y) )
		matter_error("x and y must have equal number of columns")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != ncol(x) )
			matter_error("length of weights must match number of columns")
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
	if ( is.matrix(ix) )
		ix <- array2list(ix, 1L)
	if ( is.matrix(iy) )
		iy <- array2list(iy, 1L)
	if ( length(ix) != length(iy) && length(ix) != 1L && length(iy) != 1L )
		matter_error("length of ix [", length(ix),
			"] and iy [", length(iy), "] must be equal or 1")
	if ( any(lengths(ix) != lengths(iy) & lengths(ix) != 1L & lengths(iy) != 1L) )
		matter_error("lengths of ix [", paste_head(lengths(ix), collapse=", "),
			"] and iy [", paste_head(lengths(iy), collapse=", "),
			"] components must all be equal or 1")
	ind <- normalize_lengths2(ix, iy)
	ix <- lapply(ind[[1L]], as.integer)
	iy <- lapply(ind[[2L]], as.integer)
	if ( nrow(x) != nrow(y) )
		matter_error("x and y must have equal number of rows")
	if ( is.integer(x) && is.double(y) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(y) )
		storage.mode(y) <- "double"
	if ( !is.null(weights) ) {
		if ( length(weights) != nrow(x) )
			matter_error("length of weights must match number of rows")
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
		matter_error("poly must have at least 3 vertices")
	if ( ncol(points) != 2L || ncol(points) != 2L )
		matter_error("points and poly must have 2 columns")
	if ( is.integer(points) && is.double(poly) )
		storage.mode(points) <- "double"
	if ( is.double(points) && is.integer(poly) )
		storage.mode(poly) <- "double"
	.Call(C_inPoly, points, poly, PACKAGE="matter")
}
