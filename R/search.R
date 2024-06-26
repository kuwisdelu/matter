
#### Search for k-th largest element ####
## --------------------------------------

qorder <- function(x)
{
	.Call(C_quickOrder, x, PACKAGE="matter")
}

qrank <- function(x, ties.max = FALSE)
{
	.Call(C_quickRank, x, ties.max, PACKAGE="matter")
}

qselect <- function(x, k = (length(x) + 1L) %/% 2L)
{
	if ( any(k < 1L | k > length(x)) )
		stop("k is out of bounds")
	.Call(C_quickSelect, x, as.integer(k - 1L), PACKAGE="matter")
}

qmedian <- function(x, na.rm = FALSE)
{
	if ( na.rm ) {
		x <- x[!is.na(x)]
	} else if ( anyNA(x) ) {
		return(NA_real_)
	}
	if ( length(x) == 0L ) {
		return(NA_real_)
	} else {
		.Call(C_quickMedian, x, PACKAGE="matter")
	}
}

qmad <- function(x, center = qmedian(x),
	constant = 1.4826, na.rm = FALSE)
{
	if ( na.rm ) {
		x <- x[!is.na(x)]
	} else if ( anyNA(x) ) {
		return(NA_real_)
	}
	if ( length(x) == 0L ) {
		return(NA_real_)
	} else {
		.Call(C_quickMAD, x, as.double(center),
			as.double(constant), PACKAGE="matter")
	}
}

#### Approximate 1D search ####
## -----------------------------

reldiff <- function(x, y, ref = "y")
{
	ref <- as_tol_ref(ref)
	if ( missing(y) ) {
		y <- x[-length(x)]
		x <- x[-1L]
	}
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	n <- max(length(x), length(y))
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	if ( is.character(x) ) {
		fun <- function(a, b) .Call(C_relativeDiff, a, b, ref, PACKAGE="matter")
		mapply(fun, x, y, USE.NAMES=FALSE)
	} else {
		if ( ref == "x" ) {
			(x - y) / x
		} else if ( ref == "y" ) {
			(x - y) / y
		} else {
			x - y
		}
	}
}

bsearch <- function(x, table, tol = 0, tol.ref = "abs",
	nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(x) && is.double(table) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(table) )
		table <- as.double(table)
	if ( is.unsorted(table) )
		stop("'table' must be sorted")
	.Call(C_binarySearch, x, table, tol, as_tol_ref(tol.ref),
		nomatch, nearest, PACKAGE="matter")
}

asearch <- function(x, keys, values = seq_along(keys),
	tol = 0, tol.ref = "abs", nomatch = NA_integer_)
{
	if ( is.integer(x) && is.double(keys) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(keys) )
		keys <- as.double(keys)
	if ( is.character(keys) ) {
		if ( is.unsorted(keys) ) {
			ord <- order(keys)
			keys <- keys[ord]
			values <- values[ord]
		}
		i <- bsearch(x, keys, tol, as_tol_ref(tol.ref), -1L)
		result <- values[ifelse(i > 0L, i, NA)]
		result[i < 0L] <- nomatch
	} else {
		result <- .Call(C_Approx1, x, keys, values, tol, as_tol_ref(tol.ref),
			nomatch, as_interp("none"), PACKAGE="matter")
	}
	result
}

#### Approximate K-D search ####
## ------------------------------

kdtree <- function(data)
{
	if ( inherits(data, "kdtree") )
		return(data)
	if ( is.list(data) )
		data <- do.call(cbind, data)
	data <- as.matrix(data)
	tree <- .Call(C_kdTree, data, PACKAGE="matter")
	nodes <- data.frame(left_child=tree[[1L]], right_child=tree[[2L]])
	structure(list(data=data, nodes=nodes, root=tree[[3L]]),
		class="kdtree")
}

print.kdtree <- function(x, ...)
{
	n <- paste0("[", nrow(x$data), "]")
	cat(class(x)[1L], n, "with k =", ncol(x$data), "\n")
}

kdsearch <- function(x, data, tol = 0, tol.ref = "abs")
{
	if ( missing(data) || is.null(data) ) {
		if ( !inherits(x, "kdtree") )
			data <- kdtree(x)
		x <- data$data
	} else {
		if ( !inherits(data, "kdtree") )
			data <- kdtree(data)
		x <- as.matrix(x)
	}
	if ( is.integer(x) && is.double(data$data) )
		storage.mode(x) <- "double"
	if ( is.double(x) && is.integer(data$data) )
		storage.mode(data$data) <- "double"
	if ( is.null(dim(x)) && length(x) != ncol(data$data) )
		stop("x must have the same number of columns as data")
	tol <- rep_len(tol, ncol(data$data))
	.Call(C_kdSearch, x, data$data,
		data$nodes$left_child, data$nodes$right_child,
		data$root, tol, as_tol_ref(tol.ref), PACKAGE="matter")
}

knnsearch <- function(x, data, k = 1L, metric = "euclidean", p = 2)
{
	if ( missing(data) || is.null(data) ) {
		if ( !inherits(x, "kdtree") )
			x <- kdtree(x)
		.Call(C_knnSelfSearch, x$data,
			x$nodes$left_child, x$nodes$right_child, k,
			as_dist(metric), p, PACKAGE="matter")
	} else {
		x <- as.matrix(x)
		if ( !inherits(data, "kdtree") )
			data <- kdtree(data)
		if ( is.integer(x) && is.double(data$data) )
			storage.mode(x) <- "double"
		if ( is.double(x) && is.integer(data$data) )
			storage.mode(data$data) <- "double"
		if ( is.null(dim(x)) && length(x) != ncol(data$data) )
			stop("x must have the same number of columns as data")
		.Call(C_knnSearch, x, data$data,
			data$nodes$left_child, data$nodes$right_child,
			data$root, k, as_dist(metric), p, PACKAGE="matter")
	}
}

nnpairs <- function(x, y, metric = "euclidean", p = 2)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	mx <- knnsearch(x, y, 1L, metric, p)
	my <- knnsearch(y, x, 1L, metric, p)
	m <- cbind(c(1L:nrow(x), my), c(mx, 1L:nrow(y)))
	dup <- duplicated(m, MARGIN=1L)
	m[!dup,,drop=FALSE]
}

