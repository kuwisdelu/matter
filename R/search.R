
#### Approximate search with fuzzy matching ####
## ---------------------------------------------

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
	.Call(C_Approx1, x, keys, values, tol, as_tol_ref(tol.ref),
		nomatch, as_interp("none"), PACKAGE="matter")
}

kdsearch <- function(x, data, tol = NA_real_, tol.ref = "abs")
{
	if ( !is(data, "kdtree") )
		data <- kdtree(data)
	if ( is.integer(x) && is.double(data$data) )
		storage.mode(x) <- "integer"
	if ( is.double(x) && is.integer(data$data) )
		storage.mode(data$data) <- "double"
	if ( is.null(dim(x)) && length(x) != ncol(data$data) )
		stop("x must have the same number of columns as data")
	tol <- rep_len(tol, ncol(data$data))
	if ( anyNA(tol) ) {
		# guess tol as ~2x the max gap in each dim
		ref <- ifelse(tol.ref == "abs", "abs", "y")
		fun <- function(x) 2 * max(abs(reldiff(sort(x), ref=ref)))
		newtol <- apply(data$data, 2L, fun)
		tol[is.na(tol)] <- newtol[is.na(tol)]
	}
	res <- .Call(C_kdSearch, x, data$data,
		data$nodes$left_child, data$nodes$right_child,
		data$root, tol, as_tol_ref(tol.ref), PACKAGE="matter")
	attr(res, "tol") <- tol
	res
}

kdtree <- function(x)
{
	x <- as.matrix(x)
	tree <- .Call(C_kdTree, x, PACKAGE="matter")
	nodes <- data.frame(left_child=tree[[1L]], right_child=tree[[2L]])
	structure(list(data=x, nodes=nodes, root=tree[[3L]]),
		class="kdtree")
}

print.kdtree <- function(x, ...)
{
	n <- paste0("[", nrow(x$data), "]")
	cat(class(x)[1L], n, "with k =", ncol(x$data), "\n")
}

#### Search for k-th largest element ####
## --------------------------------------

qorder <- function(x)
{
	.Call(C_quickOrder, x, PACKAGE="matter")
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
