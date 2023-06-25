
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

asearch <- function(x, keys, values, tol = 0, tol.ref = "abs",
	nomatch = NA_integer_, interp = "none")
{
	if ( is.integer(x) && is.double(keys) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(keys) )
		keys <- as.double(keys)
	if ( is.unsorted(keys) ) {
		ord <- order(keys)
		keys <- keys[ord]
		values <- values[ord]
	}
	.Call(C_Approx1, x, keys, values, tol, as_tol_ref(tol.ref),
		nomatch, as_interp(interp), PACKAGE="matter")
}

#### Search for k-th largest element ####
## --------------------------------------

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
