
#### Approximate search with resampling ####
## ------------------------------------------

is.sorted <- function(x, ...) !is.unsorted(x, ...)

# relative difference

reldiff <- function(x, y, ref = "abs")
{
	if ( is.integer(x) && is.double(y) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(y) )
		y <- as.double(y)
	ref <- pmatch(ref, c("abs", "x", "y"), nomatch=1L)
	n <- max(length(x), length(y))
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	fun <- function(a, b) .Call("Mt_relativeDiff", a, b, ref, PACKAGE="matter")
	mapply(fun, x, y, USE.NAMES=FALSE)
}

# exported version with argument checking (safer, easier)

asearch <- function(x, keys, values, tol = 0, tol.ref = "abs",
					nomatch = NA_integer_, interp = "none")
{
	if ( is.integer(x) && is.double(keys) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(keys) )
		keys <- as.double(keys)
	tol.ref <- pmatch(tol.ref, c("abs", "x", "y"), nomatch=1L)
	interp <- pmatch(interp, levels(make_combiner("")), nomatch=1L)
	asearch_int(x, keys=keys, values=values, tol=tol, tol.ref=tol.ref,
		nomatch=nomatch, interp=interp, sorted=is.sorted(keys))
}

# faster internal version with no argument checking

asearch_int <- function(x, keys, values, tol = 0, tol.ref = 1L,
					nomatch = NA_integer_, interp = 1L, sorted = TRUE)
{
	.Call("Mt_approxSearch", x, keys, values, tol, tol.ref,
		nomatch, interp, sorted, PACKAGE="matter")
}

# exported version with argument checking (safer, easier)

bsearch <- function(x, table, tol = 0, tol.ref = "abs",
					nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(x) && is.double(table) )
		x <- as.double(x)
	if ( is.double(x) && is.integer(table) )
		table <- as.double(table)
	if ( is.unsorted(table) )
		stop("'table' must be sorted")
	tol.ref <- pmatch(tol.ref, c("abs", "x", "y"), nomatch=1L)
	bsearch_int(x, table=table, tol=tol, tol.ref=tol.ref,
		nomatch=nomatch, nearest=nearest)
}

# faster internal version with no argument checking

bsearch_int <- function(x, table, tol = 0, tol.ref = 1L,
					nomatch = NA_integer_, nearest = FALSE)
{
	.Call("Mt_binarySearch", x, table, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}

