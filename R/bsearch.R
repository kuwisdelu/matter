
#### Binary search that allows near-matches ####
## ----------------------------------------------

bsearch <- function(key, values, tol = 0, tol.ref = "none",
					nomatch = NA_integer_, nearest = FALSE)
{
	if ( is.integer(key) && is.double(values) )
		key <- as.double(key)
	if ( is.double(key) && is.integer(values) )
		values <- as.double(values)
	if ( tol < 0 )
		stop("'tol' must be non-negative")
	tol.ref <- pmatch(tol.ref, c("none", "key", "values"), nomatch=1L)
	.Call("C_binarySearch", key, values, tol,
		tol.ref, nomatch, nearest, PACKAGE="matter")
}
