### Define matter<data table> class for virtual data tables ####
# --------------------------------------------------------------

setClass("matter_tbl",
	contains = c("matter", "VIRTUAL"),
	prototype = prototype(
		dim = c(0L, 0L),
		names = character(),
		dimnames = list(NULL, character())),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@names) )
			errors <- c(errors, "data table must have non-NULL 'names'")
		if ( any(object@names != object@dimnames[[2]]) )
			errors <- c(errors, "'names' and column names do not match")
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("describe_for_display", "matter_tbl", function(x) "out-of-memory data table")

print_tabular_data <- function(x, classinfo, n = 6L, ...) {
	tbl <- head(x, n=n)
	out <- as.matrix(format(tbl))
	if ( missing(classinfo) )
		classinfo <- sapply(tbl, function(y)
			paste0("<", class(y)[1], ">"), USE.NAMES=FALSE)
	classinfo <- matrix(classinfo, nrow = 1,
		dimnames = list("", colnames(out)))
	out <- rbind(classinfo, out)
	print(out, quote=FALSE, right=TRUE)
	if ( nrow(x) > n )
		cat("[and ", nrow(x) - n, " more rows]", "\n", sep="")
}

setMethod("head", "matter_tbl",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    n <- if (n < 0L) 
	        max(nrow(x) + n, 0L)
	    else min(n, nrow(x))
	    x[seq_len(n),,drop=FALSE]
})

setMethod("tail", "matter_tbl",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    nrx <- nrow(x)
	    n <- if (n < 0L) 
	        max(nrx + n, 0L)
	    else min(n, nrx)
	    x[seq.int(to=nrx, length.out=n),,drop=FALSE]
})

setReplaceMethod("names", "matter_tbl", function(x, value) {
	x@names <- value
	if ( is.null(x@dimnames) ) {
		x@dimnames <- list(NULL, value)
	} else {
		x@dimnames[[2]] <- value
	}
	if ( validObject(x) )
		x
})

setReplaceMethod("dimnames", "matter_tbl", function(x, value) {
	x@names <- value[[2]]
	x@dimnames <- value[[2]]
	if ( validObject(x) )
		x
})
