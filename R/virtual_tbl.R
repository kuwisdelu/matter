### Define matter<data table> class for virtual data tables ####
# --------------------------------------------------------------

setClass("virtual_tbl",
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

setMethod("describe_for_display", "virtual_tbl", function(x) {
	desc1 <- paste0("<", x@dim[[1]], " row, ", x@dim[[2]], " column> ", class(x))
	desc2 <- paste0("virtual data table")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "virtual_tbl", function(x) {
	preview_table(x, classinfo=sapply(atomdata(x), function(y) class(y)[1L]))
})

setMethod("head", "virtual_tbl",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    n <- if (n < 0L) 
	        max(nrow(x) + n, 0L)
	    else min(n, nrow(x))
	    x[seq_len(n),,drop=FALSE]
})

setMethod("tail", "virtual_tbl",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    nrx <- nrow(x)
	    n <- if (n < 0L) 
	        max(nrx + n, 0L)
	    else min(n, nrx)
	    x[seq.int(to=nrx, length.out=n),,drop=FALSE]
})

setReplaceMethod("names", "virtual_tbl", function(x, value) {
	x@names <- value
	if ( is.null(x@dimnames) ) {
		x@dimnames <- list(NULL, value)
	} else {
		x@dimnames[[2]] <- value
	}
	if ( validObject(x) )
		x
})

setReplaceMethod("dimnames", "virtual_tbl", function(x, value) {
	x@names <- value[[2]]
	x@dimnames <- value
	if ( validObject(x) )
		x
})
