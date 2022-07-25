
#### Define matter<sparse vector> classes for sparse data ####
## -----------------------------------------------------------

setClass("sparse_vec",
	slots = c(
		data = "numeric",
		index = "numeric",
		keys = "numeric_OR_NULL",
		tolerance = "numeric",
		combiner = "factor"),
	prototype = prototype(
		data = numeric(),
		datamode = make_datamode("numeric", type="R"),
		length = 0,
		dim = NULL,
		names = NULL,
		dimnames = NULL,
		index = integer(),
		keys = NULL,
		tolerance = make_tolerance(0),
		combiner = make_combiner("top")),
	contains = "matter",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@dim) )
			errors <- c(errors, "vector must have NULL 'dim'")
		if ( !is.null(object@dimnames) )
			errors <- c(errors, "vector must have NULL 'dimnames'")
		if ( inherits(data, c("list", "matter_list")) ) {
			if ( !all(lengths(object@data) == lengths(object@index)) )
				errors <- c(errors, "lengths of 'data' must match lengths of 'index'")
		} else {
			if ( length(object@data) != length(object@index) )
				errors <- c(errors, "length of 'data' must match length of 'index'")
		}
		if ( is.null(errors) ) TRUE else errors
	})

sparse_vec <- function(data, index, datamode = "double", length = 0,
					names = NULL, keys = NULL, from0 = FALSE,
					tolerance = c(abs=0), combiner = "top",
					chunksize = getOption("matter.default.chunksize"), ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(index) ) {
			nz <- data != 0
			length <- length(data)
			index <- which(nz) - from0
			data <- data[nz]
		}
	}
	if ( missing(length) ) {
		if ( is.null(keys) ) {
			length <- max(index) + from0
		} else {
			length <- length(keys)
		}
	}
	if ( missing(keys) || is.null(keys) )
		keys <- as.integer(!from0)
	if ( length(index) != length(data) )
		index <- rep(index, length.out=length(data))
	if ( length(keys) > 1L && length(keys) != length )
		keys <- rep(keys, length.out=length)
	new("sparse_vec",
		data=data,
		datamode=make_datamode(datamode, type="R"),
		paths=character(),
		filemode=make_filemode(),
		chunksize=as.integer(chunksize),
		length=length,
		names=names,
		index=index,
		keys=keys,
		tolerance=make_tolerance(tolerance),
		combiner=make_combiner(combiner))
}

setMethod("describe_for_display", "sparse_vec", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("sparse ", x@datamode, " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "sparse_vec", function(x) preview_vector(x))

getSparseVector <- function(x, i) {
	y <- .Call("C_getSparseVector", x, i, PACKAGE="matter")
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	y
}

setMethod("[",
	c(x = "sparse_vec", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ..., drop) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			getSparseVector(x, i)
		} else {
			getSparseVector(x, NULL)
		}
	})

