
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
					names = NULL, keys = NULL, from0 = TRUE,
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
	if ( length(index) != length(data) )
		index <- rep(index, length.out=length(data))
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

# getSparseVector <- function(x) {
# 	# y <- .Call("C_getVector", x, PACKAGE="matter")
# 	y <- kvsearch(seq_len(x@length), x@index, x@data, nomatch=zero)
# 	if ( !is.null(names(x)) )
# 		names(y) <- names(x)
# 	y
# }

# setSparseVector <- function(x, value) {
# 	if ( length(x) %% length(value) != 0 )
# 		warning("number of items to replace is not ",
# 			"a multiple of replacement length")
# 	if ( length(value) != 1 )
# 		value <- rep(value, length.out=length(x))
# 	if ( is.logical(value) )
# 		value <- as.integer(value)
# 	if ( is.character(value) )
# 		value <- as.double(value)
# 	.Call("C_setVector", x, value, PACKAGE="matter")
# 	if ( validObject(x) )
# 		invisible(x)
# }

