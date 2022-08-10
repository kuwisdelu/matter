
#### Define matter<sparse vector> classes for sparse data ####
## -----------------------------------------------------------

setClassUnion("matter_numeric_types", c("integer", "numeric", "matter_vec"))

setClass("sparse_vec",
	slots = c(
		data = "matter_numeric_types",
		index = "matter_numeric_types",
		offset = "integer",
		keys = "numeric_OR_NULL",
		dim = "NULL",
		dimnames = "NULL",
		tolerance = "numeric",
		combiner = "factor"),
	prototype = prototype(
		data = numeric(),
		datamode = make_datamode("numeric", type="R"),
		index = integer(),
		offset = 0L,
		keys = NULL,
		length = 0,
		names = NULL,
		tolerance = make_tolerance(0),
		combiner = make_combiner("none")),
	contains = "sparse_",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@keys) && length(object@keys) != object@length )
			errors <- c(errors, paste0("'keys' must be NULL ",
				"OR match length of object [", object@length, "]"))
		if ( length(object@data) != length(object@index) )
			errors <- c(errors, paste0("length of 'data' [", length(object@data),
				"] must match length of 'index' [", length(object@index), "]"))
		if ( length(object@offset) != 1L )
			errors <- c(errors, "'offset' must be scalar (length 1)")
		if ( is.null(errors) ) TRUE else errors
	})

sparse_vec <- function(data, index, datamode = "double", length = 0,
					names = NULL, keys = NULL, offset = 1L,
					tolerance = c(abs=0), combiner = "none",
					chunksize = getOption("matter.default.chunksize"), ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(index) ) {
			nz <- data != 0
			length <- length(data)
			index <- which(nz) - 1L + offset
			data <- data[nz]
		}
	}
	if ( missing(length) ) {
		if ( is.null(keys) ) {
			length <- max(index) + 1 - offset
		} else {
			length <- length(keys)
		}
	}
	if ( length(index) != length(data) )
		index <- rep(index, length.out=length(data))
	if ( length(keys) > 1L && length(keys) != length )
		keys <- rep(keys, length.out=length)
	new("sparse_vec",
		data=data,
		datamode=make_datamode(datamode, type="R"),
		index=index,
		offset=as.integer(offset),
		keys=keys,
		paths=character(),
		filemode=make_filemode(),
		chunksize=as.integer(chunksize),
		length=length,
		names=names,
		tolerance=make_tolerance(tolerance),
		combiner=make_combiner(combiner))
}

setMethod("describe_for_display", "sparse_vec", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("sparse ", x@datamode, " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "sparse_vec", function(x) {
	hdr <- preview_vector_data(x)
	if ( is.null(colnames(x)) && !is.null(keys(x)) ) {
		n <- ncol(hdr)
		if ( colnames(hdr)[n] == "..." ) {
			colnames(hdr) <- c(paste0("(", keys(x)[1:(n - 1)], ")"), "...")
		} else {
			colnames(hdr) <- paste0("(", keys(x)[1:n], ")")
		}
	}
	print(hdr, quote=FALSE, right=TRUE)
	cat("(", nnz(x), "/", length(x), " non-zero elements: ",
		round(nnz(x) / length(x), 4) * 100, "% density)\n", sep="")
})

setMethod("nnz", "sparse_vec", function(x, ...) length(object@data))

extract_sparse_vec <- function(x, i = NULL) {
	i <- if ( is.null(i) ) NULL else i - 1
	y <- .Call("Mt_getSparseVector", x, i, PACKAGE="matter")
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
			extract_sparse_vec(x, i)
		} else {
			extract_sparse_vec(x)
		}
	})

