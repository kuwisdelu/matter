
#### Define matter<sparse vector> classes for sparse data ####
## -----------------------------------------------------------

setClass("sparse_vec",
	slots = c(
		data = "matter_OR_numeric",
		index = "matter_OR_numeric",
		pointers = "NULL",
		dim = "NULL",
		dimnames = "NULL"),
	prototype = prototype(
		data = numeric(),
		datamode = make_datamode("numeric", type="R"),
		index = integer(),
		domain = NULL,
		offset = 0L,
		length = 0,
		names = NULL,
		tolerance = make_tolerance(0),
		sampler = make_sampler("none")),
	contains = "sparse_",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@domain) && length(object@domain) != object@length )
			errors <- c(errors, paste0("'domain' must be NULL ",
				"OR match length of object [", object@length, "]"))
		if ( length(object@data) != length(object@index) )
			errors <- c(errors, paste0("length of 'data' [", length(object@data),
				"] must match length of 'index' [", length(object@index), "]"))
		if ( is.null(errors) ) TRUE else errors
	})

sparse_vec <- function(data, index, datamode = "double", length = 0,
					names = NULL, domain = NULL, offset = 1L,
					tolerance = c(abs=0), sampler = "none",
					chunksize = getOption("matter.default.chunksize"), ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) && is.atomic(data) )
			datamode <- typeof(data)
		if ( missing(index) ) {
			nz <- data != 0
			length <- length(data)
			index <- which(nz) - 1L + offset
			data <- data[nz]
		}
	}
	if ( missing(length) ) {
		if ( is.null(domain) ) {
			length <- max(index) + 1 - offset
		} else {
			length <- length(domain)
		}
	}
	new("sparse_vec",
		data=data,
		datamode=make_datamode(datamode, type="R"),
		index=index,
		domain=domain,
		offset=as.integer(offset),
		paths=character(),
		filemode=make_filemode(),
		chunksize=as.integer(chunksize),
		length=length,
		names=names,
		tolerance=make_tolerance(tolerance),
		sampler=make_sampler(sampler))
}

setMethod("describe_for_display", "sparse_vec", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("sparse ", x@datamode[1L], " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "sparse_vec", function(x) {
	hdr <- preview_vector_data(x)
	if ( is.null(colnames(x)) && !is.null(domain(x)) ) {
		n <- ncol(hdr)
		if ( colnames(hdr)[n] == "..." ) {
			colnames(hdr) <- c(paste0("(", domain(x)[1:(n - 1)], ")"), "...")
		} else {
			colnames(hdr) <- paste0("(", domain(x)[1:n], ")")
		}
	}
	print(hdr, quote=FALSE, right=TRUE)
	cat("(", nnzero(x), "/", length(x), " non-zero elements: ",
		round(nnzero(x) / length(x), 4) * 100, "% density)\n", sep="")
})

setReplaceMethod("domain", "sparse_vec", function(object, value) {
	object@domain <- value
	object@length <- length(value)
	if ( validObject(object) )
		object
})

setMethod("nnzero", "sparse_vec",
	function(x, na.counted = NA) length(x@data))

get_sparse_vec_elts <- function(x, i = NULL)
{
	y <- .Call("C_getSparseVector", x, i, PACKAGE="matter")
	if ( !is.null(names(x)) ) {
		if ( !is.null(i) ) {
			names(y) <- names(x)[i]
		} else {
			names(y) <- names(x)
		}
	}
	y
}

setMethod("[",
	c(x = "sparse_vec", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop)
	{
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		if ( missing(i) )
			i <- NULL
		get_sparse_vec_elts(x, i)
	})

