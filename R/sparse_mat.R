
#### Define matter<sparse vector> classes for sparse data ####
## -----------------------------------------------------------

setClassUnion("matter_list_types", c("list", "matter_list"))

setClass("sparse_mat",
	slots = c(pointers = "numeric_OR_NULL"),
	prototype = prototype(
		pointers = NULL,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL),
	contains = c("sparse_", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "sparse matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "sparse matrix must have 'dim' of length 2")
		if ( prod(object@dim) != object@length )
			errors <- c(errors, paste0("dims [product ", prod(object@dim),
				"] do not match the length of array [", object@length, "]"))
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matl",
	slots = c(
		data = "matter_list_types",
		index = "matter_list_types",
		keys = "numeric",
		pointers = "NULL"),
	prototype = prototype(
		data = list(),
		index = list(),
		keys = 0L,
		pointers = NULL),
	contains = c("sparse_mat", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( !all(lengths(object@data) == lengths(object@index)) )
			errors <- c(errors, "lengths of 'data' must match lengths of 'index'")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matlc",
	contains = "sparse_matl",
	validity = function(object) {
		errors <- NULL
		if ( length(object@data) != object@dim[2L] )
			errors <- c(errors, paste0("length of 'data' must ",
				"match ncol of object [", object@dim[2L], "]"))
		if ( length(object@index) != object@dim[2L] )
			errors <- c(errors, paste0("length of 'index' must ",
				"match ncol of object [", object@dim[2L], "]"))
		if ( length(object@keys) != 1L && length(object@keys) != object@dim[1L] )
			errors <- c(errors, paste0("'keys' must be scalar ",
				"OR match nrow of object [", object@dim[1L], "]"))
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matlr",
	contains = "sparse_matl",
	validity = function(object) {
		errors <- NULL
		if ( length(object@data) != object@dim[1L] )
			errors <- c(errors, paste0("length of 'data' must ",
				"match nrow of object [", object@dim[1L], "]"))
		if ( length(object@index) != object@dim[1L] )
			errors <- c(errors, paste0("length of 'index' must ",
				"match nrow of object [", object@dim[1L], "]"))
		if ( length(object@keys) != 1L && length(object@keys) != object@dim[2L] )
			errors <- c(errors, paste0("'keys' must be scalar ",
				"OR match ncol of object [", object@dim[2L], "]"))
		if ( is.null(errors) ) TRUE else errors
	})

sparse_mat <- function(data, index, datamode = "double", nrow = 0, ncol = 0,
					rowMaj = FALSE, dimnames = NULL, keys = NULL, from0 = FALSE,
					tolerance = c(abs=0), combiner = "none",
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

# setMethod("describe_for_display", "sparse_vec", function(x) {
# 	desc1 <- paste0("<", x@length, " length> ", class(x))
# 	desc2 <- paste0("sparse ", x@datamode, " vector")
# 	paste0(desc1, " :: ", desc2)
# })

# setMethod("preview_for_display", "sparse_vec", function(x) {
# 	hdr <- preview_vector_data(x)
# 	if ( is.null(colnames(x)) && length(keys(x)) == length(x) ) {
# 		n <- ncol(hdr)
# 		if ( colnames(hdr)[n] == "..." ) {
# 			colnames(hdr) <- c(paste0("[~", keys(x)[1:(n - 1)], "]"), "...")
# 		} else {
# 			colnames(hdr) <- paste0("[~", keys(x)[1:n], "]")
# 		}
# 	}
# 	print(hdr, quote=FALSE, right=TRUE)
# 	cat("(", nnz(x), "/", length(x), " non-zero elements: ",
# 		round(nnz(x) / length(x), 4) * 100, "% density)\n", sep="")
# })

setMethod("nnz", "sparse_mat", function(x, ...) sum(lengths(object@data)))

extract_sparse_mat <- function(x, i = NULL) {
	# not implemented yet
}

setMethod("[",
	c(x = "sparse_mat", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ..., drop) {
		# not implemented yet
	})
