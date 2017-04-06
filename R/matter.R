
#### Define matter VIRTUAL class ####
## ----------------------------------

setClassUnion("character_OR_NULL", c("character", "NULL")) # imported from S4Vectors
setClassUnion("integer_OR_NULL", c("integer", "NULL"))
setClassUnion("list_OR_NULL", c("list", "NULL"))

setClass("matter",
	slots = c(
		data = "atoms",
		datamode = "factor",
		paths = "character",
		filemode = "character",
		chunksize = "integer",
		length = "numeric",
		dim = "integer_OR_NULL",
		names = "character_OR_NULL",
		dimnames = "list_OR_NULL"),
	contains = "VIRTUAL",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@paths) && any(!file.exists(object@paths)) )
			errors <- c(errors, "file [", which(!file.exists(object@paths)), "] does not exist")
		C_readmodes <- c("rb", "rb+")
		if ( length(object@filemode) != 1 || !object@filemode %in% C_readmodes )
			errors <- c(errors, "'filemode' should be one of [",
				paste(C_readmodes, collapse=", "), "]")
		R_datamodes <- levels(make_datamode(type="R"))
		if ( !as.character(object@datamode) %in% R_datamodes )
			errors <- c(errors, "'datamode' should be one of [",
				paste(R_datamodes, collapse=", "), "]")
		if ( !object@chunksize > 0L )
			errors <- c(errors, "chunksize must be positive")
		if ( !is.null(object@dim) && prod(object@dim) != object@length )
			errors <- c(errors, "dims [product ", prod(object@dim), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(object@names) && length(object@names) != object@length )
			errors <- c(errors, "names [length ", length(object@names), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(dimnames) && is.null(dim) )
			errors <- c(errors, "'dimnames' applied to non-array")
		if ( !is.null (object@dimnames) ) {
			if ( is.null(object@dim) )
				errors <- c(errors, "'dimnames' applied to non-array")
			if ( length(object@dimnames) != length(object@dim) )
				errors <- c(errors, "length of 'dimnames' [", length(object@dimnames), "] ",
					"must match that of 'dims' [", length(object@dim), "]")
			for ( i in seq_along(object@dimnames) ) {
				dmn <- object@dimnames[[i]]
				if ( !is.null(dmn) && length(dmn) != object@dim[i] )
					errors <- c(errors, "length of 'dimnames' [", i, "] ",
						"not equal to array extent")
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

matter <- function(...) {
	dots <- match.call(expand.dots=FALSE)$...
	nm <- names(dots)
	if ( "data" %in% nm ) {
		data <- dots$data
	} else {
		data <- eval(dots[[1]])
	}
	if ( "extent" %in% nm ) {
		uneq.extent <- length(unique(eval(dots$extent))) > 1
	} else {
		uneq.extent <- FALSE
	}
	vec.args <- c("length", "names")
	mat.args <- c("nrow", "ncol", "dimnames", "rowMaj")
	if ( any(vec.args %in% nm ) || is.vector(dots) || uneq.extent ) {
		matter_vec(...)
	} else if ( any(mat.args %in% nm ) || is.matrix(dots) ) {
		matter_mat(...)
	} else {
		matter_vec(...)
	}
}

setMethod("adata", "matter", function(object) atomdata(object))

setMethod("atomdata", "matter", function(object) object@data)

setMethod("show", "matter", function(object) {
	object.memory <- object.size(object)
	class(object.memory) <- "bytes"
	cat("    sources: ", length(object@paths), "\n", sep="")
	cat("    datamode: ", paste(object@datamode), "\n", sep="")
	cat("    ", format(object.memory, units="auto"), " in-memory\n", sep="")
	cat("    ", format(disk_used(object@data), units="auto"), " on-disk\n", sep="")
})

setMethod("datamode", "matter", function(x) x@datamode)

setReplaceMethod("datamode", "matter", function(x, value) {
	x@datamode <- make_datamode(value, type="R")
	x
})

setMethod("paths", "matter", function(x) x@paths)

setReplaceMethod("paths", "matter", function(x, value) {
	x@paths <- value
	x
})

setMethod("filemode", "matter", function(x) x@filemode)

setReplaceMethod("filemode", "matter", function(x, value) {
	x@filemode <- value
	x
})

setMethod("chunksize", "matter", function(x) x@chunksize)

setReplaceMethod("chunksize", "matter", function(x, value) {
	x@chunksize <- as.integer(value)
	x
})

setMethod("length", "matter", function(x) x@length)

setReplaceMethod("length", "matter", function(x, value) {
	stop("cannot change length of 'matter' object")
})

setMethod("dim", "matter", function(x) x@dim)

setReplaceMethod("dim", "matter", function(x, value) {
	if ( !is.null(value) )
		value <- as.integer(value)
	x@dim <- value
	if ( validObject(x) )
		x
})

setMethod("names", "matter", function(x) x@names)

setReplaceMethod("names", "matter", function(x, value) {
	if ( !is.null(value) )
		value <- as.character(value)
	x@names <- value
	if ( validObject(x) )
		x
})

setMethod("dimnames", "matter", function(x) x@dimnames)

setReplaceMethod("dimnames", "matter", function(x, value) {
	x@dimnames <- value
	if ( validObject(x) )
		x
})
