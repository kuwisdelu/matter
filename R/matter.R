
#### Define matter VIRTUAL class ####
## ----------------------------------

setClassUnion("atoms_OR_list", c("atoms", "list"))
setClassUnion("character_OR_NULL", c("character", "NULL"))
setClassUnion("integer_OR_NULL", c("integer", "NULL"))
setClassUnion("list_OR_NULL", c("list", "NULL"))

setClass("matter",
	slots = c(
		data = "atoms_OR_list",
		datamode = "factor",
		paths = "character",
		filemode = "factor",
		chunksize = "integer",
		length = "numeric",
		dim = "integer_OR_NULL",
		names = "character_OR_NULL",
		dimnames = "list_OR_NULL",
		ops = "list_OR_NULL"),
	contains = "VIRTUAL",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@paths) && any(!file.exists(object@paths)) )
			errors <- c(errors, paste0("file [", which(!file.exists(object@paths)), "] does not exist"))
		IO_filemodes <- levels(make_filemode())
		object_filemodes <- object@filemode[!is.na(object@filemode)]
		if ( length(object@filemode) != 1 || !all(object_filemodes %in% IO_filemodes) )
			errors <- c(errors, paste0("'filemode' should include only [",
				paste(IO_filemodes, collapse=", "), "]"))
		R_datamodes <- levels(make_datamode(type="R"))
		if ( !all(levels(object@datamode) %in% R_datamodes) )
			errors <- c(errors, paste0("'datamode' levels should be [",
				paste(R_datamodes, collapse=", "), "]"))
		if ( !object@chunksize > 0L )
			errors <- c(errors, "chunksize must be positive")
		if ( !is.null(object@names) && length(object@names) != object@length )
			errors <- c(errors, paste0("names [length ", length(object@names), "] ",
				"do not match length of object [", object@length, "]"))
		if ( !is.null(dimnames) && is.null(dim) )
			errors <- c(errors, "'dimnames' applied to non-array")
		if ( !is.null (object@dimnames) ) {
			if ( is.null(object@dim) )
				errors <- c(errors, "'dimnames' applied to non-array")
			if ( length(object@dimnames) != length(object@dim) )
				errors <- c(errors, paste0("length of 'dimnames' [", length(object@dimnames), "] ",
					"must match that of 'dims' [", length(object@dim), "]"))
			for ( i in seq_along(object@dimnames) ) {
				dmn <- object@dimnames[[i]]
				if ( !is.null(dmn) && length(dmn) != object@dim[i] )
					errors <- c(errors, paste0("length of 'dimnames' [", i, "] ",
						"not equal to array extent"))
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

matter <- function(...) {
	dots <- match.call(expand.dots=FALSE)$...
	nm <- names(dots)
	if ( is.null(nm) || nchar(nm[[1]]) == 0 ) {
		data <- eval(dots[[1]])
	} else if ( "data" %in% nm ) {
		data <- eval(dots$data)
	} else {
		data <- NULL
	}
	if ( nargs() == 1 && !is.null(data) )
		return(as.matter(data))
	vec.args <- c("length", "names")
	arr.args <- c("dim", "dimnames")
	mat.args <- c("nrow", "ncol", "rowMaj")
	list.args <- c("lengths")
	char.args <- c("nchar")
	fc.args <- c("levels")
	known.args <- c(vec.args, arr.args, mat.args,
		list.args, char.args, fc.args)
	if ( any(nm %in% known.args) ) {
		if ( any(vec.args %in% nm) ) {
			matter_vec(...)
		} else if ( any(arr.args %in% nm) ) {
			matter_arr(...)
		} else if ( any(mat.args %in% nm) ) {
			matter_mat(...)
		} else if ( any(list.args %in% nm) ) {
			matter_list(...)
		} else if ( any(char.args %in% nm) ) {
			matter_str(...)
		} else if ( any(fc.args %in% nm) ) {
			matter_fc(...)
		} else {
			stop("couldn't guess data structure, use 'matter_' functions")
		}
	} else if ( !is.null(data) ) {
		if ( is.raw(data) || is.logical(data) || is.integer(data) || is.numeric(data) ) {
			matter_vec(...)
		} else if ( is.array(data) ) {
			matter_arr(...)
		} else if ( is.matrix(data) ) {
			matter_mat(...)
		} else if ( is.list(data) ) {
			matter_list(...)
		} else if ( is.character(data) ) {
			matter_str(...)
		} else if ( is.factor(data) ) {
			matter_fc(...)
		} else if ( is.data.frame(data) ) {
			matter_df(...)
		} else {
			stop("couldn't guess data structure, use 'matter_' functions")
		}
	} else {
		stop("couldn't guess data structure, use 'matter_' functions")
	}
}

setMethod("describe_for_display", "ANY", function(x) class(x))

setMethod("preview_for_display", "ANY", function(x) head(x))

is.matter <- function(x) {
	is(x, "matter")
}

as.matter <- function(x, ...) {
	switch(class(x),
		raw = as.matter_vec(x),
		logical = as.matter_vec(x),
		integer = as.matter_vec(x),
		numeric = as.matter_vec(x),
		character = as.matter_str(x),
		factor = as.matter_fc(x),
		matrix = as.matter_mat(x),
		array = as.matter_arr(x),
		list = as.matter_list(x),
		data.frame = as.matter_df(x),
		stop(paste0("cannot coerce class '", class(x),
			"' to a 'matter' object")))
}

setMethod("adata", "matter", function(object) atomdata(object))

setMethod("atomdata", "matter", function(object) object@data)

setReplaceMethod("atomdata", "matter", function(object, value) {
	object@data <- value
	object
})

setMethod("show", "matter", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	if ( getOption("matter.show.head") )
		preview_for_display(object)
	if ( is.matter(object) ) {
		object.memory <- object.size(object)
		class(object.memory) <- "num_bytes"
		rmem <- format(object.memory, units="auto")
		vmem <- format(vm_used(object), units="auto")
		cat("(", rmem, " real", " | ", vmem, " virtual)\n", sep="")
	}
})

setMethod("datamode", "matter", function(x) x@datamode)

setReplaceMethod("datamode", "matter", function(x, value) {
	x@datamode <- make_datamode(value, type="R")
	x
})

setMethod("paths", "matter", function(x) x@paths)

setReplaceMethod("paths", "matter", function(x, value) {
	x@paths <- normalizePath(value, mustWork=FALSE)
	x
})

setMethod("filemode", "matter", function(x) x@filemode)

setReplaceMethod("filemode", "matter", function(x, value) {
	x@filemode <- make_filemode(value)
	x
})

setMethod("readonly", "matter", function(x) x@filemode == "r")

setReplaceMethod("readonly", "matter", function(x, value) {
	if ( isTRUE(value) ) {
		x@filemode <- make_filemode("r")
	} else {
		x@filemode <- make_filemode("rw")
	}
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
	if ( !is.null(x@names) )
		x@names <- NULL
	if ( !is.null(x@dimnames) )
		x@dimnames <- NULL
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

#### Additional methods ####
## ------------------------

setMethod("c", "matter", function(x, ...)
{
	dots <- list(...)
	if ( length(dots) > 0 ) {
		combine(x, ...)
	} else {
		x
	}
})

setMethod("cbind", "matter", function(..., deparse.level=1)
{
	dots <- list(...)
	if ( length(dots) > 1 ) {
		combine_by_cols(...)
	} else {
		dots[[1]]
	}
})

setMethod("rbind", "matter", function(..., deparse.level=1)
{
	dots <- list(...)
	if ( length(dots) > 1 ) {
		combine_by_rows(...)
	} else {
		dots[[1]]
	}
})

setMethod("which", "matter",
	function(x, arr.ind = FALSE, useNames = TRUE, ...) {
		if ( datamode(x)[1] != "logical" )
			stop("argument to 'which' is not logical")
		wh <- .Call("C_getWhich", x, PACKAGE="matter")
		if ( arr.ind && !is.null(dim(x)) )  {
			arrayInd(wh, dim(x), dimnames(x), useNames=useNames)
		} else {
			wh
		}
})

#### Checksum ####
## ----------------

setMethod("checksum", "matter",
	function(x, algo=c("sha1", "md5"), ...)
{
	algo <- match.arg(algo)
	hash <- sapply(paths(x), function(file) {
		digest(file=file, algo=algo)
	})
	attr(hash, "algo") <- algo
	hash
})

