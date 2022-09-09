
#### 'matter_arr' class for file-based arrays ####
## ------------------------------------------------

setClass("matter2_arr",
	slots = c(
		ops = "list_OR_NULL",
		transpose = "logical"),
	contains = "matter2_",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "array must have non-NULL 'dim'")
			if ( length(object@type) != 1L )
			errors <- c(errors, "'type' must be a scalar (length 1)")
		if ( length(object@transpose) != 1L )
			errors <- c(errors, "'transpose' must be a scalar (length 1)")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter2_mat",
	contains = "matter2_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 2L )
			errors <- c(errors, "matrix must have exactly 2 dimensions")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter2_vec",
	contains = "matter2_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 1L )
			errors <- c(errors, "vector can't have more than 1 dimension")
		if ( is.null(errors) ) TRUE else errors
	})

matter2_arr <- function(data, type = "double", path = NULL,
	dim = NA_integer_, dimnames = NULL, offset = 0, extent = NA_real_,
	readonly = NA, rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		if ( anyNA(type) )
			type <- typeof(data)
		if ( anyNA(dim) && is.vector(data) ) {
			dim <- length(data)
		} else if ( anyNA(dim) ) {
			dim <- dim(data)
		}
		if ( is.null(dimnames) )
			dimnames <- dimnames(data)
	}
	if ( is.null(path) )
		path <- tempfile(tmpdir=getOption("matter.dump.dir"), fileext=".bin")
	path <- normalizePath(path, mustWork=FALSE)
	exists <- file.exists(path)
	if ( is.na(readonly) )
		readonly <- all(exists)
	if ( any(exists) && !readonly && !missing(data) )
		warning("data may overwrite existing file(s): ",
			paste0(sQuote(path[exists]), collapse=", "))
	if ( all(exists) && missing(data) ) {
		if ( anyNA(dim) && anyNA(extent) ) {
			sizes <- file.size(path)
			# FIXME: can we infer the NA dims instead of overwriting?
			dim <- sum((sizes - offset) %/% sizeof(type))
		}
	}
	if ( anyNA(dim) && anyNA(extent) ) {
		extent <- dim <- rep.int(0, length(dim))
	} else if ( anyNA(extent) ) {
		extent <- prod(dim)
	} else if ( anyNA(dim) ) {
		dim <- sum(extent)
	}
	if ( length(offset) != length(extent) && length(path) == 1L ) {
		sizes <- sizeof(type) * extent
		offset <- cumsum(c(offset, sizes[-length(sizes)]))
	}
	if ( any(!exists) ) {
		if ( missing(data) && any(extent > 0) )
			warning("creating uninitialized backing file(s): ",
				paste0(sQuote(path[!exists]), collapse=", "))
		success <- file.create(path)
		if ( !all(success) )
			stop("error creating file(s): ",
				paste0(sQuote(path[!success]), collapse=", "))
	}
	x <- new("matter2_arr",
		data=atoms2(
			source=path,
			type=as_Ctype(type),
			offset=as.double(offset),
			extent=as.double(extent),
			group=0L,
			readonly=readonly),
		type=collapse_Rtype(type),
		dim=dim,
		dimnames=dimnames,
		ops=NULL,
		transpose=rowMaj, ...)
	if ( !missing(data) && is.vector(data) ) {
		x <- as(x, "matter2_vec")
	} else if ( length(dim) == 2L ) {
		x <- as(x, "matter2_mat")
	}
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

matter2_mat <- function(data, type = "double", path = NULL,
	nrow = NA_integer_, ncol = NA_integer_, dimnames = NULL,
	offset = 0, extent = NA_real_, readonly = NA, rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		if ( anyNA(type) )
			type <- typeof(data)
		if ( is.na(nrow) && is.na(ncol) ) {
			nrow <- nrow(data)
			ncol <- ncol(data)
		} else if ( is.na(nrow) ) {
			nrow <- length(data) / ncol
		} else if ( is.na(ncol) ) {
			ncol <- length(data) / nrow
		}
	}
	x <- matter2_arr(data, type=type, path=path, dim=c(nrow, ncol),
		dimnames=dimnames, offset=offset, extent=extent,
		readonly=readonly, rowMaj=rowMaj, ...)
	as(x, "matter2_mat")
	if ( validObject(x) )
		x
}

matter2_vec <- function(data, type = "double", path = NULL,
	length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		if ( anyNA(type) )
			type <- typeof(data)
		if ( is.na(length) )
			length <- length(data)
		if ( is.null(names) )
			names <- names(data)
	}
	x <- matter2_arr(data, type=type, path=path, dim=length,
		names=names, offset=offset, extent=extent,
		readonly=readonly, rowMaj=rowMaj, ...)
	x <- as(x, "matter2_vec")
	if ( validObject(x) )
		x
}

setMethod("describe_for_display", "matter2_arr", function(x) {
	desc1 <- paste0("<", paste0(dim(x), collapse=" x "), " dim> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " array")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "matter2_mat", function(x) {
	desc1 <- paste0("<", nrow(x), " row x ", ncol(x), " col> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " matrix")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "matter2_vec", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter2_arr", function(x) {
	if ( length(dim(x)) <= 1L ) {
		preview_vector(x)
	} else if ( length(dim(x)) == 2L ) {
		preview_matrix(x)
	} else {
		preview_Nd_array(x)
	}
})

get_matter_arr_elts <- function(x, i = NULL) {
	.Call("C_getMatterArray", x, i, PACKAGE="matter")
}

set_matter_arr_elts <- function(x, i = NULL, value = 0L) {
	.Call("C_setMatterArray", x, i, value, PACKAGE="matter")
}

get_matter_arr_subarray <- function(x, index, drop = FALSE)
{
	index <- as_array_subscripts(index, x)
	i <- linear_ind(index, dim(x))
	y <- get_matter_arr_elts(x, i)
	dim(y) <- vapply(seq_along(index), function(j)
		if (is.null(index[[j]])) dim(x)[j]
		else length(index[[j]]), numeric(1))
	y <- set_dimnames(y, dimnames(x), index)
	y <- set_names(y, names(x), i)
	if ( drop )
		y <- drop(y)
	y
}

set_matter_arr_subarray <- function(x, index, value)
{
	index <- as_array_subscripts(index, x)
	i <- linear_ind(index, dim(x))
	set_matter_arr_elts(x, i, value)
}

setMethod("[", c(x = "matter2_arr"),
	function(x, i, j, ..., drop = TRUE) {
		narg <- nargs() - 1L - !missing(drop)
		if ( (narg == 1L && !missing(i)) || is.null(dim(x)) ) {
			if ( missing(i) ) i <- NULL
			get_matter_arr_elts(x, i)
		} else {
			if ( narg != 1L && narg != length(dim(x)) )
				stop("incorrect number of dimensions")
			if ( missing(i) ) i <- NULL
			if ( missing(j) ) j <- NULL
			if ( ...length() > 0 ) {
				more.ind <- eval(substitute(alist(...)))
				more.ind <- lapply(more.ind, function(elt) 
					if (identical(elt, quote(expr=))) NULL else eval(elt))
			} else {
				more.ind <- NULL
			}
			if ( length(dim(x)) >= 2 ) {
				index <- c(list(i, j), more.ind)
			} else {
				index <- list(i)
			}
			get_matter_arr_subarray(x, index, drop)
		}
	})

setReplaceMethod("[", c(x = "matter2_arr"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2L
		if ( (narg == 1L && !missing(i)) || is.null(dim(x)) ) {
			if ( missing(i) ) i <- NULL
			set_matter_arr_elts(x, i, value)
		} else {
			if ( narg != 1L && narg != length(dim(x)) )
				stop("incorrect number of dimensions")
			if ( missing(i) ) i <- NULL
			if ( missing(j) ) j <- NULL
			if ( ...length() > 0 ) {
				more.ind <- eval(substitute(alist(...)))
				more.ind <- lapply(more.ind, function(elt) 
					if (identical(elt, quote(expr=))) NULL else eval(elt))
			} else {
				more.ind <- NULL
			}
			if ( length(dim(x)) >= 2 ) {
				index <- c(list(i, j), more.ind)
			} else {
				index <- list(i)
			}
			set_matter_arr_subarray(x, index, value)
		}
	})

setMethod("t", "matter2_arr", function(x)
{
	x@transpose <- !x@transpose
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

setMethod("dim", "matter2_vec", function(x) NULL)

setReplaceMethod("dim", "matter2_arr", function(x, value) {
	if ( is.null(value) ) {
		x@transpose <- FALSE
		x@dim <- prod(x@dim)
		x@dimnames <- NULL
		as(x, "matter2_vec")
	} else {
		callNextMethod()
	}
})

setReplaceMethod("dim", "matter2_vec", function(x, value) {
	if ( !is.null(value) )
		x <- as(x, "matter2_arr")
	callNextMethod(x, value)
})

