
#### Define sparse VIRTUAL class ####
## ----------------------------------

setClassUnion("matter_OR_list", c("matter_list", "list"))
setClassUnion("matter_OR_array", c("matter_arr", "numeric", "array"))
setClassUnion("matter_OR_array_OR_list", c("matter_OR_array", "matter_OR_list"))

setClass("sparse_arr",
	slots = c(
		data = "matter_OR_array_OR_list",
		index = "matter_OR_array_OR_list",
		pointers = "numeric_OR_NULL",
		domain = "numeric_OR_NULL",
		offset = "integer",
		tolerance = "numeric",
		sampler = "factor",
		ops = "ops_OR_NULL",
		transpose = "logical"),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "array must have non-NULL 'dim'")
		if ( length(object@dim) > 2L )
			errors <- c(errors, "sparse arrays are limited to 2 dimensions or fewer")
		if ( length(object@type) != 1L )
			errors <- c(errors, "'type' must be a scalar (length 1)")
		if ( length(object@transpose) != 1L )
			errors <- c(errors, "'transpose' must be a scalar (length 1)")
		if ( !is.null(object@domain) && anyNA(object@domain) )
			errors <- c(errors, "'domain' can't have missing values")
		if ( length(object@offset) != 1L )
			errors <- c(errors, "'offset' must be scalar (length 1)")
		if ( is(object@data, "matter_OR_list") && !is(object@index, "matter_OR_list") )
			errors <- c(errors, "'data' and 'index' are non-conformable classes")
		if ( is(object@data, "matter_OR_array") && !is(object@index, "matter_OR_array") )
			errors <- c(errors, "'data' and 'index' are non-conformable classes")
		if ( object@transpose ) {
			sdim <- object@dim[length(object@dim)]
			ddim <- prod(object@dim[-length(object@dim)])
		} else {
			sdim <- object@dim[1L]
			ddim <- prod(object@dim[-1L])
		}
		if ( is(object@data, "matter_OR_list") ) {
			# data and index are both lists
			if ( length(object@data) != length(object@index) )
				errors <- c(errors, paste0("length of 'data' [", length(object@data),
					"] must match length of 'index' [", length(object@index), "]"))
			if ( !all(lengths(object@data) == lengths(object@index)) )
				errors <- c(errors, "lengths of 'data' must match lengths of 'index'")
			if ( length(object@data) != ddim )
				errors <- c(errors, paste0("length of 'data' [", length(object@data),
					"] must match dense extent of array [", ddim, "]"))
		} else if ( !is.null(object@pointers) ) {
			# data and index are both arrays
			if ( length(object@data) != length(object@index) )
				errors <- c(errors, paste0("length of 'data' [", length(object@data),
					"] must match length of 'index' [", length(object@index), "]"))
			if ( length(object@data) != object@pointers[length(object@pointers)] )
				errors <- c(errors, paste0("last element of 'pointers' ",
					"[", object@pointers[length(object@pointers)], "] ",
						"must match length of 'data' [", length(object@data), "]"))
			if ( length(object@pointers) != ddim + 1 )
				errors <- c(errors, paste0("length of 'pointers' [", length(object@pointers),
					"] must match dense extent of array [", ddim, "] plus 1"))
		} else {
			# data is an array with shared index
			if ( is.matter(object@index) )
				errors <- c(errors, "shared 'index' must be in-memory (numeric or integer)")
			if ( length(object@data) %% length(object@index) != 0 )
				errors <- c(errors, "'index' and 'data' have non-conformable lengths")
			if ( length(object@data) / length(object@index) != ddim )
				errors <- c(errors, paste0("division of lengths of 'data' and 'index' must ",
					"match dense extent of array [", ddim, "]"))
		}
		if ( !is.null(object@domain) && length(object@domain) != sdim )
			errors <- c(errors, paste0("length of 'domain' [", length(object@domain),
				"] must match sparse extent of array [", sdim, "]"))
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_mat",
	contains = "sparse_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 2L )
			errors <- c(errors, "matrix must have exactly 2 dimensions")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_vec",
	contains = "sparse_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 1L )
			errors <- c(errors, "vector can't have more than 1 dimension")
		if ( is.null(errors) ) TRUE else errors
	})

sparse_vec <- function(data, index, type = "double",
	length = NA_integer_, names = NULL,
	domain = NULL, offset = 0L, rowMaj = FALSE,
	tolerance = c(abs=0), sampler = "none", ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( missing(type) && is.atomic(data) )
			type <- typeof(data)
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
		type=as_Rtype(type),
		index=index,
		pointers=NULL,
		domain=domain,
		offset=as.integer(offset),
		dim=length,
		names=names,
		tolerance=as_tol(tolerance),
		sampler=as_interp(sampler),
		ops=NULL,
		transpose=rowMaj, ...)
}

sparse_mat <- function(data, index, type = "double",
	nrow = NA_integer_, ncol = NA_integer_, dimnames = NULL,
	pointers = NULL, domain = NULL, offset = 0L, rowMaj = FALSE,
	tolerance = c(abs=0), sampler = "none", ...)
{
	if ( !missing(data) && is.matrix(data) )
	{
		if ( missing(type) )
			type <- typeof(data)
		if ( missing(nrow) )
			nrow <- nrow(data)
		if ( missing(ncol) )
			ncol <- ncol(data)
		if ( missing(index) ) {
			if ( rowMaj ) {
				index <- apply(data, 1L,
					function(x) {
						which(x != 0) - 1L + offset
					}, simplify=FALSE)
				data <- apply(data, 1L,
					function(x) {
						x[x != 0]
					}, simplify=FALSE)
			} else {
				index <- apply(data, 2L,
					function(x) {
						which(x != 0)  - 1L + offset
					}, simplify=FALSE)
				data <- apply(data, 2L,
					function(x) {
						x[x != 0]
					}, simplify=FALSE)
			}
			if ( isTRUE(pointers) ) {
				pointers <- c(0, cumsum(lengths(index)))
				index <- unlist(index)
				data <- unlist(data)
			}
		} else {
			if ( rowMaj ) {
				data <- as.vector(t(data))
			} else {
				data <- as.vector(data)
			}
		}
	}
	if ( is(data, "matter_OR_list") ) {
		# data and index are both lists
		if ( missing(nrow) && rowMaj )
			nrow <- length(data)
		if ( missing(nrow) && !rowMaj )
			nrow <- length(domain)
		if ( missing(ncol) && rowMaj )
			ncol <- length(domain)
		if ( missing(ncol) && !rowMaj )
			ncol <- length(data)
	} else if ( !is.null(pointers) ) {
		# data and index are both arrays
		if ( missing(nrow) && rowMaj )
			nrow <- length(pointers) - 1L
		if ( missing(nrow) && !rowMaj )
			nrow <- length(domain)
		if ( missing(ncol) && rowMaj )
			ncol <- length(domain)
		if ( missing(ncol) && !rowMaj )
			ncol <- length(pointers) - 1L
	} else {
		# data is an array with shared index
		if ( missing(nrow) && rowMaj )
			nrow <- length(data) / length(index)
		if ( missing(ncol) && !rowMaj )
			ncol <- length(data) / length(index)
	}
	new("sparse_mat",
		data=data,
		type=as_Rtype(type),
		index=index,
		pointers=pointers,
		domain=domain,
		offset=as.integer(offset),
		dim=as.integer(c(nrow, ncol)),
		dimnames=dimnames,
		tolerance=as_tol(tolerance),
		sampler=as_interp(sampler),
		ops=NULL,
		transpose=rowMaj, ...)
}

setMethod("describe_for_display", "sparse_mat", function(x) {
	desc1 <- paste0("<", x@dim[[1L]], " row x ", x@dim[[2L]], " col> ", class(x))
	desc2 <- paste0("sparse ", x@datamode[1L], " matrix")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "sparse_vec", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("sparse ", x@datamode[1L], " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "sparse_mat", function(x) {
	hdr <- preview_matrix_data(x)
	if ( x@transpose ) {
		if ( is.null(colnames(x)) && !is.null(domain(x)) ) {
			n <- ncol(hdr)
			if ( colnames(hdr)[n] == "..." ) {
				colnames(hdr) <- c(paste0("(,", domain(x)[1:(n - 1)], ")"), "...")
			} else {
				colnames(hdr) <- paste0("(,", domain(x)[1:n], ")")
			}
		}
	} else {
		if ( is.null(rownames(x)) && !is.null(domain(x)) ) {
			n <- nrow(hdr)
			if ( rownames(hdr)[n] == "..." ) {
				rownames(hdr) <- c(paste0("(", domain(x)[1:(n - 1)], ",)"), "...")
			} else {
				rownames(hdr) <- paste0("(", domain(x)[1:n], ",)")
			}
		}
	}
	print(hdr, quote=FALSE, right=TRUE)
	cat("(", nnzero(x), "/", prod(dim(x)), " non-zero elements: ",
		round(nnzero(x) / prod(dim(x)), 4) * 100, "% density)\n", sep="")
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

setAs("matrix", "sparse_mat",
	function(from) sparse_mat(from, type=typeof(from), dimnames=dimnames(from)))

setAs("array", "sparse_arr",
	function(from) sparse_mat(as.matrix(from), type=typeof(from), dimnames=dimnames(from)))

as.sparse <- function(x, ...) as(x, "sparse_arr")

is.sparse <- function(x) is(x, "sparse_arr")

setAs("sparse_arr", "matrix", function(from) from[])

setMethod("as.matrix", "sparse_arr", function(x) as(x, "matrix"))

setMethod("aindex", "sparse_arr", function(object) atomindex(object))

setMethod("atomindex", "sparse_arr", function(object) object@index)

setReplaceMethod("atomindex", "sparse_arr", function(object, value) {
	object@index <- value
	if ( validObject(object) )
		object
})

setMethod("pointers", "sparse_arr", function(object) object@pointers)

setReplaceMethod("pointers", "sparse_arr", function(object, value) {
	object@pointers <- value
	if ( validObject(object) )
		object
})

setMethod("domain", "sparse_arr", function(object) object@domain)

setReplaceMethod("domain", "sparse_arr", function(x, value) {
	x@domain <- value
	if ( object@transpose ) {
		smargin <- length(object@dim)
	} else {
		smargin <- 1L
	}
	x@dim[smargin] <- length(value)
	if ( !is.null(x@dimnames) )
		x@dimnames[smargin] <- list(NULL)
	if ( validObject(x) )
		x
})

setMethod("tolerance", "sparse_arr", function(object, ...) object@tolerance)

setReplaceMethod("tolerance", "sparse_arr", function(object, ..., value) {
	object@tolerance <- as_tol(value)
	object
})

setMethod("sampler", "sparse_arr", function(object, ...) object@sampler)

setReplaceMethod("sampler", "sparse_arr", function(object, ..., value) {
	object@sampler <- as_interp(value)
	object
})

setMethod("nnzero", "sparse_arr",
	function(x, na.counted = NA) {
		if ( !missing(na.counted) )
			warning("argument 'na.counted' will be ignored")
		if ( is(x@data, "matter_OR_list") ) {
			sum(lengths(x@data))
		} else {
			length(x@data)
		}
	})

get_sparse_mat_submatrix <- function(x, i = NULL, j = NULL, drop = FALSE) {
	stop("not implemented yet")
	# y <- .Call(C_getSparseMatrix, x, i, j)
	y <- set_dimnames(y, dimnames(x), list(i, j))
	if ( drop )
		y <- drop(y)
	y
}

set_sparse_mat_submatrix <- function(x, i = NULL, j = NULL, value = 0)
{
	stop("not implemented yet")
	# .Call(C_setSparseMatrix, x, i, j, value, PACKAGE="matter")
}

setMethod("[",
	c(x = "sparse_mat", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, j, ..., drop) {
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		if ( missing(i) )
			i <- NULL
		if ( missing(j) )
			j <- NULL
		y <- get_sparse_mat_elts(x, i, j)
		if ( isTRUE(drop) )
			y <- drop(y)
		y
	})

setReplaceMethod("[",
	c(x = "sparse_mat", i = "ANY", j = "ANY", value = "ANY"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2
		if ( !missing(i) && narg == 1 )
			stop("linear indexing not supported")
		if ( narg > 1 && narg != length(dim(x)) )
			stop("incorrect number of dimensions")
		stop("assignment not implemented yet")
	})

setMethod("t", "sparse_arr", function(x)
{
	x@transpose <- !x@transpose
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

setMethod("%*%", c("sparse_mat", "matrix"), function(x, y)
{
	if ( x@transpose ) {
		rightMatrixMult(x, y, useOuter=FALSE)
	} else {
		rightMatrixMult(x, y, useOuter=TRUE)
	}
})

setMethod("%*%", c("matrix", "sparse_mat"), function(x, y)
{
	if ( y@transpose ) {
		leftMatrixMult(x, y, useOuter=TRUE)
	} else {
		leftMatrixMult(x, y, useOuter=FALSE)
	}
	
})
