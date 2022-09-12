
#### Define matter<sparse vector> classes for sparse data ####
## -----------------------------------------------------------

setClass("sparse_mat",
	slots = c(
		data = "matter_OR_numeric_OR_list",
		index = "matter_OR_numeric_OR_list",
		dim = "integer",
		names = "NULL",
		length = "NULL"),
	contains = c("sparse_", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 2 )
			errors <- c(errors, "sparse matrix must have 'dim' of length 2")
		if ( is(object@data, "matter_OR_numeric") && !is(object@index, "matter_OR_numeric") )
			errors <- c(errors, "'index' and 'data' are non-conformable types")
		if ( is(object@data, "matter_OR_list") && !is(object@index, "matter_OR_list") )
			errors <- c(errors, "'index' and 'data' are non-conformable types")
		if ( is.null(object@pointers) ) {
			if ( is(object@data, "matter_OR_list") ) {
				# data and index are both lists
				if ( length(object@data) != length(object@index) )
					errors <- c(errors, paste0("length of 'data' [", length(object@data),
						"] must match length of 'index' [", length(object@index), "]"))
				if ( !all(lengths(object@data) == lengths(object@index)) )
					errors <- c(errors, "lengths of 'data' must match lengths of 'index'")
			} else {
				# data is numeric with shared index
				if ( is.matter(object@index) )
					errors <- c(errors, "shared 'index' must be in-memory (numeric or integer)")
				if ( length(object@data) %% length(object@index) != 0 )
					errors <- c(errors, "'index' and 'data' have non-conformable lengths")
			}
		} else {
			# data and index are both numeric
			if ( length(object@data) != length(object@index) ) {
				if ( length(object@data) != length(object@index) )
					errors <- c(errors, paste0("length of 'data' [", length(object@data),
						"] must match length of 'index' [", length(object@index), "]"))
				if ( length(object@data) != object@pointers[length(object@pointers)] )
					errors <- c(errors, paste0("last element of 'pointers' [",
						object@pointers[length(object@pointers)],
						"] must match length of 'data' [",
						length(object@data), "]"))
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matc",
	contains = "sparse_mat",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@pointers) ) {
			if ( is(object@data, "matter_OR_list") ) {
				# data and index are both lists
				if ( length(object@data) != object@dim[2L] )
					errors <- c(errors, paste0("length of 'data' [", length(object@data),
						"] must match ncol of object [", object@dim[2L], "]"))
			} else {
				# data is numeric with shared index
				if ( length(object@data) / length(object@index) != object@dim[2L] )
					errors <- c(errors, paste0("division of lengths of 'data' and 'index' must ",
						"match ncol of object [", object@dim[2L], "]"))
			}
		} else {
			# data and index are both numeric
			if ( length(object@pointers) != object@dim[2L] + 1 )
				errors <- c(errors, paste0("length of 'pointers' [", length(object@pointers),
					"] must match ncol of object [", object@dim[2L], "] plus 1"))
		}
		if ( !is.null(object@domain) && length(object@domain) != object@dim[1L] )
			errors <- c(errors, paste0("length of 'domain' [", length(object@domain),
				"] must match nrow of object [", object@dim[1L], "]"))
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matr",
	contains = "sparse_mat",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@pointers) ) {
			if ( is(object@data, "matter_OR_list") ) {
				# data and index are both lists
				if ( length(object@data) != object@dim[1L] )
					errors <- c(errors, paste0("length of 'data' [", length(object@data),
						"] must match nrow of object [", object@dim[1L], "]"))
			} else {
				# data is numeric with shared index
				if ( length(object@data) / length(object@index) != object@dim[1L] )
					errors <- c(errors, paste0("division of lengths of 'data' and 'index' must ",
						"match nrow of object [", object@dim[1L], "]"))
			}
		} else {
			# data and index are both numeric
			if ( length(object@pointers) != object@dim[1L] + 1 )
				errors <- c(errors, paste0("length of 'pointers' [", length(object@pointers),
					"] must match nrow of object [", object@dim[1L], "] plus 1"))
		}
		if ( !is.null(object@domain) && length(object@domain) != object@dim[2L] )
			errors <- c(errors, paste0("length of 'domain' [", length(object@domain),
				"] must match ncol of object [", object@dim[2L], "]"))
		if ( is.null(errors) ) TRUE else errors
	})

sparse_mat <- function(data, index, datamode = "double", nrow = 0, ncol = 0,
					rowMaj = FALSE, dimnames = NULL, domain = NULL, offset = 1L,
					tolerance = c(abs=0), sampler = "none", pointers = NULL,
					chunksize = getOption("matter.default.chunksize"), ...)
{
	if ( !missing(data) && is.matrix(data) )
	{
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(nrow) )
			nrow <- nrow(data)
		if ( missing(ncol) )
			ncol <- ncol(data)
		if ( missing(index) ) {
			if ( rowMaj ) {
				index <- apply(data, 1,
					function(x) {
						which(x != 0) - 1L + offset
					}, simplify=FALSE)
				data <- apply(data, 1,
					function(x) {
						x[x != 0]
					}, simplify=FALSE)
			} else {
				index <- apply(data, 2,
					function(x) {
						which(x != 0)  - 1L + offset
					}, simplify=FALSE)
				data <- apply(data, 2,
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
	if ( is.null(pointers) ) {
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
		} else {
			# data is numeric with shared index
			if ( missing(nrow) && rowMaj )
				nrow <- length(data) / length(index)
			if ( missing(ncol) && !rowMaj )
				ncol <- length(data) / length(index)
		}
	} else {
		# data and index are both numeric
		if ( missing(nrow) && rowMaj )
			nrow <- length(pointers) - 1L
		if ( missing(nrow) && !rowMaj )
			nrow <- length(domain)
		if ( missing(ncol) && rowMaj )
			ncol <- length(domain)
		if ( missing(ncol) && !rowMaj )
			ncol <- length(pointers) - 1L
	}
	new(if (rowMaj) "sparse_matr" else "sparse_matc",
		data=data,
		datamode=as_Rtype(datamode),
		index=index,
		domain=domain,
		offset=as.integer(offset),
		pointers=pointers,
		paths=character(),
		chunksize=as.integer(chunksize),
		dim=as.integer(c(nrow, ncol)),
		dimnames=dimnames,
		tolerance=as_tol(tolerance),
		sampler=as_kern(sampler))
}

setMethod("describe_for_display", "sparse_mat", function(x) {
	desc1 <- paste0("<", x@dim[[1L]], " row, ", x@dim[[2L]], " column> ", class(x))
	desc2 <- paste0("sparse ", x@datamode[1L], " matrix")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "sparse_mat", function(x) {
	hdr <- preview_matrix_data(x)
	if ( is(x, "sparse_matc") ) {
		if ( is.null(rownames(x)) && !is.null(domain(x)) ) {
			n <- nrow(hdr)
			if ( rownames(hdr)[n] == "..." ) {
				rownames(hdr) <- c(paste0("(", domain(x)[1:(n - 1)], ",)"), "...")
			} else {
				rownames(hdr) <- paste0("(", domain(x)[1:n], ",)")
			}
		}
	}
	if ( is(x, "sparse_matr") ) {
		if ( is.null(colnames(x)) && !is.null(domain(x)) ) {
			n <- ncol(hdr)
			if ( colnames(hdr)[n] == "..." ) {
				colnames(hdr) <- c(paste0("(,", domain(x)[1:(n - 1)], ")"), "...")
			} else {
				colnames(hdr) <- paste0("(,", domain(x)[1:n], ")")
			}
		}
	}
	print(hdr, quote=FALSE, right=TRUE)
	cat("(", nnzero(x), "/", prod(dim(x)), " non-zero elements: ",
		round(nnzero(x) / prod(dim(x)), 4) * 100, "% density)\n", sep="")
})

setAs("matrix", "sparse_mat",
	function(from) sparse_mat(from, datamode=typeof(from), dimnames=dimnames(from)))

setAs("array", "sparse_mat",
	function(from) sparse_mat(as.matrix(from), datamode=typeof(from), dimnames=dimnames(from)))

as.sparse <- function(x, ...) as(x, "sparse_mat")

is.sparse <- function(x) is(x, "sparse_mat")

setAs("sparse_mat", "matrix", function(from) from[])

setMethod("as.matrix", "sparse_mat", function(x) as(x, "matrix"))

setReplaceMethod("domain", "sparse_matc", function(object, value) {
	object@domain <- value
	object@dim[1L] <- length(value)
	if ( validObject(object) )
		object
})

setReplaceMethod("domain", "sparse_matr", function(object, value) {
	object@domain <- value
	object@dim[2L] <- length(value)
	if ( validObject(object) )
		object
})

setMethod("length", "sparse_mat",
	function(x) prod(dim(x)))

setMethod("nnzero", "sparse_mat",
	function(x, na.counted = NA) {
		if ( is(x@data, "matter_OR_list") ) {
			# data and index are both lists
			sum(lengths(x@data))
		} else {
			# data is numeric with shared index
			length(x@data)
		}
	})

get_sparse_matc_elts <- function(x, i = NULL, j = NULL) {
	y <- .Call("C_getSparseMatrixC", x, i, j)
	if ( !is.null(dimnames(x)) ) {
		if ( !is.null(i) ) {
			rownames(y) <- rownames(x)[i]
		} else {
			rownames(y) <- rownames(x)
		}
		if ( !is.null(j) ) {
			rownames(y) <- rownames(x)[j]
		} else {
			rownames(y) <- rownames(x)
		}
	}
	y
}

get_sparse_matr_elts <- function(x, i = NULL, j = NULL) {
	y <- .Call("C_getSparseMatrixR", x, i, j)
	if ( !is.null(dimnames(x)) ) {
		if ( !is.null(i) ) {
			rownames(y) <- rownames(x)[i]
		} else {
			rownames(y) <- rownames(x)
		}
		if ( !is.null(j) ) {
			rownames(y) <- rownames(x)[j]
		} else {
			rownames(y) <- rownames(x)
		}
	}
	y
}

setMethod("[",
	c(x = "sparse_matc", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, j, ..., drop) {
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		if ( missing(i) )
			i <- NULL
		if ( missing(j) )
			j <- NULL
		y <- get_sparse_matc_elts(x, i, j)
		if ( isTRUE(drop) )
			y <- drop(y)
		y
	})

setMethod("[",
	c(x = "sparse_matr", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, j, ..., drop) {
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		if ( missing(i) )
			i <- NULL
		if ( missing(j) )
			j <- NULL
		y <- get_sparse_matr_elts(x, i, j)
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

setMethod("t", "sparse_matc", function(x)
{
	class(x) <- "sparse_matr"
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

setMethod("t", "sparse_matr", function(x)
{
	class(x) <- "sparse_matc"
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

setMethod("%*%", c("sparse_matc", "matrix"), function(x, y)
{
	rightMatrixMult(x, y, useOuter=TRUE)
})

setMethod("%*%", c("sparse_matr", "matrix"), function(x, y)
{
	rightMatrixMult(x, y, useOuter=FALSE)
})

setMethod("%*%", c("matrix", "sparse_matc"), function(x, y)
{
	leftMatrixMult(x, y, useOuter=FALSE)
})

setMethod("%*%", c("matrix", "sparse_matr"), function(x, y)
{
	leftMatrixMult(x, y, useOuter=TRUE)
})

