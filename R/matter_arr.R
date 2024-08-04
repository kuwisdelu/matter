
#### 'matter_arr' class for file-based arrays ####
## ------------------------------------------------

setClassUnion("ops_OR_NULL", c("deferred_ops", "NULL"))

setClass("matter_arr",
	slots = c(
		ops = "ops_OR_NULL",
		transpose = "logical"),
	contains = "matter_",
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

setClass("matter_mat",
	slots = c(indexed = "logical"),
	contains = "matter_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 2L )
			errors <- c(errors, "matrix must have exactly 2 dimensions")
		if ( length(object@indexed) != 1L )
			errors <- c(errors, "'indexed' must be a scalar (length 1)")
		if ( isTRUE(object@indexed) && length(object) != 0L ) {
			adims <- dim(object@data)
			if ( isTRUE(object@transpose) )
				adims <- rev(adims)
			if ( !isTRUE(all(object@dim == adims)) )
				errors <- c(errors, paste0("'indexed' is true but atoms groups [",
					paste0(adims, collapse=", "),
					"] are non-comforbable with array dimensions [",
					paste0(object@dim, collapse=", "), "]"))
		}
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter_vec",
	contains = "matter_arr",
	validity = function(object) {
		errors <- NULL
		if ( length(object@dim) != 1L )
			errors <- c(errors, "vector can't have more than 1 dimension")
		if ( is.null(errors) ) TRUE else errors
	})

matter_arr <- function(data, type = "double", path = NULL,
	dim = NA_integer_, dimnames = NULL, offset = 0, extent = NA_real_,
	readonly = NA, append = FALSE, rowMaj = FALSE, ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( missing(type) )
			type <- typeof(data)
		if ( anyNA(dim) && is.vector(data) ) {
			dim <- length(data)
		} else if ( anyNA(dim) ) {
			dim <- dim(data)
		}
		if ( is.null(dimnames) )
			dimnames <- dimnames(data)
	}
	if ( is.null(path) ) {
		readonly <- FALSE
		path <- tempfile(tmpdir=getOption("matter.temp.dir"), fileext=".bin")
		refs <- list(matter_shared_resource(create=path))
	} else {
		refs <- NULL
		if ( any(tolower(path) %in% ":memory:") ) {
			readonly <- FALSE
			for ( i in which(tolower(path) %in% ":memory:") ) {
				path[i] <- tempmem()
				refs <- c(refs, list(matter_shared_resource(create=path[i])))
			}
		}
	}
	path <- normalizePath(path, mustWork=FALSE)
	exists <- file.exists(path) | is_shared_memory_object(path)
	if ( append ) {
		readonly <- FALSE
		eof <- file.size(path)
		offset <- ifelse(exists, offset + eof, offset)
	}
	if ( is.na(readonly) )
		readonly <- all(exists) && !missing(data) && !is.null(data)
	if ( any(exists) && !readonly && !missing(data) && !is.null(data) ) {
		overwrite <- offset < file.size(path) & !is_shared_memory_pattern(path)
		if ( any(overwrite) )
			matter_warn("data may overwrite existing file(s): ",
				paste0(sQuote(path[overwrite]), collapse=", "))
	}
	if ( all(exists) && missing(data) ) {
		# TODO try to infer the dims from the type and file size
		# can we infer only the NA dims instead of overwriting all?
		if ( anyNA(dim) && anyNA(extent) ) {
			sizes <- file.size(path)
			dim <- sum((sizes - offset) %/% sizeof(type))
		}
	}
	if ( anyNA(dim) && anyNA(extent) )
		dim <- rep.int(0, length(dim))
	if ( anyNA(extent) )
		extent <- prod(dim)
	if ( anyNA(dim) )
		dim <- sum(extent)
	if ( length(offset) != length(extent) && length(path) == 1L ) {
		sizes <- sizeof(type) * extent
		offset <- cumsum(c(offset, sizes[-length(sizes)]))
	}
	files <- !is_shared_memory_pattern(path)
	newfiles <- !exists & files
	if ( any(newfiles) ) {
		if ( missing(data) && any(extent > 0) && !is.null(data) )
			matter_warn("creating uninitialized backing file(s): ",
				paste0(sQuote(path[newfiles]), collapse=", "))
		success <- file.create(path[newfiles])
		if ( !all(success) )
			matter_error("error creating file(s): ",
				paste0(sQuote(path[newfiles][!success]), collapse=", "))
	}
	path[files] <- normalizePath(path[files], mustWork=TRUE)
	x <- new("matter_arr",
		data=atoms(
			source=path,
			type=as_Ctype(type),
			offset=as.double(offset),
			extent=as.double(extent),
			group=0L,
			readonly=readonly,
			refs=refs),
		type=topmode_Rtype(type),
		dim=dim,
		dimnames=dimnames,
		ops=NULL,
		transpose=rowMaj, ...)
	if ( length(dim) == 1L )
		x <- as(x, "matter_vec")
	if ( length(dim) == 2L )
		x <- as(x, "matter_mat")
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

matter_vec <- function(data, type = "double", path = NULL,
	length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, append = FALSE, rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		if ( missing(type) )
			type <- typeof(data)
		if ( is.na(length) )
			length <- length(data)
		if ( is.null(names) )
			names <- names(data)
	}
	x <- matter_arr(data, type=type, path=path, dim=length,
		names=names, offset=offset, extent=extent,
		readonly=readonly, append=append, rowMaj=rowMaj, ...)
	as(x, "matter_vec")
}

matter_mat <- function(data, type = "double", path = NULL,
	nrow = NA_integer_, ncol = NA_integer_, dimnames = NULL,
	offset = 0, extent = NA_real_, readonly = NA,
	append = FALSE, rowMaj = FALSE, ...)
{
	if ( !missing(data) ) {
		if ( missing(type) )
			type <- typeof(data)
		if ( is.na(nrow) && is.na(ncol) ) {
			nrow <- nrow(data)
			ncol <- ncol(data)
		} else if ( is.na(nrow) ) {
			nrow <- length(data) / ncol
		} else if ( is.na(ncol) ) {
			ncol <- length(data) / nrow
		}
	} else {
		if ( is.na(nrow) && rowMaj )
			nrow <- max(length(offset), length(extent))
		if ( is.na(ncol) && rowMaj )
			ncol <- max(extent)
		if ( is.na(ncol) && !rowMaj )
			ncol <- max(length(offset), length(extent))
		if ( is.na(nrow) && !rowMaj )
			nrow <- max(extent)
	}
	x <- matter_arr(data=NULL, type=type, path=path, dim=c(nrow, ncol),
		dimnames=dimnames, offset=offset, extent=extent,
		readonly=readonly, append=append, rowMaj=rowMaj, ...)
	x <- as(x, "matter_mat")
	if ( x@transpose ) {
		n1 <- nrow(x)
		n2 <- ncol(x)
	} else {
		n1 <- ncol(x) # number of atoms/groups
		n2 <- nrow(x) # extent of each atom/group
	}
	if ( length(x@data) == n1 && all(unique(extent) == n2) ) {
		x@data <- regroup_atoms(x@data, 0L:(n1 - 1L))
	} else {
		x@data <- regroup_atoms(ungroup_atoms(x@data), n1)
	}
	x@indexed <- TRUE
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	if ( validObject(x) )
		x
}

setAs("matter_arr", "matter_vec",
	function(from) new("matter_vec", from, dim=length(from), dimnames=NULL))

setAs("matter_mat", "matter_vec",
	function(from) new("matter_vec", from, dim=length(from), dimnames=NULL))

setAs("matter_arr", "matter_mat",
	function(from) {
		x <- new("matter_mat", from, indexed=FALSE)
		if ( validObject(x) )
			x
	})

setAs("matter_vec", "matter_mat",
	function(from) {
		dm <- c(length(from), 1L)
		x <- new("matter_mat", from, dim=dm, names=NULL, indexed=FALSE)
		if ( validObject(x) )
			x
	})

setMethod("as.vector", "matter_arr",
	function(x, mode = "any") {
		if ( mode != "any" )
			type(x) <- mode
		names(x) <- NULL
		dimnames(x) <- NULL
		y <- as(x, "matter_vec")
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(y)
		} else {
			y[]
		}
	})

setMethod("as.raw", "matter_arr",
	function(x) as.vector(x, "raw"))

setMethod("as.logical", "matter_arr",
	function(x, ...) as.vector(x, "logical"))

setMethod("as.integer", "matter_arr",
	function(x, ...) as.vector(x, "integer"))

setMethod("as.double", "matter_arr",
	function(x, ...) as.vector(x, "double"))

setMethod("as.numeric", "matter_arr",
	function(x, ...) as.vector(x, "double"))

setMethod("as.matrix", "matter_arr",
	function(x, ...) {
		y <- as(x, "matter_mat")
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(y)
		} else {
			y[]
		}
	})

setMethod("as.array", "matter_arr",
	function(x, ...) {
		y <- as(x, "matter_arr")
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(y)
		} else {
			y[]
		}
	})

setMethod("describe_for_display", "matter_arr", function(x) {
	desc1 <- paste0("<", paste0(dim(x), collapse=" x "), " dim> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " array")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "matter_mat", function(x) {
	desc1 <- paste0("<", nrow(x), " row x ", ncol(x), " col> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " matrix")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "matter_vec", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory ", type(x), " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_arr", function(x) {
	rank <- length(dim(x))
	if ( rank <= 1L ) {
		preview_vector(x)
	} else if ( rank == 2L ) {
		preview_matrix(x)
	} else {
		preview_Nd_array(x)
	}
})

setMethod("vm_realized", "matter_arr", function(x) {
	size_bytes(sum(length(x) * sizeof(type(x)), na.rm=TRUE))
})

subset_matter_arr_elts <- function(x, i = NULL)
{
	if ( x@transpose ) {
		index <- array_ind(i, dim(x))
		i <- linear_ind(index, dim(x), rowMaj=TRUE)
	}
	data <- ungroup_atoms(x@data)
	data <- subset_atoms2(data, i, NULL)
	if ( !is.null(dim(x)) && !is.null(x@ops) ) {
		ops <- NULL
		matter_warn("deferred operations will be dropped")
	} else {
		ops <- subset_ops(x@ops, list(i))
	}
	y <- new(class(x), x, data=data, dim=length(i),
		names=x@names[i], dimnames=NULL, ops=ops)
	if ( validObject(y) )
		y
}

get_matter_arr_elts <- function(x, i = NULL) {
	y <- .Call(C_getMatterArray, x, i, PACKAGE="matter")
	y <- set_names(y, names(x), i)
	y
}

set_matter_arr_elts <- function(x, i = NULL, value = 0) {
	.Call(C_setMatterArray, x, i, value, PACKAGE="matter")
}

subset_matter_arr_subarray <- function(x, index)
{
	index <- as_array_subscripts(index, x)
	if ( x@transpose ) {
		i <- linear_ind(index, dim(x), rowMaj=TRUE)
	} else {
		i <- linear_ind(index, dim(x), rowMaj=FALSE)
	}
	data <- ungroup_atoms(x@data)
	data <- subset_atoms2(data, i, NULL)
	dm <- vapply(seq_along(index), function(j)
		if (is.null(index[[j]])) dim(x)[j]
		else length(index[[j]]), numeric(1))
	dnm <- subset_dimnames(x@dimnames, index)
	ops <- subset_ops(x@ops, index)
	y <- new(class(x), x, data=data, dim=dm,
		dimnames=dnm, ops=ops)
	if ( validObject(y) )
		y
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

subset_matter_mat_submatrix <- function(x, i = NULL, j = NULL)
{
	if ( !x@indexed )
		return(subset_matter_arr_subarray(x, list(i, j)))
	if ( x@transpose ) {
		data <- subset_atoms2(x@data, j, i)
	} else {
		data <- subset_atoms2(x@data, i, j)
	}
	nrow <- if (is.null(i)) x@dim[1L] else length(i)
	ncol <- if (is.null(j)) x@dim[2L] else length(j)
	dm <- c(nrow, ncol)
	dnm <- subset_dimnames(x@dimnames, list(i, j))
	ops <- subset_ops(x@ops, list(i, j))
	y <- new(class(x), x, data=data, dim=dm,
		dimnames=dnm, ops=ops)
	if ( validObject(y) )
		y
}

get_matter_mat_submatrix <- function(x, i = NULL, j = NULL, drop = FALSE)
{
	y <- .Call(C_getMatterMatrix, x, i, j, PACKAGE="matter")
	y <- set_dimnames(y, dimnames(x), list(i, j))
	if ( drop )
		y <- drop(y)
	y
}

set_matter_mat_submatrix <- function(x, i = NULL, j = NULL, value = 0)
{
	.Call(C_setMatterMatrix, x, i, j, value, PACKAGE="matter")
}

setMethod("[", c(x = "matter_arr"),
	function(x, i, j, ..., drop = TRUE) {
		narg <- nargs() - 1L - !missing(drop)
		if ( (narg == 1L && !missing(i)) || is.null(dim(x)) ) {
			i <- as_subscripts(i, x)
			if ( is_null_or_na(drop) ) {
				subset_matter_arr_elts(x, i)
			} else {
				get_matter_arr_elts(x, i)
			}
		} else {
			if ( narg != 1L && narg != length(dim(x)) )
				matter_error("incorrect number of dimensions")
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
			if ( !any(non_null(index)) && missing(drop) )
				drop <- FALSE
			if ( is_null_or_na(drop) ) {
				subset_matter_arr_subarray(x, index)
			} else {
				get_matter_arr_subarray(x, index, drop)
			}
		}
	})

setReplaceMethod("[", c(x = "matter_arr"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2L
		if ( (narg == 1L && !missing(i)) || is.null(dim(x)) ) {
			i <- as_subscripts(i, x)
			set_matter_arr_elts(x, i, value)
		} else {
			if ( narg != 1L && narg != length(dim(x)) )
				matter_error("incorrect number of dimensions")
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

setMethod("[", c(x = "matter_mat"),
	function(x, i, j, ..., drop = TRUE) {
		narg <- nargs() - 1L - !missing(drop)
		if ( (narg == 1L && !missing(i)) ) {
			i <- as_subscripts(i, x)
			get_matter_arr_elts(x, i)
		} else {
			if ( narg != 1L && narg != 2L )
				matter_error("incorrect number of dimensions")
			if ( missing(i) && missing(j) && missing(drop) )
				drop <- FALSE
			i <- as_row_subscripts(i, x)
			j <- as_col_subscripts(j, x)
			if ( isTRUE(x@indexed) ) {
				if ( is_null_or_na(drop) ) {
					subset_matter_mat_submatrix(x, i, j)
				} else {
					get_matter_mat_submatrix(x, i, j, drop)
				}
			} else {
				if ( is_null_or_na(drop) ) {
					subset_matter_arr_subarray(x, list(i, j))
				} else {
					get_matter_arr_subarray(x, list(i, j), drop)
				}
			}
		}
	})

setReplaceMethod("[", c(x = "matter_mat"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2L
		if ( (narg == 1L && !missing(i)) ) {
			i <- as_subscripts(i, x)
			set_matter_arr_elts(x, i, value)
		} else {
			if ( narg != 1L && narg != 2L )
				matter_error("incorrect number of dimensions")
			i <- as_row_subscripts(i, x)
			j <- as_col_subscripts(j, x)
			if ( isTRUE(x@indexed) ) {
				set_matter_mat_submatrix(x, i, j, value)
			} else {
				set_matter_arr_subarray(x, list(i, j), value)
			}
		}
	})

setMethod("combine", "matter_arr",
	function(x, y, ...) {
		if ( rowMaj(x) || rowMaj(y) )
			matter_error("can't combine row-major arrays")
		data <- rbind(
			ungroup_atoms(x@data),
			ungroup_atoms(y@data))
		if ( !is.null(x@ops) || !is.null(y@ops) )
			matter_warn("deferred operations will be dropped")
		new("matter_vec", data=data, type=x@type,
			dim=length(x) + length(y),
			names=combine_names(x, y),
			dimnames=NULL,
			transpose=FALSE, ops=NULL)
	})

setMethod("c", "matter_arr", combine_any)

setMethod("cbind2", c("matter_vec", "matter_vec"),
	function(x, y, ...)
		cbind2(as(x, "matter_mat"), as(y, "matter_mat")))

setMethod("cbind2", c("matter_mat", "matter_vec"),
	function(x, y, ...) cbind2(x, as(y, "matter_mat")))

setMethod("cbind2", c("matter_vec", "matter_mat"),
	function(x, y, ...) cbind2(as(x, "matter_mat"), y))

setMethod("cbind2", c("matter_mat", "matter_mat"),
	function(x, y, ...) {
		if ( nrow(x) != nrow(y) )
			matter_error("number of rows of matrices must match")
		if ( rowMaj(x) || rowMaj(y) )
			matter_error("can't cbind row-major matrices")
		if ( !is.null(x@ops) || !is.null(y@ops) )
			matter_warn("deferred operations will be dropped")
		data <- cbind(x@data, y@data)
		new(class(x), x, data=data,
			dim=c(nrow(x), ncol(x) + ncol(y)),
			dimnames=cbind_dimnames(x, y),
			ops=NULL)
	})

setMethod("rbind2", c("matter_vec", "matter_vec"),
	function(x, y, ...) rbind2(t(x), t(y)))

setMethod("rbind2", c("matter_mat", "matter_vec"),
	function(x, y, ...) rbind2(x, t(y)))

setMethod("rbind2", c("matter_vec", "matter_mat"),
	function(x, y, ...) rbind2(t(x), y))

setMethod("rbind2", c("matter_mat", "matter_mat"),
	function(x, y, ...) {
		if ( ncol(x) != ncol(y) )
			matter_error("number of columns of matrices must match")
		if ( !x@transpose || !y@transpose )
			matter_error("can't rbind column-major matrices")
		if ( !is.null(x@ops) || !is.null(y@ops) )
			matter_warn("deferred operations will be dropped")
		data <- cbind(x@data, y@data)
		new(class(x), x, data=data,
			dim=c(nrow(x) + nrow(y), ncol(x)),
			dimnames=rbind_dimnames(x, y),
			ops=NULL)
	})

cbind.matter_arr <- cbind_any

rbind.matter_arr <- rbind_any

setMethod("dim", "matter_vec", function(x) NULL)

setReplaceMethod("dim", "matter_arr", function(x, value) {
	if ( is.null(value) ) {
		x@transpose <- FALSE
		x@dim <- prod(x@dim)
		x@dimnames <- NULL
		as(x, "matter_vec")
	} else {
		y <- callNextMethod()
		if ( length(value) == 2L )
			y <- as(y, "matter_mat")
		y
	}
})

setReplaceMethod("dim", "matter_vec", function(x, value) {
	if ( !is.null(value) )
		x <- as(x, "matter_arr")
	callNextMethod(x, value)
})

setMethod("Arith", c(e1 = "matter_arr", e2 = "vector"),
	function(e1, e2) register_op(e1, .Generic, e2, FALSE))

setMethod("Arith", c(e1 = "matter_arr", e2 = "array"),
	function(e1, e2) register_op(e1, .Generic, e2, FALSE))

setMethod("Arith", c(e1 = "vector", e2 = "matter_arr"),
	function(e1, e2) register_op(e2, .Generic, e1, TRUE))

setMethod("Arith", c(e1 = "array", e2 = "matter_arr"),
	function(e1, e2) register_op(e2, .Generic, e1, TRUE))

setMethod("exp", "matter_arr", function(x) register_op(x, "exp"))
setMethod("log", "matter_arr", function(x) register_op(x, "log"))
setMethod("log2", "matter_arr", function(x) register_op(x, "log2"))
setMethod("log10", "matter_arr", function(x) register_op(x, "log10"))
setMethod("log1p", "matter_arr", function(x) register_op(x, "log1p"))

setMethod("rowMaj", "matter_arr", function(x)
{
	isTRUE(x@transpose)
})

setMethod("t", "matter_arr", function(x)
{
	if ( length(dim(x)) > 2L )
		matter_error("argument is not a matrix")
	x@transpose <- !x@transpose
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	x@ops <- t_ops(x@ops)
	if ( validObject(x) )
		x
})

setMethod("t", "matter_vec", function(x)
{
	t(as(x, "matter_mat"))
})

setMethod("%*%", c("matter_mat", "vector"), function(x, y)
{
	if ( rowMaj(x) ) {
		rmatmul(x, as.matrix(y), useOuter=FALSE)
	} else {
		rmatmul(x, as.matrix(y), useOuter=TRUE)
	}
})

setMethod("%*%", c("vector", "matter_mat"), function(x, y)
{
	if ( rowMaj(y) ) {
		lmatmul(t(x), y, useOuter=TRUE)
	} else {
		lmatmul(t(x), y, useOuter=FALSE)
	}
})

setMethod("%*%", c("matter_mat", "matrix"), function(x, y)
{
	if ( rowMaj(x) ) {
		rmatmul(x, y, useOuter=FALSE)
	} else {
		rmatmul(x, y, useOuter=TRUE)
	}
})

setMethod("%*%", c("matrix", "matter_mat"), function(x, y)
{
	if ( rowMaj(y) ) {
		lmatmul(x, y, useOuter=TRUE)
	} else {
		lmatmul(x, y, useOuter=FALSE)
	}
})

setMethod("crossprod", c("matter_mat", "ANY"),
	function(x, y = NULL, ...) t(x) %*% y)

setMethod("crossprod", c("ANY", "matter_mat"),
	function(x, y = NULL, ...) t(x) %*% y)

setMethod("tcrossprod", c("matter_mat", "ANY"),
	function(x, y = NULL, ...) x %*% t(y))

setMethod("tcrossprod", c("ANY", "matter_mat"),
	function(x, y = NULL, ...) x %*% t(y))

rmatmul <- function(x, y, useOuter = FALSE)
{
	BPPARAM <- getOption("matter.matmul.bpparam")
	if ( is.null(BPPARAM) ) {
		rmatmul_sc(x, y, useOuter)
	} else {
		rmatmul_mc(x, y, useOuter, BPPARAM)
	}
}

lmatmul <- function(x, y, useOuter = FALSE)
{
	BPPARAM <- getOption("matter.matmul.bpparam")
	if ( is.null(BPPARAM) ) {
		lmatmul_sc(x, y, useOuter)
	} else {
		lmatmul_mc(x, y, useOuter, BPPARAM)
	}
}

# serial right matrix mult
rmatmul_sc <- function(x, y, useOuter = FALSE)
{
	nchunks <- getOption("matter.default.nchunks")
	verbose <- getOption("matter.default.verbose")
	ans <- matrix(0, nrow=nrow(x), ncol=ncol(y))
	if ( useOuter ) {
		INDEX <- chunkify(seq_len(ncol(x)), nchunks)
		for ( i in INDEX ) {
			xi <- as.matrix(x[,i,drop=FALSE])
			attr(xi, "chunkinfo") <- attributes(i)
			matter_log_chunk(xi, verbose=verbose)
			ans <- ans + xi %*% y[i,,drop=FALSE]
		}
	} else {
		INDEX <- chunkify(seq_len(nrow(x)), nchunks)
		for ( i in INDEX ) {
			xi <- as.matrix(x[i,,drop=FALSE])
			attr(xi, "chunkinfo") <- attributes(i)
			matter_log_chunk(xi, verbose=verbose)
			ans[i,] <- xi %*% y
		}
	}
	ans
}

# parallel right matrix mult
rmatmul_mc <- function(x, y, useOuter = FALSE, BPPARAM = NULL)
{
	env <- list2env(list(y=y, parent=baseenv()))
	if ( useOuter ) {
		add <- function(...) Reduce("+", list(...))
		matmul <- local(function(xi) {
			i <- attr(xi, "index")
			xi %*% y[i,,drop=FALSE]
		}, envir=env)
		ans <- chunk_colapply(x, matmul,
			simplify=add, BPPARAM=BPPARAM)
	} else {
		matmul <- local(function(xi) xi %*% y, envir=env)
		ans <- chunk_rowapply(x, matmul,
			simplify=rbind, BPPARAM=BPPARAM)
	}
	ans
}

# serial left matrix mult
lmatmul_sc <- function(x, y, useOuter = FALSE)
{
	nchunks <- getOption("matter.default.nchunks")
	verbose <- getOption("matter.default.verbose")
	ans <- matrix(0, nrow=nrow(x), ncol=ncol(y))
	if ( useOuter ) {
		INDEX <- chunkify(seq_len(nrow(y)), nchunks)
		for ( i in INDEX ) {
			yi <- as.matrix(y[i,,drop=FALSE])
			attr(yi, "chunkinfo") <- attributes(i)
			matter_log_chunk(yi, verbose=verbose)
			ans <- ans + x[,i,drop=FALSE] %*% yi
		}
	} else {
		INDEX <- chunkify(seq_len(ncol(y)), nchunks)
		for ( i in INDEX ) {
			yi <- as.matrix(y[,i,drop=FALSE])
			attr(yi, "chunkinfo") <- attributes(i)
			matter_log_chunk(yi, verbose=verbose)
			ans[,i] <- x %*% yi
		}
	}
	ans
}

# parallel left matrix mult
lmatmul_mc <- function(x, y, useOuter = FALSE, BPPARAM = NULL)
{
	env <- list2env(list(x=x, parent=baseenv()))
	if ( useOuter ) {
		add <- function(...) Reduce("+", list(...))
		matmul <- local(function(yi) {
			i <- attr(yi, "index")
			x[,i,drop=FALSE] %*% yi
		}, envir=env)
		ans <- chunk_rowapply(y, matmul,
			simplify=add, BPPARAM=BPPARAM)
	} else {
		matmul <- local(function(yi) x %*% yi, envir=env)
		ans <- chunk_colapply(y, matmul,
			simplify=cbind, BPPARAM=BPPARAM)
	}
	ans
}
