

#### Define matter<sparse matrix> classes for sparse data ####
## -----------------------------------------------------------

setClassUnion("valid_key_type",
	c("integer", "numeric", "character", "NULL"))

setClass("sparse_mat",
	slots = c(
		keys = "valid_key_type",
		tolerance = "numeric",
		combiner = "function"),
	prototype = prototype(
		datamode = make_datamode(c("virtual", "numeric"), type="R"),
		dim = c(0L,0L),
		dimnames = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	contains = c("matter_vt", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "sparse matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "sparse matrix must have 'dim' of length 2")
		if ( !all(c("keys", "values") %in% names(object@data)) )
			errors <- c(errors, "'data' must include elements named 'keys' and 'values'")
		if ( !all(lengths(object@data$keys) == lengths(object@data$values)) )
			errors <- c(errors, "lengths of 'data$keys' must match lengths of 'data$values'")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matc",
	prototype = prototype(
		data = list(),
		datamode = make_datamode(c("virtual", "numeric"), type="R"),
		paths = character(),
		filemode = "",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	contains = "sparse_mat",
	validity = function(object) {
		errors <- NULL
		if ( object@dim[2] != length(object@data$values) )
			errors <- c(errors, "length of 'data$values'  must match number of columns")
		if ( !is.null(object@keys) && object@dim[1] != length(object@keys) )
			errors <- c(errors, "length of 'keys' must match number of rows")
		if ( !object@datamode[2] %in% c("logical", "integer", "numeric") )
			errors <- c(errors, "'datamode[2]' must be 'logical', 'integer', or 'numeric'")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matr",
	prototype = prototype(
		data = list(),
		datamode = make_datamode(c("virtual", "numeric"), type="R"),
		paths = character(),
		filemode = "",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	contains = "sparse_mat",
	validity = function(object) {
		errors <- NULL
		if ( object@dim[1] != length(object@data$values) )
			errors <- c(errors, "length of 'data$values'  must match number of rows")
		if ( !is.null(object@keys) && object@dim[2] != length(object@keys) )
			errors <- c(errors, "length of 'keys' must match number of columns")
		if ( is.null(errors) ) TRUE else errors
	})

sparse_mat <- function(data, datamode = "double", nrow = 0, ncol = 0,
					rowMaj = FALSE, dimnames = NULL, keys = NULL,
					tolerance = c(abs=0), combiner = "identity", ...) {
	if ( !missing(data) ) {
		if ( is.matrix(data) ) {
			if ( missing(datamode) )
				datamode <- typeof(data)
			if ( missing(nrow) )
				nrow <- nrow(data)
			if ( missing(ncol) )
				ncol <- ncol(data)
		} else if ( is.list(data) ) {
			if ( !("keys" %in% names(data)) )
				stop("data must have an element named 'keys'")
			if ( !("values" %in% names(data)) )
				stop("data must have an element named 'values'")
			if ( rowMaj ) {
				if ( missing(ncol) && missing(keys) )
					stop("'ncol' cannot be missing")
				if ( missing(nrow) )
					nrow <- length(data$keys)
			} else {
				if ( missing(nrow) && missing(keys) )
					stop("'nrow' cannot be missing")
				if ( missing(ncol) )
					ncol <- length(data$keys)
			}
		}
	}
	datamode <- as.character(make_datamode(datamode, type="R"))
	if ( rowMaj ) {
		mclass <- "sparse_matr"
		if ( missing(ncol) && !missing(keys) )
			ncol <- length(keys)
	} else {
		mclass <- "sparse_matc"
		if ( missing(nrow) && !missing(keys) )
			nrow <- length(keys)
	}
	keymode <- if ( is.null(keys) ) "integer" else typeof(keys)
	if ( missing(data) || !is.list(data) ) {
		adata <- function() {
			n <- if ( rowMaj ) nrow else ncol
			list(keys=rep(list(vector(keymode, 0)), n),
				values=rep(list(vector("numeric", 0)), n))
		}
	} else {
		adata <- function() data
	}
	x <- new(mclass,
		data=adata(),
		datamode=make_datamode(c("virtual", datamode), type="R"),
		paths=character(),
		filemode="",
		length=as.numeric(sum(lengths(adata()$values))),
		dim=as.integer(c(nrow, ncol)),
		names=NULL,
		dimnames=dimnames,
		ops=NULL,
		keys=keys,
		tolerance=as_sparse_mat_tolerance(tolerance),
		combiner=as_sparse_mat_combiner(combiner))
	if ( !missing(data) && !is.list(data) )
		x[] <- data
	x
}

as_sparse_mat_combiner <- function(combiner) {
	if ( is.character(combiner) ) {
		if ( combiner == "identity" ) {
			fun <- groupIds
			attr(fun, "name") <- "identity"
		} else if ( combiner == "mean" ) {
			fun <- groupMeans
			attr(fun, "name") <- "mean"
		} else if ( combiner == "sum" ) {
			fun <- groupSums
			attr(fun, "name") <- "sum"
		} else if ( combiner == "min" ) {
			fun <- groupMins
			attr(fun, "name") <- "min"
		} else if ( combiner == "max" ) {
			fun <- groupMaxs
			attr(fun, "name") <- "max"
		} else {
			fun <- groupCombiner(match.fun(combiner))
			attr(fun, "name") <- combiner
		}
	} else if ( is.function(combiner) ) {
		fun <- combiner
		attr(fun, "name") <- "<user>"
	} else {
		stop("invalid 'combiner'")
	}
	fun
}

as_sparse_mat_tolerance <- function(tolerance) {
	tol <- tolerance[1]
	if ( !is.null(names(tol)) ) {
		type <- pmatch(names(tol), c("absolute", "relative"), nomatch=1L)
	} else {
		type <- 1L
	}
	tol <- as.vector(tol)
	attr(tol, "type") <- factor(type,
		levels=c(1, 2), labels=c("absolute", "relative"))
	tol
}

setMethod("describe_for_display", "sparse_mat", function(x) "sparse matrix")

setMethod("show", "sparse_mat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		describe_for_display(object), "\n", sep="")
	memnames <- names(object@data)[!sapply(object@data, is.matter)]
	disknames <- names(object@data)[sapply(object@data, is.matter)]
	object.memory <- bytes(object.size(object))
	cat("    datamode:", paste0(object@datamode[2]), "\n")
	cat("    ", format(object.memory, units="auto"), " real memory: ",
		paste_head(memnames, collapse=", "), "\n", sep="")
	cat("    ", format(disk_used(object), units="auto"), " virtual memory: ",
		paste_head(disknames, collapse=", "), "\n", sep="")
	cat("    ", length(object), " non-zero elements\n", sep="")
	cat("    ", round(length(object) / prod(dim(object)), 4) * 100,
		"% density\n", sep="")
	if ( !is.null(attr(object, "scaled:center")) )
		cat("    scaled:center = TRUE\n")
	if ( !is.null(attr(object, "scaled:scale")) )
		cat("    scaled:scale = TRUE\n")
})

setReplaceMethod("datamode", "sparse_mat", function(x, value) {
	value <- as.character(make_datamode(value, type="R"))
	if ( value[1] != "virtual" )
		x@datamode <- make_datamode(c("virtual", value), type="R")
	x
})

setAs("matrix", "sparse_mat",
	function(from) sparse_mat(from, datamode=typeof(from), dimnames=dimnames(from)))

setAs("array", "sparse_mat",
	function(from) sparse_mat(as.matrix(from), datamode=typeof(from), dimnames=dimnames(from)))

as.sparse <- function(x, ...) as(x, "sparse_mat")

is.sparse <- function(x) is(x, "sparse_mat")

setMethod("keys", "sparse_mat", function(object) object@keys)

setReplaceMethod("keys", "sparse_mat", function(object, value) {
	object@keys <- value
	if ( validObject(object) )
		object
})

setMethod("tolerance", "sparse_mat", function(object) object@tolerance)

setReplaceMethod("tolerance", "sparse_mat", function(object, value) {
	object@tolerance <- as_sparse_mat_tolerance(value)
	object
})

setMethod("combiner", "sparse_mat", function(object) object@combiner)

setReplaceMethod("combiner", "sparse_mat", function(object, value) {
	object@combiner <- as_sparse_mat_combiner(value)
	object
})

getSparseMatrixElements <- function(x, i, j, drop=TRUE) {
	if ( is.null(i) ) {
		i <- 1:dim(x)[1]
		all.i <- TRUE
	} else {
		if ( is.logical(i) )
			i <- logical2index(x, i, 1)
		if ( is.character(i) )
			i <- dimnames2index(x, i, 1)
		all.i <- FALSE
		if ( any(i <= 0 | i > dim(x)[1]) )
			stop("subscript out of bounds")
	}
	if ( is.null(j) ) {
		j <- 1:dim(x)[2]
		all.j <- TRUE
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, 2)
		if ( is.character(j) )
			j <- dimnames2index(x, j, 2)
		all.j <- FALSE
		if ( any(j <= 0 | j > dim(x)[2]) )
			stop("subscript out of bounds")
	}
	vmode <- as.character(x@datamode[2])
	zero <- as.vector(0, mode=vmode)
	init <- as.vector(NA, mode=vmode)
	y <- matrix(init, nrow=length(i), ncol=length(j))
	rowMaj <- switch(class(x), sparse_matr=TRUE, sparse_matc=FALSE)
	sorted <- FALSE
	if ( is.null(keys(x)) ) {
		keymode <- typeof(atomdata(x)$keys[[1]])
		if ( rowMaj ) {
			if ( is.sorted(j) )
				sorted <- TRUE
			if ( all.j ) {
				keys <- NULL
			} else if ( sorted ) {
				keys <- as.vector(j, mode=keymode)
			} else {
				ord <- order(j)
				keys <- as.vector(j[ord], mode=keymode)
			}
		} else {
			if ( is.sorted(i) )
				sorted <- TRUE
			if ( all.i ) {
				keys <- NULL
			} else if ( sorted ) {
				keys <- as.vector(i, mode=keymode)
			} else {
				ord <- order(i)
				keys <- as.vector(i[ord], mode=keymode)
			}
		}
	} else {
		if ( rowMaj ) {
			if ( is.sorted(keys(x)[j]) ) {
				sorted <- TRUE
				keys <- keys(x)[j]
			} else {
				ord <- order(keys(x)[j])
				keys <- keys(x)[j][ord]
			}
		} else {
			if ( is.sorted(keys(x)[i]) ) {
				sorted <- TRUE
				keys <- keys(x)[i]
			} else {
				ord <- order(keys(x)[i])
				keys <- keys(x)[i][ord]
			}
		}
	}
	.keys <- atomdata(x)$keys
	.vals <- atomdata(x)$values
	dup.ok <- is.double(keys) && x@tolerance > 0
	tol.type <- as.integer(attr(x@tolerance, "type"))
	if ( rowMaj ) {
		for ( ii in seq_along(i) ) {
			if ( is.na(i[ii]) )
				next
			.jkeys <- .keys[[i[ii]]]
			.jvals <- .vals[[i[ii]]]
			if ( is.null(keys) ) {
				if ( sorted ) {
					y[ii,] <- zero
					y[ii,.jkeys] <- as.vector(.jvals, mode=vmode)
				} else {
					y[ii,] <- zero
					y[ii,ord] <- as.vector(.jvals, mode=vmode)
				}
			} else if ( length(keys) > length(.jkeys) || dup.ok ) {
				if ( tol.type == 1 ) { # absolute
					index <- bsearch_int(key=.jkeys, values=keys,
						tol=x@tolerance, tol.ref=1L) # 1 = 'none'
				} else if ( tol.type == 2 ) { # relative
					index <- bsearch_int(key=.jkeys, values=keys,
						tol=x@tolerance, tol.ref=3L) # 3 = 'values'
				}
				if ( sorted ) {
					y[ii,] <- as.vector(x@combiner(.jvals, index,
						length(keys), default=zero), mode=vmode)
				} else {
					y[ii,ord] <- as.vector(x@combiner(.jvals, index,
						length(keys), default=zero), mode=vmode)
				}
			} else {
				index <- bsearch_int(key=keys, values=.jkeys)
				zwh <- is.na(index) & !is.na(keys)
				if ( sorted ) {
					y[ii,] <- as.vector(.jvals[index], mode=vmode)
					y[ii,zwh] <- zero
				} else {
					y[ii,ord] <- as.vector(.jvals[index], mode=vmode)
					y[ii,ord[zwh]] <- zero
				}
			}
		}
	} else {
		for ( jj in seq_along(j) ) {
			if ( is.na(j[jj]) )
				next
			.ikeys <- .keys[[j[jj]]]
			.ivals <- .vals[[j[jj]]]
			if ( is.null(keys) ) {
				if ( sorted ) {
					y[,jj] <- zero
					y[.ikeys,jj] <- as.vector(.ivals, mode=vmode)
				} else {
					y[,jj] <- zero
					y[ord,jj] <- as.vector(.ivals[], mode=vmode)
				}
			} else if ( length(keys) > length(.ikeys) || dup.ok ) {
				if ( tol.type == 1 ) { # absolute
					index <- bsearch_int(key=.ikeys, values=keys,
						tol=x@tolerance, tol.ref=1L) # 1 = 'none'
				} else if ( tol.type == 2 ) { # relative
					index <- bsearch_int(key=.ikeys, values=keys,
						tol=x@tolerance, tol.ref=3L) # 3 = 'values'
				}
				if ( sorted ) {
					y[,jj] <- as.vector(x@combiner(.ivals, index,
						length(keys), default=zero), mode=vmode)
				} else {
					y[ord,jj] <- as.vector(x@combiner(.ivals, index,
						length(keys), default=zero), mode=vmode)
				}
			} else {
				index <- bsearch_int(key=keys, values=.ikeys)
				zwh <- is.na(index) & !is.na(keys)
				if ( sorted ) {
					y[,jj] <- as.vector(.ivals[index], mode=vmode)
					y[zwh,jj] <- zero
				} else {
					y[ord,jj] <- as.vector(.ivals[index], mode=vmode)
					y[ord[zwh],jj] <- zero
				}
			}
		}
	}
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x)[i], colnames(x)[j])
	if ( drop ) 
		y <- drop(y)
	y
}

setSparseMatrixElements <- function(x, i, j, value) {
	if ( is.null(i) ) {
		i <- 1:dim(x)[1]
		all.i <- TRUE
	} else {
		if ( is.logical(i) )
			i <- logical2index(x, i, 1)
		if ( is.character(i) )
			i <- dimnames2index(x, i, 1)
		all.i <- FALSE
		if ( any(i <= 0 | i > dim(x)[1]) )
			stop("subscript out of bounds")
	}
	if ( is.null(j) ) {
		j <- 1:dim(x)[2]
		all.j <- TRUE
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, 2)
		if ( is.character(j) )
			j <- dimnames2index(x, j, 2)
		all.j <- FALSE
		if ( any(j <= 0 | j > dim(x)[2]) )
			stop("subscript out of bounds")
	}
	if ( (length(i) * length(j)) %% length(value) != 0 )
		stop("number of items to replace is not ",
			"a multiple of replacement length")
	value <- rep(value, length.out=length(i) * length(j))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	rowMaj <- switch(class(x), sparse_matr=TRUE, sparse_matc=FALSE)
	dim(value) <- c(length(i), length(j))
	vmode <- as.character(x@datamode[2])
	zero <- as.vector(0, mode=vmode)
	if ( is.null(keys(x)) ) {
		keymode <- typeof(atomdata(x)$keys[[1]])
		if ( rowMaj ) {
			if ( all.i ) {
				keys <- NULL
			} else {
				keys <- as.vector(j, mode=keymode)
			}
		} else {

			if ( all.j ) {
				keys <- NULL
			} else {
				keys <- as.vector(i, mode=keymode)
			}
		}
	} else {
		if ( rowMaj ) {
			keys <- keys(x)[j]
		} else {
			keys <- keys(x)[i]
		}
	}
	.keys <- atomdata(x)$keys
	.vals <- atomdata(x)$values
	dup.ok <- is.double(keys) && x@tolerance > 0
	if ( dup.ok )
		warning("assigning with tolerance > 0, results may be unexpected")
	if ( rowMaj ) {
		for ( ii in seq_along(i) ) {
			if ( is.na(i[ii]) )
				next
			.jkeys <- .keys[[i[ii]]]
			.jvals <- .vals[[i[ii]]]
			if ( is.null(keys) ) {
				newkeys <- 1:dim(x)[2]
			} else {
				newkeys <- keys
			}
			zwh <- value[ii,] == zero
			nz <- !zwh
			na <- is.na(newkeys)
			remove <- .jkeys %in% newkeys[zwh]
			newkeys <- newkeys[nz & !na]
			newvals <- value[ii,nz & !na]
			keep <- !.jkeys %in% newkeys
			newkeys <- c(.jkeys[keep & !remove], newkeys)
			newvals <- c(.jvals[keep & !remove], newvals)
			o <- order(newkeys)
			.keys[[i[ii]]] <- newkeys[o]
			.vals[[i[ii]]] <- newvals[o]
		}
	} else {
		for ( jj in seq_along(j) ) {
			if ( is.na(j[jj]) )
				next
			.ikeys <- .keys[[j[jj]]]
			.ivals <- .vals[[j[jj]]]
			if ( is.null(keys) ) {
				newkeys <- 1:dim(x)[1]
			} else {
				newkeys <- keys
			}
			zwh <- value[,jj] == zero
			nz <- !zwh
			na <- is.na(newkeys)
			remove <- .ikeys %in% newkeys[zwh]
			newkeys <- newkeys[nz & !na]
			newvals <- value[nz & !na,jj]
			keep <- !.ikeys %in% newkeys
			newkeys <- c(.ikeys[keep & !remove], newkeys)
			newvals <- c(.ivals[keep & !remove], newvals)
			o <- order(newkeys)
			.keys[[j[jj]]] <- newkeys[o]
			.vals[[j[jj]]] <- newvals[o]
		}
	}
	x@length <- as.numeric(sum(lengths(.vals)))
	atomdata(x)$keys <- .keys
	atomdata(x)$values <- .vals
	if ( validObject(x) )
		invisible(x)
}

getSparseMatrixRows <- function(x, i, drop=TRUE) {
	getSparseMatrixElements(x, i, NULL, drop=drop)
}

setSparseMatrixRows <- function(x, i, value) {
	setSparseMatrixElements(x, i, NULL, value)
}

getSparseMatrixCols <- function(x, j, drop=TRUE) {
	getSparseMatrixElements(x, NULL, j, drop=drop)
}

setSparseMatrixCols <- function(x, j, value) {
	getSparseMatrixElements(x, NULL, j, value)
}

getSparseMatrix <- function(x) {
	getSparseMatrixElements(x, NULL, NULL, drop=FALSE)
}

setSparseMatrix <- function(x, value) {
	setSparseMatrixElements(x, NULL, NULL, value)
}

subSparseMatrix <- function(x, i, j) {
	if ( is(x, "sparse_matc") ) {
		subSparseMatrixRows(subSparseMatrixCols(x, j), i)
	} else if ( is(x, "sparse_matr") ) {
		subSparseMatrixCols(subSparseMatrixRows(x, i), j)
	}
}

subSparseMatrixCols <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	x <- switch(class(x),
		sparse_matc=new("sparse_matc",
			data=list(keys=x@data$keys[j], values=x@data$values[j]),
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(sum(lengths(x@data$values[j]))),
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL,
			ops=NULL,
			keys=x@keys,
			tolerance=x@tolerance,
			combiner=x@combiner),
		sparse_matr=new("sparse_matr",
			data=x@data,
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=x@length,
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL,
			ops=NULL,
			keys=if ( !is.null(x@keys) )
				x@keys[j] else as.vector(j, mode=typeof(x@data$keys[[1]])),
			tolerance=x@tolerance,
			combiner=x@combiner))
	if ( validObject(x) )
		invisible(x)
}

subSparseMatrixRows <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	x <- switch(class(x),
		sparse_matc=new("sparse_matc",
			data=x@data,
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=x@length,
			dim=c(length(i), x@dim[2]),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]][i], x@dimnames[[2]]) else NULL,
			ops=NULL,
			keys=if ( !is.null(x@keys) )
				x@keys[i] else as.vector(i, mode=typeof(x@data$keys[[1]])),
			tolerance=x@tolerance,
			combiner=x@combiner),
		sparse_matr=new("sparse_matr",
			data=list(keys=x@data$keys[i], values=x@data$values[i]),
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(sum(lengths(x@data$values[i]))),
			dim=c(length(i), x@dim[2]),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]][i], x@dimnames[[2]]) else NULL,
			ops=NULL,
			keys=x@keys,
			tolerance=x@tolerance,
			combiner=x@combiner))
	if ( validObject(x) )
		invisible(x)
}

# sparse matrix getter methods

setMethod("[",
	c(x = "sparse_mat", i = "missing", j = "missing"),
	function(x, ..., drop) getSparseMatrix(x))

setMethod("[",
	c(x = "sparse_mat", j = "missing"),
	function(x, i, ..., drop) getSparseMatrixRows(x, i, drop))

setMethod("[",
	c(x = "sparse_mat", i = "missing"),
	function(x, j, ..., drop) getSparseMatrixCols(x, j, drop))

setMethod("[",
	c(x = "sparse_mat"),
	function(x, i, j, ..., drop) getSparseMatrixElements(x, i, j, drop))

setMethod("[",
	c(x = "sparse_mat", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subSparseMatrixRows(x, i))

setMethod("[",
	c(x = "sparse_mat", i = "missing", drop = "NULL"),
	function(x, j, ..., drop) subSparseMatrixCols(x, j))

setMethod("[",
	c(x = "sparse_mat", drop = "NULL"),
	function(x, i, j, ..., drop) subSparseMatrix(x, i, j))

# sparse matrix setter methods

setReplaceMethod("[",
	c(x = "sparse_mat", i = "missing", j = "missing"),
	function(x, ..., value) setSparseMatrix(x, value))

setReplaceMethod("[",
	c(x = "sparse_mat", j = "missing"),
	function(x, i, ..., value) setSparseMatrixRows(x, i, value))

setReplaceMethod("[",
	c(x = "sparse_mat", i = "missing"),
	function(x, j, ..., value) setSparseMatrixCols(x, j, value))

setReplaceMethod("[",
	c(x = "sparse_mat"),
	function(x, i, j, ..., value) setSparseMatrixElements(x, i, j, value))


# combine by rows

setMethod("combine_by_rows", c("sparse_matr", "sparse_matr"), function(x, y, ...) {
	if ( ncol(x) != ncol(y) )
		stop("number of columns of sparse matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	if ( !all(x@keys == y@keys) )
		warning("'keys' do not match, results may be unexpected")
	keys <- c(x@data$keys, y@data$keys)
	values <- c(x@data$values, y@data$values)
	new(class(x),
		data=list(keys=keys, values=values),
		datamode=x@datamode,
		paths=character(),
		filemode="rb",
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine_rownames(x,y),
		ops=NULL,
		keys=x@keys,
		tolerance=x@tolerance,
		combiner=x@combiner)
})

# combine by cols

setMethod("combine_by_cols", c("sparse_matc", "sparse_matc"), function(x, y, ...) {
	if ( nrow(x) != nrow(y) )
		stop("number of rows of sparse matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	if ( !all(x@keys == y@keys) )
		warning("'keys' do not match, results may be unexpected")
	keys <- c(x@data$keys, y@data$keys)
	values <- c(x@data$values, y@data$values)
	new(class(x),
		data=list(keys=keys, values=values),
		datamode=x@datamode,
		paths=character(),
		filemode="rb",
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine_colnames(x,y),
		ops=NULL,
		keys=x@keys,
		tolerance=x@tolerance,
		combiner=x@combiner)
})

# transpose

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

