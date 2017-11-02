

#### Define matter<sparse matrix> classes for sparse data ####
## -----------------------------------------------------------

setClassUnion("valid_key_type",
	c("integer", "numeric", "character", "NULL"))

setClass("sparse_mat",
	slots = c(
		atoms = "list",
		keys = "valid_key_type",
		tolerance = "numeric",
		combiner = "function"),
	prototype = prototype(
		data = list(),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "sparse matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "sparse matrix must have 'dim' of length 2")
		if ( !all(lengths(object@data$keys) == lengths(object@data$values)) )
			errors <- c(errors, "lengths of 'data$keys' must match lengths of 'data$values'")
		if ( !object@datamode %in% c("integer", "numeric") )
			errors <- c(errors, "'datamode' must be 'integer' or 'numeric'")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matc",
	contains = "sparse_mat",
	prototype = prototype(
		data = list(),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	validity = function(object) {
		errors <- NULL
		if ( object@dim[2] != length(object@data$values) )
			errors <- c(errors, "length of 'data$values'  must match number of columns")
		if ( !is.null(object@keys) && object@dim[1] != length(object@keys) )
			errors <- c(errors, "length of 'keys' must match number of rows")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("sparse_matr",
	contains = "sparse_mat",
	prototype = prototype(
		data = list(),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		keys = NULL,
		tolerance = 0,
		combiner = groupIds),
	validity = function(object) {
		errors <- NULL
		if ( object@dim[1] != length(object@data$values) )
			errors <- c(errors, "length of 'data$values'  must match number of rows")
		if ( !is.null(object@keys) && object@dim[2] != length(object@keys) )
			errors <- c(errors, "length of 'keys' must match number of columns")
		if ( is.null(errors) ) TRUE else errors
	})

sparse_mat <- function(data, datamode = "double", keys = NULL,
					nrow = 0, ncol = 0, rowMaj = FALSE, dimnames = NULL,
					tolerance = c(absolute=0), combiner = "identity", ...) {
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
	vmode <- as.character(make_datamode(datamode, type="R"))
	if ( missing(data) || !is.list(data) ) {
		adata <- function() {
			n <- if ( rowMaj ) nrow else ncol
			list(keys=rep(list(vector(keymode, 0)), n),
				values=rep(list(vector(vmode, 0)), n))
		}
	} else {
		adata <- function() data
	}
	x <- new(mclass,
		data=adata(),
		datamode=make_datamode(datamode, type="R"),
		paths=character(),
		filemode="rb",
		length=sum(lengths(adata()$values)),
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

setMethod("show", "sparse_mat", function(object) {
	keys.memory <- bytes(object.size(adata(object)$keys))
	values.memory <- bytes(object.size(adata(object)$values))
	if ( is.matter(atomdata(object)$keys) ) {
		keys.disk <- disk_used(adata(adata(object)$keys))
		keys.summary <- paste0(format(keys.memory), " in-memory (",
			format(keys.disk), " on-disk)")
	} else {
		keys.disk <- bytes(0)
		keys.summary <- paste0(format(keys.memory), " in-memory")
	}
	if ( is.matter(atomdata(object)$values) ) {
		values.disk <- disk_used(adata(adata(object)$values))
		values.summary <- paste0(format(values.memory), " in-memory (",
			format(values.disk), " on-disk)")
	} else {
		values.disk <- bytes(0)
		values.summary <- paste0(format(values.memory), " in-memory")
	}
	object.memory <- bytes(object.size(object))
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		"sparse matrix", "\n", sep="")
	cat("    keys:", keys.summary, "\n")
	cat("    values:", values.summary, "\n")
	cat("    ", length(object), " non-zero elements\n", sep="")
	cat("    ", round(length(object) / prod(dim(object)), 2) * 100,
		"% density\n", sep="")
	if ( !is.null(attr(object, "scaled:center")) )
		cat("    scaled:center = TRUE\n")
	if ( !is.null(attr(object, "scaled:scale")) )
		cat("    scaled:scale = TRUE\n")
})

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

getSparseMatrixElements <- function(x, i, j, drop) {
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
	init <- as(NA, as.character(datamode(x)))
	y <- matrix(init, nrow=length(i), ncol=length(j))
	rowMaj <- switch(class(x), sparse_matr=TRUE, sparse_matc=FALSE)
	sorted <- FALSE
	if ( is.null(keys(x)) ) {
		if ( rowMaj ) {
			if ( is.sorted(j) )
				sorted <- TRUE
			if ( all.j ) {
				keys <- NULL
			} else if ( sorted ) {
				keys <- as.integer(j)
			} else {
				ord <- order(j)
				keys <- as.integer(j)[ord]
			}
		} else {
			if ( is.sorted(i) )
				sorted <- TRUE
			if ( all.i ) {
				keys <- NULL
			} else if ( sorted ) {
				keys <- as.integer(i)
			} else {
				ord <- order(i)
				keys <- as.integer(i)[ord]
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
			.ikeys <- .keys[[i[ii]]]
			.ivals <- .vals[[i[ii]]]
			if ( is.null(keys) ) {
				if ( sorted ) {
					y[ii,] <- 0
					y[ii,] <- .ivals
				} else {
					y[ii,] <- 0
					y[ii,ord] <- .ivals
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
					y[ii,] <- x@combiner(.ivals, index, length(keys), default=0)
				} else {
					y[ii,ord] <- x@combiner(.ivals, index, length(keys), default=0)
				}
			} else {
				index <- bsearch_int(key=keys, values=.ikeys)
				zero <- is.na(index) & !is.na(keys)
				if ( sorted ) {
					y[ii,] <- .ivals[index]
					y[ii,zero] <- 0
				} else {
					y[ii,ord] <- .jvals[index]
					y[ii,ord[zero]] <- 0
				}
			}
		}
	} else {
		for ( jj in seq_along(j) ) {
			if ( is.na(j[jj]) )
				next
			.jkeys <- .keys[[j[jj]]]
			.jvals <- .vals[[j[jj]]]
			if ( is.null(keys) ) {
				if ( sorted ) {
					y[,jj] <- 0
					y[.jkeys,jj] <- .jvals
				} else {
					y[,jj] <- 0
					y[ord,jj] <- .jvals[]
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
					y[,jj] <- x@combiner(.jvals, index, length(keys), default=0)
				} else {
					y[ord,jj] <- x@combiner(.jvals, index, length(keys), default=0)
				}
			} else {
				index <- bsearch_int(key=keys, values=.jkeys)
				zero <- is.na(index) & !is.na(keys)
				if ( sorted ) {
					y[,jj] <- .jvals[index]
					y[zero,jj] <- 0
				} else {
					y[ord,jj] <- .jvals[index]
					y[ord[zero],jj] <- 0
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
	if ( is.null(keys(x)) ) {
		if ( rowMaj ) {
			if ( all.i ) {
				keys <- NULL
			} else {
				keys <- as.integer(j)
			}
		} else {
			if ( all.j ) {
				keys <- NULL
			} else {
				keys <- as.integer(i)
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
			.ikeys <- .keys[[i[ii]]]
			.ivals <- .vals[[i[ii]]]
			if ( is.null(keys) ) {
				newkeys <- 1:dim(x)[2]
			} else {
				newkeys <- keys
			}
			zero <- value[ii,] == 0
			nz <- !zero
			na <- is.na(newkeys)
			remove <- .ikeys %in% newkeys[zero]
			newkeys <- newkeys[nz & !na]
			newvals <- value[ii,nz & !na]
			keep <- !.ikeys %in% newkeys
			na <- is.na(.ikeys)
			newkeys <- c(.ikeys[keep & !remove], newkeys)
			newvals <- c(.ivals[keep & !remove], newvals)
			o <- order(newkeys)
			.keys[[i[ii]]] <- newkeys[o]
			.vals[[i[ii]]] <- newvals[o]
		}
	} else {
		for ( jj in seq_along(j) ) {
			if ( is.na(j[jj]) )
				next
			.jkeys <- .keys[[j[jj]]]
			.jvals <- .vals[[j[jj]]]
			if ( is.null(keys) ) {
				newkeys <- 1:dim(x)[1]
			} else {
				newkeys <- keys
			}
			zero <- value[,jj] == 0
			nz <- !zero
			na <- is.na(newkeys)
			remove <- .jkeys %in% newkeys[zero]
			newkeys <- newkeys[nz & !na]
			newvals <- value[nz & !na,jj]
			keep <- !.jkeys %in% newkeys
			newkeys <- c(.jkeys[keep & !remove], newkeys)
			newvals <- c(.jvals[keep & !remove], newvals)
			o <- order(newkeys)
			.keys[[j[jj]]] <- newkeys[o]
			.vals[[j[jj]]] <- newvals[o]
		}
	}
	x@length <- sum(lengths(.vals))
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

subsetSparseMatterMatrix <- function(x, i, j) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( is(x, "sparse_matc") ) {
		if ( !allIndices(x, i, 1) )
			stop("cannot subset compressed-column sparse matrix as S4 by row")
		subsetSparseMatterCols(x, j)
	} else if ( is(x, "sparse_matr") ) {
		if ( !allIndices(x, j, 2) )
			stop("cannot subset compressed-row sparse matrix as S4 by column")
		subsetSparseMatterRows(x, i)
	}
}

subsetSparseMatterCols <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	data <- list(
		keys=x@data$keys[j],
		values=x@data$values[j])
	x <- switch(class(x),
		sparse_matc=new("sparse_matc",
			data=data,
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=sum(lengths(data$values)),
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL,
			ops=NULL,
			keys=x@keys,
			tolerance=x@tolerance,
			combiner=x@combiner),
		sparse_matr=stop("cannot subset compressed-row sparse matrix by columns"))
	if ( validObject(x) )
		invisible(x)
}

subsetSparseMatterRows <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	data <- list(
		keys=x@data$keys[i],
		values=x@data$values[i])
	x <- switch(class(x),
		sparse_matc=stop("cannot subset compressed-column sparse matrix by rows"),
		sparse_matr=new("sparse_matr",
			data=data,
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=sum(lengths(data$values)),
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
	function(x, i, ..., drop) subsetSparseMatterRows(x, i))

setMethod("[",
	c(x = "sparse_mat", i = "missing", drop = "NULL"),
	function(x, j, ..., drop) subsetSparseMatterCols(x, j))

setMethod("[",
	c(x = "sparse_mat", drop = "NULL"),
	function(x, i, j, ..., drop) subsetSparseMatterMatrix(x, i, j))

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


# matrix manipulation

setMethod("combine", c("sparse_matc", "sparse_matc"), function(x, y, ...) {
	if ( nrow(x) != nrow(y) )
		stop("number of rows of compressed-column sparse matrices must match")
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

setMethod("combine", c("sparse_matr", "sparse_matr"), function(x, y, ...) {
	if ( ncol(x) != ncol(y) )
		stop("number of columns of compressed-row sparse matrices must match")
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

