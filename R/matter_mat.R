
#### Define matter<matrix> classes for matrix-like data ####
## --------------------------------------------------------

setClass("matter_mat",
	slot = c(data = "atoms"),
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL,
		ops = NULL),
	contains = c("matter", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "matrix must have 'dim' of length 2")
		if ( prod(object@dim) != object@length )
			errors <- c(errors, paste0("dims [product ", prod(object@dim),
				"] do not match the length of array [", object@length, "]"))
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter_matc",
	contains = "matter_mat",
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL))

setClass("matter_matr",
	contains = "matter_mat",
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL))

matter_mat <- function(data, datamode = "double", paths = NULL,
					filemode = ifelse(all(file.exists(paths)), "rb", "rb+"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = if (rowMaj) rep(ncol, nrow) else rep(nrow, ncol),
					nrow = 0, ncol = 0, rowMaj = FALSE, dimnames = NULL, ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(nrow) || missing(ncol) ) {
			if ( !is.matrix(data) )
				stop("data is not a matrix")
			if ( missing(nrow) )
				nrow <- nrow(data)
			if ( missing(ncol) )
				ncol <- ncol(data)
		}
	}
	if ( nrow == 0 && ncol == 0 && all(extent == 0) ) {
		if ( rowMaj ) {
			return(new("matter_matr"))
		} else {
			return(new("matter_matc"))
		}
	}
	if ( length(unique(extent)) > 1 )
		stop("all elements of 'extent' must be equal")
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=max(length(extent), 1))
	noatoms <- FALSE
	if ( rowMaj ) {
		mclass <- "matter_matr"
		nrow <- length(extent)
		if ( nrow > 0 ) {
			ncol <- unique(extent)
		} else {
			noatoms <- TRUE
		}
	} else {
		mclass <- "matter_matc"
		ncol <- length(extent)
		if ( ncol > 0 ) {
			nrow <- unique(extent)
		} else {
			noatoms <- TRUE
		}
	}
	if ( is.null(paths) )
		paths <- tempfile(fileext=".bin")
	paths <- normalizePath(paths, mustWork=FALSE)
	if ( !file.exists(paths) ) {
		if ( missing(data) )
			data <- vector(as.character(widest_datamode(datamode)), length=1)
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	} else if ( !missing(data) ) {
		warning("file already exists")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=max(length(extent), 1))
	if ( noatoms ) {
		adata <- function() atoms()
	} else {
		adata <- function() atoms(
			group_id=seq_along(extent),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(make_datamode(datamode, type="C")),
			offset=as.numeric(offset),
			extent=as.numeric(extent))
	}
	x <- new(mclass,
		data=adata(),
		datamode=widest_datamode(datamode),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=as.numeric(prod(c(nrow, ncol))),
		dim=as.integer(c(nrow, ncol)),
		names=NULL,
		dimnames=dimnames,
		ops=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_mat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		"on-disk matrix", "\n", sep="")
	callNextMethod(object)
	if ( !is.null(attr(object, "scaled:center")) )
		cat("    scaled:center = TRUE\n")
	if ( !is.null(attr(object, "scaled:scale")) )
		cat("    scaled:scale = TRUE\n")
})

setAs("raw", "matter_mat", function(from) matter_mat(as.matrix(from)))

setAs("logical", "matter_mat", function(from) matter_mat(as.matrix(from)))

setAs("integer", "matter_mat", function(from) matter_mat(as.matrix(from)))

setAs("numeric", "matter_mat", function(from) matter_mat(as.matrix(from)))

setAs("matrix", "matter_mat",
	function(from) matter_mat(from, datamode=typeof(from), dimnames=dimnames(from)))

as.matter_mat <- function(x) as(x, "matter_mat")

setAs("matter_mat", "matrix", function(from) from[])

setMethod("as.matrix", "matter_mat", function(x) as(x, "matrix"))

getMatrix <- function(x) {
	y <- .Call("C_getMatrix", x, PACKAGE="matter")
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- dimnames(x)
	y
}

setMatrix <- function(x, value) {
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(x))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrix", x, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getMatrixRows <- function(x, i, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- .Call("C_getMatrixRows", x, i - 1, PACKAGE="matter")
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x)[i], colnames(x))
	if ( drop ) 
		y <- drop(y)
	y
}

setMatrixRows <- function(x, i, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( (length(i) * ncol(x)) %% length(value) != 0 )
		stop("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(i) * ncol(x))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrixRows", x, i - 1, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getMatrixCols <- function(x, j, drop=TRUE) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- .Call("C_getMatrixCols", x, j - 1, PACKAGE="matter")
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x), colnames(x)[j])
	if ( drop ) 
		y <- drop(y)
	y
}

setMatrixCols <- function(x, j, value) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 1)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 1)
	if ( (length(j) * nrow(x)) %% length(value) != 0 )
		stop("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(j) * nrow(x))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrixCols", x, j - 1, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getMatrixElements <- function(x, i, j, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- .Call("C_getMatrixElements", x, i - 1, j - 1, PACKAGE="matter")
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x)[i], colnames(x)[j])
	if ( drop ) 
		y <- drop(y)
	y
}

setMatrixElements <- function(x, i, j, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( (length(i) * length(j)) %% length(value) != 0 )
		stop("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(i) * length(j))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrixElements", x, i - 1, j - 1, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

subMatrix <- function(x, i, j) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( is(x, "matter_matc") ) {
		subMatrixRows(subMatrixCols(x, j), i)
	} else if ( is(x, "matter_matr") ) {
		subMatrixCols(subMatrixRows(x, i), j)
	}
}

subMatrixCols <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	x <- switch(class(x),
		matter_matc=new("matter_matc",
			data=x@data[,j],
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(x@dim[1]) * as.numeric(length(j)),
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL,
			ops=NULL),
		matter_matr=new("matter_matr",
			data=subset_atoms_by_index_offset(x@data, j),
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(x@dim[1]) * as.numeric(length(j)),
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL,
			ops=NULL))
	if ( validObject(x) )
		invisible(x)
}

subMatrixRows <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	x <- switch(class(x),
		matter_matc=new("matter_matc",
			data=subset_atoms_by_index_offset(x@data, i),
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(length(i)) * as.numeric(x@dim[2]),
			dim=c(length(i), x@dim[2]),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]][i], x@dimnames[[2]]) else NULL,
			ops=NULL),
		matter_matr=new("matter_matr",
			data=x@data[,i],
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=as.numeric(length(i)) * as.numeric(x@dim[2]),
			dim=c(length(i), x@dim[2]),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]][i], x@dimnames[[2]]) else NULL,
			ops=NULL))
	if ( validObject(x) )
		invisible(x)
}

# matrix getter methods

setMethod("[",
	c(x = "matter_mat", i = "missing", j = "missing"),
	function(x, ..., drop) getMatrix(x))

setMethod("[",
	c(x = "matter_mat", j = "missing"),
	function(x, i, ..., drop) getMatrixRows(x, i, drop))

setMethod("[",
	c(x = "matter_mat", i = "missing"),
	function(x, j, ..., drop) getMatrixCols(x, j, drop))

setMethod("[",
	c(x = "matter_mat"),
	function(x, i, j, ..., drop) getMatrixElements(x, i, j, drop))

setMethod("[",
	c(x = "matter_mat", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subMatrixRows(x, i))

setMethod("[",
	c(x = "matter_mat", i = "missing", drop = "NULL"),
	function(x, j, ..., drop) subMatrixCols(x, j))

setMethod("[",
	c(x = "matter_mat", drop = "NULL"),
	function(x, i, j, ..., drop) subMatrix(x, i, j))

# matrix setter methods

setReplaceMethod("[",
	c(x = "matter_mat", i = "missing", j = "missing"),
	function(x, ..., value) setMatrix(x, value))

setReplaceMethod("[",
	c(x = "matter_mat", j = "missing"),
	function(x, i, ..., value) setMatrixRows(x, i, value))

setReplaceMethod("[",
	c(x = "matter_mat", i = "missing"),
	function(x, j, ..., value) setMatrixCols(x, j, value))

setReplaceMethod("[",
	c(x = "matter_mat"),
	function(x, i, j, ..., value) setMatrixElements(x, i, j, value))

# matrix manipulation

setMethod("combine", "matter_matc", function(x, y, ...) {
	if ( is(y, "matter_vec") )
		y <- as(y, "matter_mat")
	if ( nrow(x) != nrow(y) )
		stop("number of rows of column-major matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data@source_id <- as.integer(factor(x@paths[x@data@source_id[]],
		levels=paths))
	y@data@source_id <- as.integer(factor(y@paths[y@data@source_id[]],
		levels=paths))
	y@data@group_id <- y@data@group_id[] + max(x@data@group_id[])
	data <- combine(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest_datamode(datamode(data)),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine_colnames(x,y),
		ops=NULL)
})

setMethod("combine", "matter_matr", function(x, y, ...) {
	if ( is(y, "matter_vec") )
		y <- t(y)
	if ( ncol(x) != ncol(y) )
		stop("number of columns of row-major matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data@source_id <- as.integer(factor(x@paths[x@data@source_id[]],
		levels=paths))
	y@data@source_id <- as.integer(factor(y@paths[y@data@source_id[]],
		levels=paths))
	y@data@group_id <- y@data@group_id[] + max(x@data@group_id[])
	data <- combine(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest_datamode(datamode(data)),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine_rownames(x,y),
		ops=NULL)
})

setMethod("t", "matter_matc", function(x)
{
	class(x) <- "matter_matr"
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

setMethod("t", "matter_matr", function(x)
{
	class(x) <- "matter_matc"
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	if ( validObject(x) )
		x
})

#### Matrix multiplication for matter objects ####
## -----------------------------------------------

# matrix x vector

setMethod("%*%", c("matter_matc", "numeric"), function(x, y) { x %*% as.matrix(y) })

setMethod("%*%", c("matter_matr", "numeric"), function(x, y) { x %*% as.matrix(y) })

# vector x matrix

setMethod("%*%", c("numeric", "matter_matc"), function(x, y) { t(x) %*% y })

setMethod("%*%", c("numeric", "matter_matr"), function(x, y) { t(x) %*% y })

# matrix x matrix

setMethod("%*%", c("matter_mat", "matrix"), function(x, y)
{
	.Call("C_rightMatrixMult", x, y, PACKAGE="matter")
})

setMethod("%*%", c("matrix", "matter_mat"), function(x, y)
{
	.Call("C_leftMatrixMult", x, y, PACKAGE="matter")
})

setMethod("%*%", c("matter", "matter"), function(x, y)
{
	stop("at least one matrix must be in memory")
})

# aliases for crossprod and tcrossprod

setMethod("crossprod", c("matter", "ANY"), function(x, y) t(x) %*% y)

setMethod("crossprod", c("ANY", "matter"), function(x, y) t(x) %*% y)

setMethod("tcrossprod", c("matter", "ANY"), function(x, y) x %*% t(y))

setMethod("tcrossprod", c("ANY", "matter"), function(x, y) x %*% t(y))


#### Delayed operations on 'matter_mat' ####
## ----------------------------------------

check_comformable_dims <- function(x, y, margin = 1) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(y) != dim(x)[margin] ) {
		warning("argument length is non-conformable with matrix dimensions and will be recycled")
	}
	TRUE
}

# Arith

setMethod("Arith", c("matter_matc", "matter_matc"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( all(dim(e1) == dim(e2)) ) {
			register_op(e1, NULL, e2, .Generic)
		} else {
			stop("on-disk matrix dimensions must match exactly for delayed operation")
		}
})

setMethod("Arith", c("matter_matr", "matter_matr"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( all(dim(e1) == dim(e2)) ) {
			register_op(e1, NULL, e2, .Generic)
		} else {
			stop("on-disk matrix dimensions must match exactly for delayed operation")
		}
})

setMethod("Arith", c("matter_matc", "numeric"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_dims(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_group")
			if ( datamode(e1)[1] != "numeric" && typeof(e2) == "double" )
				datamode(e1) <- "numeric"
			e1
		}
})

setMethod("Arith", c("matter_matr", "numeric"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_dims(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_each_group")
			if ( datamode(e1)[1] != "numeric" && typeof(e2) == "double" )
				datamode(e1) <- "numeric"
			e1
		}
})

setMethod("Arith", c("numeric", "matter_matc"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_dims(e1, e2) )  {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_group")
			if ( datamode(e2)[1] != "numeric" && typeof(e1) == "double" )
				datamode(e2) <- "numeric"
			e2
		}
})

setMethod("Arith", c("numeric", "matter_matr"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_dims(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_each_group")
			if ( datamode(e2)[1] != "numeric" && typeof(e1) == "double" )
				datamode(e2) <- "numeric"
			e2
		}
})

## Compare

setMethod("Compare", c("matter_matc", "matter_matc"),
	function(e1, e2) {
		if ( all(dim(e1) == dim(e2)) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		} else {
			stop("on-disk matrix dimensions must match exactly for delayed operation")
		}
})

setMethod("Compare", c("matter_matr", "matter_matr"),
	function(e1, e2) {
		if ( all(dim(e1) == dim(e2)) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		} else {
			stop("on-disk matrix dimensions must match exactly for delayed operation")
		}
})

setMethod("Compare", c("matter_matc", "raw"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_group")
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("matter_matr", "raw"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_each_group")
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("raw", "matter_matc"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_group")
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

setMethod("Compare", c("raw", "matter_matr"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_each_group")
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

setMethod("Compare", c("matter_matc", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_group")
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("matter_matr", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic, "by_each_group")
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("numeric", "matter_matc"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_group")
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

setMethod("Compare", c("numeric", "matter_matr"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic, "by_each_group")
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

## Math

setMethod("exp", "matter_mat",
	function(x) {
		x <- register_op(x, NULL, NULL, "^", "by_each_group")
		if ( datamode(x) != "numeric" )
			datamode(x) <- "numeric"
		x
})

setMethod("log", "matter_matc",
	function(x, base) {
		if ( missing(base) ) {
			x <- register_op(x, NULL, NULL, "log", "by_group")
		} else if ( check_comformable_dims(x, base) ) {
			x <- register_op(x, base, NULL, "log", "by_group")
		}
		if ( datamode(x) != "numeric" )
			datamode(x) <- "numeric"
		x
})

setMethod("log", "matter_matr",
	function(x, base) {
		if ( missing(base) ) {
			x <- register_op(x, NULL, NULL, "log", "by_each_group")
		} else if ( check_comformable_dims(x, base) ) {
			x <- register_op(x, base, NULL, "log", "by_each_group")
		}
		if ( datamode(x) != "numeric" )
			datamode(x) <- "numeric"
		x
})

setMethod("log2", "matter_mat", function(x) log(x, base=2))

setMethod("log10", "matter_mat", function(x) log(x, base=10))

