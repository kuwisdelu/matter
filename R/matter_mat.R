
#### Define matter<matrix> classes for matrix-like data ####
## --------------------------------------------------------

setClass("matter_mat",
	contains = c("matter", "VIRTUAL"),
	prototype = prototype(
		data = list(atoms()),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "matrix must have 'dim' of length 2")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter_matc",
	contains = "matter_mat",
	prototype = prototype(
		data = list(),
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
		data = list(),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL))

matter_mat <- function(data, datamode = "double", paths = NULL,
					filemode = ifelse(is.null(paths), "rb+", "rb"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = if (rowMaj) rep(ncol, nrow) else rep(nrow, ncol),
					nrow = 0, ncol = 0, rowMaj = FALSE, dimnames = NULL, ...)
{
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
	if ( noatoms ) {
		adata <- function() list()
	} else {
		adata <- function() mapply(atoms,
			factor(paths),
			make_datamode(datamode, type="C"),
			as.numeric(offset),
			as.numeric(extent))
	}
	if ( is.null(paths) && prod(c(nrow, ncol)) > 0 ) {
		if ( missing(data) )
			data <- NA
		filemode <- force(filemode)
		paths <- tempfile(fileext=".bin")
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	}
	paths <- normalizePath(paths)
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=max(length(extent), 1))
	x <- new(mclass,
		data=adata(),
		datamode=widest_datamode(datamode, from="C"),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=as.numeric(prod(c(nrow, ncol))),
		dim=as.integer(c(nrow, ncol)),
		names=NULL,
		dimnames=dimnames, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_mat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		"on-disk binary matrix", "\n", sep="")
	if ( !is.null(attr(object, "scaled:center")) )
		cat("    scaled:center = TRUE\n")
	if ( !is.null(attr(object, "scaled:scale")) )
		cat("    scaled:scale = TRUE\n")
	callNextMethod(object)
})

getMatrix <- function(x) {
	rowMaj <- switch(class(x), matter_matr=TRUE, matter_matc=FALSE)
	y <- .Call("C_getMatrix", x)
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- dimnames(x)
	y
}

setMatrix <- function(x, value) {
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	rowMaj <- switch(class(x), matter_matr=TRUE, matter_matc=FALSE)
	value <- rep(value, length.out=length(x)) # should do this in C++ code
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrix", x, value)
	if ( validObject(x) )
		invisible(x)
}

getMatrixRows <- function(x, i, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- .Call("C_getMatrixRows", x, i - 1)
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
	value <- rep(value, length.out=length(i) * ncol(x))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrixRows", x, i - 1, value)
	if ( validObject(x) )
		invisible(x)
}

getMatrixCols <- function(x, j, drop=TRUE) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- .Call("C_getMatrixCols", x, j - 1)
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
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	value <- rep(value, length.out=length(j) * nrow(x))
	.Call("C_setMatrixCols", x, j - 1, value)
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
	y <- .Call("C_getMatrixElements", x, i - 1, j - 1)
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
	value <- rep(value, length.out=length(i) * length(j))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setMatrixElements", x, i - 1, j - 1, value)
	if ( validObject(x) )
		invisible(x)
}

subsetMatterMatrix <- function(x, i, j) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( is(x, "matter_matc") ) {
		if ( !allIndices(x, i, 1) )
			stop("cannot subset column-major matrix as S4 by row")
		subsetMatterCols(x, j)
	} else if ( is(x, "matter_matr") ) {
		if ( !allIndices(x, j, 2) )
			stop("cannot subset row-major matrix as S4 by column")
		subsetMatterRows(x, i)
	}
}

subsetMatterCols <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	x <- switch(class(x),
		matter_matc=new("matter_matc",
			data=x@data[j],
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=x@dim[1] * length(j),
			dim=c(x@dim[1], length(j)),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]], x@dimnames[[2]][j]) else NULL),
		matter_matr=stop("cannot subset row-major matrix by columns"))
	if ( validObject(x) )
		invisible(x)
}

subsetMatterRows <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	x <- switch(class(x),
		matter_matc=stop("cannot subset column-major matrix by rows"),
		matter_matr=new("matter_matr",
			data=x@data[i],
			datamode=x@datamode,
			paths=x@paths,
			chunksize=x@chunksize,
			length=length(i) * x@dim[2],
			dim=c(length(i), x@dim[2]),
			names=NULL,
			dimnames=if (!is.null(x@dimnames))
				c(x@dimnames[[1]][i], x@dimnames[[2]]) else NULL))
	if ( validObject(x) )
		invisible(x)
}

# matrix getter methods

setMethod("[",
	c(x = "matter_mat", i = "missing", j = "missing", drop = "missing"),
	function(x, ...) getMatrix(x))

setMethod("[",
	c(x = "matter_mat", j = "missing", drop = "missing"),
	function(x, i, ...) getMatrixRows(x, i, drop))

setMethod("[",
	c(x = "matter_mat", i = "missing", drop = "missing"),
	function(x, j, ...) getMatrixCols(x, j, drop))

setMethod("[",
	c(x = "matter_mat", drop = "missing"),
	function(x, i, j, ...) getMatrixElements(x, i, j, drop))

setMethod("[",
	c(x = "matter_mat", j = "missing", drop = "logical"),
	function(x, i, ..., drop) {
		if ( is.na(drop) ) {
			subsetMatterRows(x, i)
		} else {
			getMatrixRows(x, i, drop)
		}
})

setMethod("[",
	c(x = "matter_mat", i = "missing", drop = "logical"),
	function(x, j, ..., drop) {
		if ( is.na(drop) ) {
			subsetMatterCols(x, j)
		} else {
			getMatrixCols(x, j, drop)
		}
})

setMethod("[",
	c(x = "matter_mat", drop = "logical"),
	function(x, i, j, ..., drop) {
		if ( is.na(drop) ) {
			subsetMatterMatrix(x, i, j)
		} else {
			getMatrixElements(x, i, j, drop)
		}
})

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
		y <- t(t(y))
	if ( nrow(x) != nrow(y) )
		stop("number of rows of column-major matrices must match")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data <- lapply(x@data, function(xs) {
		xs@source_id <- as.integer(factor(x@paths[xs@source_id],
			levels=paths))
		xs
	})
	y@data <- lapply(y@data, function(ys) {
		ys@source_id <- as.integer(factor(y@paths[ys@source_id],
			levels=paths))
		ys
	})
	data <- c(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest_datamode(data, from="C"),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine_colnames(x,y))
})

setMethod("cbind", "matter", function(..., deparse.level=1)
{
	dots <- list(...)
	for ( i in seq_along(dots) )
		dots[[i]] <- switch(class(dots[[i]]),
			matter_vec=t(t(dots[[i]])),
			matter_matc=dots[[i]],
			matter_matr=stop("cannot 'cbind' row-major matrices"))
	if ( length(dots) == 1 ) {
		dots[[1]]
	} else {
		do.call(combine, dots)
	}
})

setMethod("combine", "matter_matr", function(x, y, ...) {
	if ( is(y, "matter_vec") )
		y <- t(y)
	if ( ncol(x) != ncol(y) )
		stop("number of columns of row-major matrices must match")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data <- lapply(x@data, function(xs) {
		xs@source_id <- as.integer(factor(x@paths[xs@source_id],
			levels=paths))
		xs
	})
	y@data <- lapply(y@data, function(ys) {
		ys@source_id <- as.integer(factor(y@paths[ys@source_id],
			levels=paths))
		ys
	})
	data <- c(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest_datamode(data, from="C"),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine_rownames(x,y))
})

setMethod("rbind", "matter", function(..., deparse.level=1)
{
	dots <- list(...)
	for ( i in seq_along(dots) )
		dots[[i]] <- switch(class(dots[[i]]),
			matter_vec=t(dots[[i]]),
			matter_matc=stop("cannot 'rbind' column-major matrices"),
			matter_matr=dots[[i]])
	if ( length(dots) == 1 ) {
		dots[[1]]
	} else {
		do.call(combine, dots)
	}
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

setMethod("scale", "matter_mat", function(x, center=TRUE, scale=TRUE)
{
	if ( is.logical(center) ) {
		if ( center )
			center <- colMeans(x, na.rm=TRUE)
	} else if ( is.numeric(center) && length(center) != ncol(x) ) {
		stop("length of 'center' must equal the number of columns of 'x'")
	} else if ( !is.null(center) ) {
		stop("'center' must be logical, a numeric vector, or NULL")
	}
	if ( is.logical(scale) ) {
		if ( scale )
			scale <- colSds(x, na.rm=TRUE) # this differs from scale.default
	} else if ( is.numeric(scale) && length(scale) != ncol(x) ) {
		stop("length of 'center' must equal the number of columns of 'x'")
	} else if ( !is.null(scale) ) {
		stop("'scale' must be logical, a numeric vector, or NULL")
	}
	if ( is.numeric(center) || is.null(center) )
		attr(x, "scaled:center") <- center
	if ( is.numeric(scale) || is.null(scale) )
		attr(x, "scaled:scale") <- scale
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
	.Call("C_rightMatrixMult", x, y)
})

setMethod("%*%", c("matrix", "matter_mat"), function(x, y)
{
	.Call("C_leftMatrixMult", x, y)
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


