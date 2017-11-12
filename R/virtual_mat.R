

#### Define matter<virtual matrix> classes for virtual data ####
## -------------------------------------------------------------

setClass("virtual_mat",
	slots = c(
		index = "list_OR_NULL",
		transpose = "logical"),
	prototype = prototype(
		datamode = make_datamode(c("virtual", "numeric"), type="R"),
		dim = c(0L,0L),
		dimnames = NULL,
		index = NULL,
		transpose = FALSE),
	contains = c("matter_vt", "VIRTUAL"),
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@dim) )
			errors <- c(errors, "virtual matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			errors <- c(errors, "virtual matrix must have 'dim' of length 2")
		if ( any(lengths(lapply(object@data, dim)) != 2) )
			errors <- c(errors, "elements of 'data' must have 'dim' of length 2")
		if ( prod(object@dim) != object@length )
			errors <- c(errors, paste0("dims [product ", prod(object@dim),
				"] do not match the length of array [", object@length, "]"))
		if ( !object@datamode[2] %in% c("logical", "integer", "numeric") )
			errors <- c(errors, "'datamode[2]' must be 'logical', 'integer', or 'numeric'")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("virtual_matc",
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
		index = NULL,
		transpose = FALSE),
	contains = "virtual_mat",
	validity = function(object) {
		errors <- NULL
		if ( length(unique(sapply(object@data, nrow))) != 1 )
			errors <- c(errors, "elements of 'data' must have the same number of rows")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("virtual_matr",
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
		index = NULL,
		transpose = FALSE),
	contains = "virtual_mat",
	validity = function(object) {
		errors <- NULL
		if ( length(unique(sapply(object@data, ncol))) != 1 )
			errors <- c(errors, "elements of 'data' must have the same number of columns")
		if ( is.null(errors) ) TRUE else errors
	})

virtual_mat <- function(data, datamode = "double", rowMaj = FALSE,
						dimnames = NULL, index = NULL, ...) {
	if ( !is.list(data) )
		data <- list(data)
	datamode <- as.character(make_datamode(datamode, type="R"))
	if ( rowMaj ) {
		mclass <- "virtual_matr"
		nrow <- sum(sapply(data, nrow))
		ncol <- ncol(data[[1]])
	} else {
		mclass <- "virtual_matc"
		nrow <- nrow(data[[1]])
		ncol <- sum(sapply(data, ncol))
	}
	x <- new(mclass,
		data=data,
		datamode=make_datamode(c("virtual", datamode), type="R"),
		paths=character(),
		filemode="",
		length=as.numeric(prod(c(nrow, ncol))),
		dim=as.integer(c(nrow, ncol)),
		names=NULL,
		dimnames=dimnames,
		ops=NULL,
		index=NULL)
	x
}

setMethod("describe_for_display", "virtual_mat", function(x) "virtual matrix")

setMethod("show", "virtual_mat", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		describe_for_display(object), "\n", sep="")
	cat("    datamode:", paste0(object@datamode[2]), "\n")
	callNextMethod(object)
	if ( !is.null(attr(object, "scaled:center")) )
		cat("    scaled:center = TRUE\n")
	if ( !is.null(attr(object, "scaled:scale")) )
		cat("    scaled:scale = TRUE\n")
})

setReplaceMethod("datamode", "virtual_mat", function(x, value) {
	value <- as.character(make_datamode(value, type="R"))
	if ( value[1] != "virtual" )
		x@datamode <- make_datamode(c("virtual", value), type="R")
	x
})

setAs("ANY", "virtual_matc",
	function(from) virtual_mat(from, dimnames=dimnames(from), rowMaj=FALSE))

setAs("ANY", "virtual_matr",
	function(from) virtual_mat(from, dimnames=dimnames(from), rowMaj=TRUE))

setAs("matrix", "virtual_mat",
	function(from) virtual_mat(from, datamode=typeof(from), dimnames=dimnames(from)))

setAs("array", "virtual_mat",
	function(from) virtual_mat(as.matrix(from), datamode=typeof(from), dimnames=dimnames(from)))

as.virtual <- function(x, ...) as(x, "virtual_mat")

is.virtual <- function(x) is(x, "virtual_mat")

getVirtualMatrixElements <- function(x, i, j, drop=TRUE) {
	if ( is.null(i) ) {
		if ( is.null(x@index[[1]]) ) {
			rows <- seq_len(dim(x)[1])
		} else {
			rows <- x@index[[1]]
		}
	} else {
		if ( is.logical(i) )
			i <- logical2index(x, i, 1)
		if ( is.character(i) )
			i <- dimnames2index(x, i, 1)
		if ( is.null(x@index[[1]]) ) {
			rows <- i
		} else {
			rows <- x@index[[1]][i]
		}
	}
	if ( is.null(j) ) {
		if ( is.null(x@index[[2]]) ) {
			cols <- seq_len(dim(x)[2])
		} else {
			cols <- x@index[[2]]
		}
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, 2)
		if ( is.character(j) )
			j <- dimnames2index(x, j, 2)
		if ( is.null(x@index[[2]]) ) {
			cols <- j
		} else {
			cols <- x@index[[2]][j]
		}
	}
	nrow <- length(rows)
	ncol <- length(cols)
	if ( x@transpose ) {
		t.rows <- rows
		t.cols <- cols
		rows <- t.cols
		cols <- t.rows
	}
	vmode <- as.character(x@datamode[2])
	init <- as.vector(NA, mode=vmode)
	y <- matrix(init, nrow=nrow, ncol=ncol)
	if ( is(x, "virtual_matc") ) {
		colranges <- c(0, cumsum(sapply(x@data, ncol)))
		wh <- findInterval(cols, colranges, left.open=TRUE)
		for ( jj in seq_along(cols) ) {
			e <- wh[jj]
			vals <- x@data[[e]][rows, cols[jj] - colranges[e]]
			if ( x@transpose ) {
				y[jj,] <- as.vector(vals, mode=vmode)
			} else {
				y[,jj] <- as.vector(vals, mode=vmode)
			}
		}
	} else if ( is(x, "virtual_matr") ) {
		rowranges <- c(0, cumsum(sapply(x@data, nrow)))
		wh <- findInterval(rows, rowranges, left.open=TRUE)
		for ( ii in seq_along(rows) ) {
			e <- wh[ii]
			vals <- x@data[[e]][rows[ii] - rowranges[e], cols]
			if ( x@transpose ) {
				y[,ii] <- as.vector(vals, mode=vmode)
			} else {
				y[ii,] <- as.vector(vals, mode=vmode)
			}
		}
	}
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- dimnames(x)
	if ( drop ) 
		y <- drop(y)
	y
}

getVirtualMatrixRows <- function(x, i, drop=TRUE) {
	getVirtualMatrixElements(x, i, NULL, drop=drop)
}

getVirtualMatrixCols <- function(x, j, drop=TRUE) {
	getVirtualMatrixElements(x, NULL, j, drop=drop)	
}

getVirtualMatrix <- function(x) {
	getVirtualMatrixElements(x, NULL, NULL, drop=FALSE)
}

subVirtualMatrix <- function(x, i, j) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	if ( !is.null(i) ) {
		if ( any(i > x@dim[1]) )
			stop("subscript out of bounds")
		if ( is.null(x@index[[1]]) ) {
			x@index <- list(i, x@index[[2]])
		} else {
			x@index[[1]] <- x@index[[1]][j]
		}
		x@dim[1] <- length(i)
		if ( !is.null(dimnames(x)) )
			x@dimnames[[1]] <- x@dimnames[[1]][i]
	}
	if ( !is.null(j) ) {
		if ( any(j > x@dim[2]) )
			stop("subscript out of bounds")
		if ( is.null(x@index[[2]]) ) {
			x@index <- list(x@index[[1]], j)
		} else {
			x@index[[2]] <- x@index[[2]][j]
		}
		x@dim[2] <- length(j)
		if ( !is.null(dimnames(x)) )
			x@dimnames[[2]] <- x@dimnames[[2]][j]
	}
	x@length <- as.numeric(prod(x@dim))
	if ( validObject(x) )
		x
}

subVirtualMatrixRows <- function(x, i) {
	subVirtualMatrix(x, i, NULL)
}

subVirtualMatrixCols <- function(x, j) {
	subVirtualMatrix(x, NULL, j)	
}

setMethod("[",
	c(x = "virtual_mat", i = "missing", j = "missing"),
	function(x, ..., drop) getVirtualMatrix(x))

setMethod("[",
	c(x = "virtual_mat", j = "missing"),
	function(x, i, ..., drop) getVirtualMatrixRows(x, i, drop))

setMethod("[",
	c(x = "virtual_mat", i = "missing"),
	function(x, j, ..., drop) getVirtualMatrixCols(x, j, drop))

setMethod("[",
	c(x = "virtual_mat"),
	function(x, i, j, ..., drop) getVirtualMatrixElements(x, i, j, drop))

setMethod("[",
	c(x = "virtual_mat", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subVirtualMatrixRows(x, i))

setMethod("[",
	c(x = "virtual_mat", i = "missing", drop = "NULL"),
	function(x, j, ..., drop) subVirtualMatrixCols(x, j))

setMethod("[",
	c(x = "virtual_mat", drop = "NULL"),
	function(x, i, j, ..., drop) subVirtualMatrix(x, i, j))

# combine by rows

setMethod("combine_by_rows", c("virtual_matr", "virtual_matr"),
	function(x, y, ...)
{
	if ( ncol(x) != ncol(y) )
		stop("number of columns of matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	xi <- x@index
	yi <- y@index
	comformable_cols <- is.null(xi[[2]]) && is.null(yi[[2]])
	is_transposed <- x@transpose || y@transpose
	if ( comformable_cols && !is_transposed ) {
		if ( is.null(xi[[1]]) && is.null(yi[[1]]) ) {
			index <- NULL
		} else {
			if ( is.null(xi[[1]]) )
				xi[[1]] <- seq_len(nrow(x))
			if ( is.null(yi[[1]]) ) {
				yi[[1]] <- seq_len(nrow(y)) + nrow(x)
			} else {
				yi[[1]] <- yi[[1]] + nrow(x)
			}
			index <- list(c(xi[[1]], yi[[1]]), NULL)
		}
		new(class(x),
			data=c(x@data, y@data),
			datamode=x@datamode,
			paths=x@paths,
			filemode=x@filemode,
			length=x@length + y@length,
			dim=c(x@dim[1] + y@dim[1], x@dim[2]),
			names=NULL,
			dimnames=combine_rownames(x,y),
			ops=NULL,
			index=index,
			transpose=FALSE)
	} else {
		callNextMethod()
	}
})

setMethod("combine_by_rows", c("virtual_mat", "virtual_mat"),
	function(x, y, ...)
{
	if ( ncol(x) != ncol(y) )
		stop("number of columns of matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	new("virtual_matr",
		data=list(x, y),
		datamode=x@datamode,
		paths=x@paths,
		filemode=x@filemode,
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine_rownames(x,y),
		ops=NULL,
		index=NULL,
		transpose=FALSE)
})

setMethod("combine_by_rows", c("virtual_mat", "ANY"),
	function(x, y, ...) combine_by_rows(x, as(y, "virtual_matr")))

setMethod("combine_by_rows", c("ANY", "virtual_mat"),
	function(x, y, ...) combine_by_rows(as(x, "virtual_matr"), y))

setMethod("combine_by_rows", c("matter", "matter"),
	function(x, y, ...)
{
	combine_by_rows(as(x, "virtual_matr"), as(y, "virtual_matr"))
})

# combine by cols

setMethod("combine_by_cols", c("virtual_matc", "virtual_matc"),
	function(x, y, ...)
{
	if ( nrow(x) != nrow(y) )
		stop("number of rows of matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	xi <- x@index
	yi <- y@index
	comformable_rows <- is.null(xi[[1]]) && is.null(yi[[1]])
	is_transposed <- x@transpose || y@transpose
	if ( comformable_rows && !is_transposed ) {
		if ( is.null(xi[[2]]) && is.null(yi[[2]]) ) {
			index <- NULL
		} else {
			if ( is.null(xi[[2]]) )
				xi[[2]] <- seq_len(ncol(x))
			if ( is.null(yi[[2]]) ) {
				yi[[2]] <- seq_len(ncol(y)) + ncol(x)
			} else {
				yi[[2]] <- yi[[2]] + ncol(x)
			}
			index <- list(NULL, c(xi[[2]], yi[[2]]))
		}
		new(class(x),
			data=c(x@data, y@data),
			datamode=x@datamode,
			paths=x@paths,
			filemode=x@filemode,
			length=x@length + y@length,
			dim=c(x@dim[1], x@dim[2] + y@dim[2]),
			names=NULL,
			dimnames=combine_colnames(x,y),
			ops=NULL,
			index=index,
			transpose=FALSE)
	} else {
		callNextMethod()
	}
})

setMethod("combine_by_cols", c("virtual_mat", "virtual_mat"),
	function(x, y, ...)
{
	if ( nrow(x) != nrow(y) )
		stop("number of rows of matrices must match")
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	new("virtual_matc",
		data=list(x, y),
		datamode=x@datamode,
		paths=x@paths,
		filemode=x@filemode,
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine_colnames(x,y),
		ops=NULL,
		index=NULL,
		transpose=FALSE)
})

setMethod("combine_by_cols", c("virtual_mat", "ANY"),
	function(x, y, ...) combine_by_cols(x, as(y, "virtual_matc")))

setMethod("combine_by_cols", c("ANY", "virtual_mat"),
	function(x, y, ...) combine_by_cols(as(x, "virtual_matc"), y))

setMethod("combine_by_cols", c("matter", "matter"),
	function(x, y, ...)
{
	combine_by_cols(as(x, "virtual_matc"), as(y, "virtual_matc"))
})

# transpose

setMethod("t", "virtual_mat", function(x)
{
	x@dim <- rev(x@dim)
	x@dimnames <- rev(x@dimnames)
	x@index <- rev(x@index)
	x@transpose <- !x@transpose
	if ( validObject(x) )
		x
})

