
#### Define new generics from base R ####
## -------------------------------------

setGeneric("t")
setGeneric("sum")
setGeneric("mean")
setGeneric("var")
setGeneric("sd")
setGeneric("colSums")
setGeneric("rowSums")
setGeneric("colMeans")
setGeneric("rowMeans")

#### Define new generics for stats ####
## -------------------------------------

setGeneric("colVar", function(x, na.rm = FALSE) standardGeneric("colVar"))
setGeneric("rowVar", function(x, na.rm = FALSE) standardGeneric("rowVar"))
setGeneric("colSd", function(x, na.rm = FALSE) standardGeneric("colSd"))
setGeneric("rowSd", function(x, na.rm = FALSE) standardGeneric("rowSd"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------

setGeneric("datamode", function(x) standardGeneric("datamode"))
setGeneric("datamode<-", function(x, value) standardGeneric("datamode<-"))
setGeneric("filepath", function(x) standardGeneric("filepath"))
setGeneric("filepath<-", function(x, value) standardGeneric("filepath<-"))
setGeneric("filemode", function(x) standardGeneric("filemode"))
setGeneric("filemode<-", function(x, value) standardGeneric("filemode<-"))
setGeneric("chunksize", function(x) standardGeneric("chunksize"))
setGeneric("chunksize<-", function(x, value) standardGeneric("chunksize<-"))

#### Miscellaneous utility functions ####
## --------------------------------------

logical2index <- function(x, i, margin) {
	if ( missing(margin) ) {
		len <- length(x)
	} else {
		len <- dim(x)[margin]
	}
	as.numeric(which(rep(i, length.out=len)))
}

names2index <- function(x, i)
	as.numeric(match(i, names(x)))

dimnames2index <- function(x, i, margin)
	as.numeric(match(i, dimnames(x)[[margin]]))

sizeof <- function(type) {
	type <- as.character(type)
	vapply(type, switch, numeric(1),
		short = 2,
		int = 4,
		long = 8,
		float = 4,
		double = 8)
}

drop.matrix <- function(x) {
	dmn <- dimnames(x)
	if ( dim(x)[1] == 1 ) {
		x <- as.vector(x)
		names(x) <- dmn[[1]]
	} else if ( dim(x)[2] == 1 ) {
		x <- as.vector(x)
		names(x) <- dmn[[2]]
	}
	x
}

combine.colnames <- function(x, y) {
	if ( is.null(dimnames(x)[[2]]) && is.null(dimnames(y)[[2]]) ) {
		colnames <- NULL
	} else if ( is.null(dimnames(x)[[2]]) ) {
		colnames <- c(character(dim(x)[2]), dimnames(y)[[2]])
	} else if ( is.null(dimnames(y)[[2]]) ) {
		colnames <- c(dimnames(x)[[2]], character(dim(x)[2]))
	} else {
		colnames <- c(dimnames(x)[[2]], dimnames(x)[[2]])
	}
	if ( !is.null(dimnames(x)[[1]]) ) {
		rownames <- dimnames(x)[[1]]
	} else {
		rownames <- dimnames(y)[[1]]
	}
	if ( is.null(rownames) && is.null(colnames) ) {
		NULL
	} else {
		list(rownames, colnames)
	}
}

combine.rownames <- function(x, y) {
	if ( is.null(dimnames(x)[[1]]) && is.null(dimnames(y)[[1]]) ) {
		rownames <- NULL
	} else if ( is.null(dimnames(x)[[1]]) ) {
		rownames <- c(character(dim(x)[1]), dimnames(y)[[1]])
	} else if ( is.null(dimnames(y)[[1]]) ) {
		rownames <- c(dimnames(x)[[1]], character(dim(x)[1]))
	} else {
		rownames <- c(dimnames(x)[[1]], dimnames(x)[[1]])
	}
	if ( !is.null(dimnames(x)[[2]]) ) {
		colnames <- dimnames(x)[[2]]
	} else {
		colnames <- dimnames(y)[[2]]
	}
	if ( is.null(rownames) && is.null(colnames) ) {
		NULL
	} else {
		list(rownames, colnames)
	}
}

#### Define data types and utility functions for them ####
## -------------------------------------------------------

make.datamode <- function(datamode, type=c("C", "R")) {
	levels <- switch(match.arg(type),
		C = c("short", "int", "long", "float", "double"),
		R = c("integer", "numeric"))
	if ( missing(datamode) )
		return(factor(levels=levels))
	if ( is.numeric(datamode) ) {
		datamode <- as.integer(datamode)
		factor(datamode, levels=seq_along(levels), labels=levels)
	} else if ( is.character(datamode) ) {
		datamode <- tolower(datamode)
		if ( any(!datamode %in% levels) )
			stop("unsupported data type")
		factor(datamode, levels=levels)
	} else {
		as.factor(datamode)
	}
}

tabulate.datamode <- function(x) {
	if ( class(x) == "atoms" ) {
		x <- as.character(datamode(x))
	} else if ( class(x) == "list" ) {
		x <- unlist(lapply(x, function(xs)
			as.character(datamode(xs))))
	}
	summary(make.datamode(x, type="C"))
}

widest.datamode <- function(x, from=c("C", "R")) {
	counts <- tabulate.datamode(x)
	tdatamode <- make.datamode(max(which(counts > 0)))
	if ( from == "C" ) {
		make.datamode(switch(as.character(tdatamode),
		short = "integer",
		int = "integer",
		long = "numeric",
		float = "numeric",
		double = "numeric"), type="R")
	} else if ( from == "R" ) {
		make.datamode(switch(as.character(tdatamode),
			integer = "int",
			numeric = "double"), type="C")
	}
}

coerce <- function(x, datamode) {
	datamode <- as.character(datamode)
	if ( datamode == "integer" && is.double(x) )
		warning("coercing 'double' to 'integer' precision")
	switch(datamode,
		integer = as.integer(x),
		numeric = as.numeric(x))
}

#### Define atoms class ####
## -------------------------

setClass("atoms",
	slots = c(
		length = "integer",
		file_id = "factor",
		datamode = "factor",
		offset = "numeric", # byte offset from start of file
		extent = "numeric", # number of elements
		index_offset = "numeric", # cumulative index of first element
		index_extent = "numeric"), # cumulative index of one-past-the-end
	validity = function(object) {
		lens <- c(file_id=length(object@file_id),
			datamode=length(object@datamode),
			offset=length(object@offset),
			extent=length(object@extent),
			index_offset=length(object@index_offset),
			index_extent=length(object@index_extent))
		if ( length(unique(lens)) != 1 )
			stop("lengths of 'file_id' [", lens["file_id"], "], ",
				"'datamode' [", lens["datamode"], "], ",
				"'offset' [", lens["offset"], "], ",
				"'extent' [", lens["extent"], "], ",
				"'index_offset' [", lens["index_offset"], "], ",
				"and 'index_extent' [", lens["index_extent"], "], ",
				"must all be equal")
		if ( object@length != unique(lens) )
			stop("'length' not equal to length of object elements")
		if ( object@index_offset[1] != 0 )
			stop("'index_offset' must begin at 0")
		C_datamodes <- levels(make.datamode(type="C"))
		if ( any(!as.character(object@datamode) %in% C_datamodes) )
			stop("'datamode' should be one of [",
				paste(C_datamodes, collapse=", "), "]")
		extent <- object@index_extent - object@index_offset
		if ( any(extent != object@extent) )
			stop("'index_offset' or 'index_extent' incongruent with 'extent'")
		index_offset.drop <-  object@index_offset[-1L]
		index_extent.drop <-  object@index_extent[-length(object@index_extent)]
		if ( any(index_offset.drop != index_extent.drop) )
			stop("'index_offset' or 'index_extent' are non-contiguous")
		length <- sum(object@extent)
		if ( length != object@index_extent[length(object@index_extent)] )
			stop("'index_extent' must terminate at sum of 'extent' [", length, "]")
	})

atoms <- function(file_id = factor(NA), datamode=make.datamode("double", type="C"),
				offset = numeric(1), extent = numeric(1))
{
	new("atoms",
		length=as.integer(length(file_id)),
		file_id=as.factor(file_id),
		datamode=make.datamode(datamode),
		offset=as.numeric(offset),
		extent=as.numeric(extent),
		index_offset=as.numeric(c(0, cumsum(extent)[-length(extent)])),
		index_extent=as.numeric(cumsum(extent)))
}

setMethod("datamode", "atoms", function(x) x@datamode)

setReplaceMethod("datamode", "atoms", function(x, value) {
	x@datamode <- value
	x
})

setMethod("combine", "atoms", function(x, y, ...) {
	atoms(file_id=factor(c(
			as.character(x@file_id),
			as.character(y@file_id))),
		datamode=make.datamode(c(
			as.character(x@datamode),
			as.character(y@datamode))),
		offset=c(x@offset, y@offset),
		extent=c(x@extent, y@extent))
})

setMethod("c", "atoms", function(x, ..., recursive=FALSE)
{
	dots <- list(...)
	if ( length(dots) == 0 ) {
		x
	} else if ( length(dots) == 1 ) {
		combine(x, dots[[1]])
	} else {
		do.call(combine, list(x, ...))
	}
})

setMethod("show", "atoms", function(object) {
	print(data.frame(
		file_id=as.integer(object@file_id),
		datamode=object@datamode,
		offset=object@offset,
		extent=object@extent,
		index_offset=object@index_offset,
		index_extent=object@index_extent))
})

#### Define matter VIRTUAL class ####
## ----------------------------------

setClassUnion("_atoms", c("atoms", "list"))
setClassUnion("_dim", c("integer", "NULL"))
setClassUnion("_names", c("character", "NULL"))
setClassUnion("_dimnames", c("list", "NULL"))

setClass("matter",
	slots = c(
		data = "_atoms",
		datamode = "factor",
		filepath = "character",
		filemode = "character",
		chunksize = "integer",
		length = "numeric",
		dim = "_dim",
		names = "_names",
		dimnames = "_dimnames"),
	contains = "VIRTUAL",
	validity = function(object) {
		if ( !is.null(object@filepath) && any(!file.exists(object@filepath)) )
			stop("file [", which(!file.exists(object@filepath)), "] does not exist")
		C_readmodes <- c("rb", "rb+")
		if ( length(object@filemode) != 1 || !object@filemode %in% C_readmodes )
			stop("'filemode' should be one of [",
				paste(C_readmodes, collapse=", "), "]")
		R_datamodes <- levels(make.datamode(type="R"))
		if ( !as.character(object@datamode) %in% R_datamodes )
			stop("'datamode' should be one of [",
				paste(R_datamodes, collapse=", "), "]")
		if ( !object@chunksize > 0L )
			stop("chunksize must be positive")
		if ( !is.null(object@dim) && prod(object@dim) != object@length )
			stop("dims [product ", prod(object@dim), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(object@names) && length(object@names) != object@length )
			stop("names [length ", length(object@names), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(dimnames) && is.null(dim) )
			stop("'dimnames' applied to non-array")
		if ( !is.null (object@dimnames) ) {
			if ( is.null(object@dim) )
				stop("'dimnames' applied to non-array")
			if ( length(object@dimnames) != length(object@dim) )
				stop("length of 'dimnames' [", length(object@dimnames), "] ",
					"must match that of 'dims' [", length(object@dim), "]")
			for ( i in seq_along(object@dimnames) ) {
				dmn <- object@dimnames[[i]]
				if ( !is.null(dmn) && length(dmn) != object@dim[i] )
					stop("length of 'dimnames' [", i, "] ",
						"not equal to array extent")
			}
		}
	})

matter <- function(...) {
	# need to implement
}

setMethod("datamode", "matter", function(x) x@datamode)

setReplaceMethod("datamode", "matter", function(x, value) {
	x@datamode <- value
	x
})

setMethod("filepath", "matter", function(x) x@filepath)

setReplaceMethod("filepath", "matter", function(x, value) {
	x@filepath <- value
	x
})

setMethod("filemode", "matter", function(x) x@filemode)

setReplaceMethod("filemode", "matter", function(x, value) {
	x@filemode <- value
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

setMethod("sum", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getSum", x, na.rm)
})

setMethod("mean", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getMean", x, na.rm)
})

setMethod("var", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getVar", x, na.rm)
})

setMethod("sd", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	sqrt(.Call("getVar", x, na.rm))
})

setMethod("colSums", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getColSums", x, na.rm)
})

setMethod("colMeans", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getColMeans", x, na.rm)
})

setMethod("colVar", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getColVar", x, na.rm)
})

setMethod("colSd", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	sqrt(.Call("getColVar", x, na.rm))
})

setMethod("rowSums", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getRowSums", x, na.rm)
})

setMethod("rowMeans", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getRowMeans", x, na.rm)
})

setMethod("rowVar", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	.Call("getRowVar", x, na.rm)
})

setMethod("rowSd", "matter", function(x, na.rm = FALSE) {
	if ( datamode(x) == "integer" )
		warning("NAs not yet handled correctly for datamode 'integer'")
	sqrt(.Call("getRowVar", x, na.rm))
})

#### Define matter<vector> class for vector-like data ####
## --------------------------------------------------------

setClass("matter_vec",
	prototype = prototype(
		data = atoms(),
		datamode = make.datamode("numeric", type="R"),
		filepath = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = NULL,
		names = NULL,
		dimnames = NULL),
	contains = "matter",
	validity = function(object) {
		if ( !is.null(object@dim) )
			stop("vector must have NULL 'dim'")
		if ( !is.null(object@dimnames) )
			stop("vector must have NULL 'dimnames'")
	})

matter_vec <- function(data, datamode = "double", filepath = NULL,
					filemode = ifelse(is.null(filepath), "rb+", "rb"),
					offset = 0, extent = length, length = 0, names = NULL, ...)
{
	if ( length == 0 && all(extent == 0) )
		return(new("matter_vec"))
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=length(extent))
	if ( is.null(filepath) ) {
		if ( missing(data) )
			data <- NA
		filemode <- force(filemode)
		filepath <- tempfile(fileext=".bin")
		result <- file.create(filepath)
		if ( !result )
			stop("error creating file")
	}
	if ( length(filepath) != length(extent) )
		filepath <- rep(filepath, length.out=length(extent))
	x <- new("matter_vec",
		data=atoms(
			file_id=factor(filepath),
			datamode=make.datamode(datamode, type="C"),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=widest.datamode(datamode, from="C"),
		filepath=levels(factor(filepath)),
		filemode=filemode,
		length=as.numeric(sum(extent)),
		dim=NULL,
		names=names,
		dimnames=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

getVector <- function(x) {
	y <- .Call("getVector", x)
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	y
}

setVector <- function(x, value) {
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	value <- rep(value, length.out=length(x)) # should do this in C++ code
	value <- coerce(value, datamode(x))
	.Call("setVector", x, value)
	if ( validObject(x) )
		invisible(x)
}

getVectorElements <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	y <- .Call("getVectorElements", x, i - 1)
	if ( !is.null(names(x)) )
		names(y) <- names(x)[i]
	y	
}

setVectorElements <- function(x, i, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	value <- rep(value, length.out=length(i))
	value <- coerce(value, datamode(x))
	.Call("setVectorElements", x, i - 1, value)
	if ( validObject(x) )
		invisible(x)	
}

setMethod("[",
	c(x = "matter_vec", i = "missing", j = "missing"),
	function(x, ...) getVector(x))

setReplaceMethod("[",
	c(x = "matter_vec", i = "missing", j = "missing"),
	function(x, ..., value) setVector(x, value))

setMethod("[",
	c(x = "matter_vec", i = "ANY", j = "missing"),
	function(x, i, ...) getVectorElements(x, i))

setReplaceMethod("[",
	c(x = "matter_vec", i = "ANY", j = "missing"),
	function(x, i, ..., value) setVectorElements(x, i, value))

setMethod("combine", "matter_vec", function(x, y, ...) {
	data <- combine(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest.datamode(data, from="C"),
		filepath=levels(factor(c(x@filepath, y@filepath))),
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=NULL,
		names=NULL,
		dimnames=NULL)
})

setMethod("c", "matter_vec", function(x, ...)
{
	dots <- list(...)
	if ( length(dots) == 0 ) {
		x
	} else if ( length(dots) == 1 ) {
		combine(x, dots[[1]])
	} else {
		do.call(combine, list(x, ...))
	}
})

setMethod("t", "matter_vec", function(x)
{
	class(x) <- "matter_matr"
	x@data <- list(x@data)
	x@dim <- c(1L, as.integer(x@length))
	if ( !is.null(x@names) )
		x@dimnames <- list(NULL, x@namesL)
	x@names <- NULL
	if ( validObject(x) )
		x
})

#### Define matter<matrix> classes for matrix-like data ####
## --------------------------------------------------------

setClass("matter_mat",
	contains = c("matter", "VIRTUAL"),
	prototype = prototype(
		data = list(atoms()),
		datamode = make.datamode("numeric", type="R"),
		filepath = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL),
	validity = function(object) {
		if ( is.null(object@dim) )
			stop("matrix must have non-NULL 'dim'")
		if ( length(object@dim) != 2 )
			stop("matrix must have 'dim' of length 2")
	})

setClass("matter_matc",
	contains = "matter_mat",
	prototype = prototype(
		data = list(),
		datamode = make.datamode("numeric", type="R"),
		filepath = character(),
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
		datamode = make.datamode("numeric", type="R"),
		filepath = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L,0L),
		names = NULL,
		dimnames = NULL))

matter_mat <- function(data, datamode = "double", filepath = NULL,
					filemode = ifelse(is.null(filepath), "rb+", "rb"),
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
			factor(filepath),
			make.datamode(datamode, type="C"),
			as.numeric(offset),
			as.numeric(extent))
	}
	if ( is.null(filepath) && prod(c(nrow, ncol)) > 0 ) {
		if ( missing(data) )
			data <- NA
		filemode <- force(filemode)
		filepath <- tempfile(fileext=".bin")
		result <- file.create(filepath)
		if ( !result )
			stop("error creating file")
	}
	if ( prod(c(nrow, ncol)) == 0 )
		filepath <- as.character(NA)
	if ( length(filepath) != length(extent) )
		filepath <- rep(filepath, length.out=max(length(extent), 1))
	x <- new(mclass,
		data=adata(),
		datamode=widest.datamode(datamode, from="C"),
		filepath=levels(factor(filepath)),
		filemode=filemode,
		length=as.numeric(prod(c(nrow, ncol))),
		dim=as.integer(c(nrow, ncol)),
		names=NULL,
		dimnames=dimnames, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

getMatrix <- function(x) {
	rowMaj <- switch(class(x), matter_matr=TRUE, matter_matc=FALSE)
	y <- .Call("getMatrix", x, rowMaj)
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
	value <- coerce(value, datamode(x))
	.Call("setMatrix", x, value, rowMaj)
	if ( validObject(x) )
		invisible(x)
}

getMatrixRows <- function(x, i, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- .Call("getMatrixRows", x, i - 1)
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x)[i], colnames(x))
	if ( drop ) 
		y <- drop.matrix(y)
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
	value <- coerce(value, datamode(x))
	.Call("setMatrixRows", x, i - 1, value)
	if ( validObject(x) )
		invisible(x)
}

getMatrixCols <- function(x, j, drop=TRUE) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- .Call("getMatrixCols", x, j - 1)
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x), colnames(x)[j])
	if ( drop ) 
		y <- drop.matrix(y)
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
	value <- rep(value, length.out=length(j) * nrow(x))
	value <- coerce(value, datamode(x))
	.Call("setMatrixCols", x, j - 1, value)
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
	y <- .Call("getMatrixElements", x, i - 1, j - 1)
	if ( !is.null(dimnames(x)) )
		dimnames(y) <- list(rownames(x)[i], colnames(x)[j])
	if ( drop ) 
		y <- drop.matrix(y)
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
	value <- coerce(value, datamode(x))
	.Call("setMatrixElements", x, i - 1, j - 1, value)
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
	c(x = "matter_mat", j = "missing", drop = "logical"),
	function(x, i, ..., drop) getMatrixRows(x, i, drop))

setMethod("[",
	c(x = "matter_mat", i = "missing", drop = "missing"),
	function(x, j, ...) getMatrixCols(x, j, drop))

setMethod("[",
	c(x = "matter_mat", i = "missing", drop = "logical"),
	function(x, j, ..., drop) getMatrixCols(x, j, drop))

setMethod("[",
	c(x = "matter_mat", drop = "missing"),
	function(x, i, j, ...) getMatrixElements(x, i, j, drop))

setMethod("[",
	c(x = "matter_mat", drop = "logical"),
	function(x, i, j, ..., drop) getMatrixElements(x, i, j, drop))

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
	if ( nrow(x) != nrow(y) )
		stop("number of rows of matrices must match")
	filepaths <- levels(factor(c(x@filepath, y@filepath)))
	data <- lapply(append(x@data, y@data), function(xs) {
		xs@file_id <- factor(xs@file_id, levels=filepaths)
		xs
	})
	new(class(x),
		data=data,
		datamode=widest.datamode(data, from="C"),
		filepath=filepaths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine.colnames(x,y))
})

setMethod("cbind", "matter_matc", function(..., deparse.level=1)
{
	dots <- list(...)
	if ( length(dots) == 1 ) {
		dots[[1]]
	} else {
		do.call(combine, dots)
	}
})

setMethod("combine", "matter_matr", function(x, y, ...) {
	if ( ncol(x) != ncol(y) )
		stop("number of columns of matrices must match")
	filepaths <- levels(factor(c(x@filepath, y@filepath)))
	data <- lapply(append(x@data, y@data), function(xs) {
		xs@file_id <- factor(xs@file_id, levels=filepaths)
		xs
	})
	new(class(x),
		data=data,
		datamode=widest.datamode(data, from="C"),
		filepath=filepaths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine.rownames(x,y))
})

setMethod("rbind", "matter_matr", function(..., deparse.level=1)
{
	dots <- list(...)
	if ( length(dots) == 1 ) {
		dots[[1]]
	} else {
		do.call(combine, dots)
	}
})

setMethod("cbind", "matter_matr", function(..., deparse.level=1)
{
	stop("cannot 'cbind' row-major matrices")
})

setMethod("rbind", "matter_matc", function(..., deparse.level=1)
{
	stop("cannot 'rbind' column-major matrices")
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
	.Call("rightMultRMatrix", x, y)
})

setMethod("%*%", c("matrix", "matter_mat"), function(x, y)
{
	.Call("leftMultRMatrix", x, y)
})

setMethod("%*%", c("matter", "matter"), function(x, y)
{
	stop("at least one matrix must be in memory")
})

#### Apply functions over matter matrices ####
## -------------------------------------------

setMethod("apply", "matter_mat",
	function(X, MARGIN, FUN, ...) {
		apply.matter(X, MARGIN, FUN, ...)
})

# based on code from package:base and package:biganalytics

apply.matter <- function(X, MARGIN, FUN, ...)
{
	if ( length(MARGIN) > 1 ) 
		stop("dim(MARGIN) > 1 not supported for 'matter' objects")
	FUN <- match.fun(FUN)
	dn.ans <- dimnames(X)[MARGIN]
	if ( MARGIN == 1 ) {
		d2 <- nrow(X)
		ans <- vector("list", nrow(X))
		for ( i in 1:d2 ) {
			tmp <- FUN(X[i,], ...)
			if (!is.null(tmp))
				ans[[i]] <- tmp
		}
	} else if ( MARGIN == 2 ) {
		d2 <- ncol(X)
		ans <- vector("list", ncol(X))
		for (i in 1:d2) {
			tmp <- FUN(X[,i], ...)
			if (!is.null(tmp))
				ans[[i]] <- tmp
		}
	} else {
		stop("only MARGIN = 1 or 2 supported for 'matter' objects")
	}
	ans.list <- is.recursive(ans[[1]])
	l.ans <- length(ans[[1]])
	ans.names <- names(ans[[1]])
	if ( !ans.list )
		ans.list <- any(unlist(lapply(ans, length)) != l.ans)
	if ( !ans.list && length(ans.names) ) {
		all.same <- sapply(ans, function(x) identical(names(x), ans.names))
		if (!all(all.same))
			ans.names <- NULL
	}
	if ( ans.list ) {
		len.a <- d2
	} else {
		len.a <- length(ans <- unlist(ans, recursive = FALSE))
	}
	if ( len.a == d2 ) {
		if ( length(dn.ans[[1]]) )
			names(ans) <- dn.ans[[1]]
		return(ans)
	}
	if ( len.a > 0 && len.a %% d2 == 0 ) {
		if ( is.null(dn.ans) )
			dn.ans <- vector(mode = "list", length(d2))
		dn.ans <- c(list(ans.names), dn.ans)
		return(array(ans, c(len.a%/%d2, d2), if (!all(sapply(dn.ans, 
			is.null))) dn.ans))
	}
	return(ans)
}

#### Linear regression for matter matrices ####
## -------------------------------------------

setMethod("bigglm", c("formula", "matter_mat"),
	function(formula, data, ..., chunksize = NULL, factors = NULL) {
		bigglm.matter(formula, data, ...,
			chunksize=chunksize, fc=factors)
})

# based on code from package:biglm and package:biganalytics

bigglm.matter <- function(formula, data, ..., chunksize, fc) {
	n <- nrow(data)
	vars <- unique(c(all.vars(formula), fc))
	p <- length(vars)
	if ( is.null(chunksize) )
		chunksize <- chunksize(data) %/% p
	if ( !is.null(fc) ) {
		flevels <- lapply(fc, function(x) sort(unique(x[,fc])))
		names(flevels) <- fc
	}
	current <- 1
	getNextDataChunk <- function(reset = FALSE) {
		if ( reset ) {
			current <<- 1
			return(NULL)
		}
		if ( current > n )
			return(NULL)
		chunkrange <- current:(current + min(chunksize, n - current))
		chunk <- as.data.frame(data[chunkrange,vars])
		if ( !is.null(fc) ) {
			for ( name in names(flevels) )
				chunk[,name] <- factor(chunk[,name], levels=flevels[name])
		}
		current <<- max(chunkrange) + 1
		chunk
	}
	bigglm(formula, getNextDataChunk, ...)
}

