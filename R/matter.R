
#### Define new generics from base R ####
## -------------------------------------

setGeneric("t")
setGeneric("sum")
setGeneric("mean")
setGeneric("var", signature="x")
setGeneric("sd", signature="x")
# setGeneric("colSums", signature="x") # Use S4Vectors or BiocGenerics(?)
# setGeneric("rowSums", signature="x") # Use S4Vectors or BiocGenerics(?)
# setGeneric("colMeans", signature="x") # Use S4Vectors or BiocGenerics(?)
# setGeneric("rowMeans", signature="x") # Use S4Vectors or BiocGenerics(?)
setGeneric("apply", signature="X")
setGeneric("scale", signature="x")

#### Define new generics from stats ####
## -------------------------------------

setGeneric("prcomp")

#### Define new generics for summary stats ####
## --------------------------------------------

# Do these conditionally in case user has generics from matrixStats package

if ( !isGeneric("colVars") )
	setGeneric("colVars", function(x, na.rm = FALSE) standardGeneric("colVars"))
if ( !isGeneric("rowVars") )
	setGeneric("rowVars", function(x, na.rm = FALSE) standardGeneric("rowVars"))
if ( !isGeneric("colSds") )
	setGeneric("colSds", function(x, na.rm = FALSE) standardGeneric("colSds"))
if ( !isGeneric("rowSds") )
	setGeneric("rowSds", function(x, na.rm = FALSE) standardGeneric("rowSds"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------	

setGeneric("adata", function(object) standardGeneric("adata"))
setGeneric("datamode", function(x) standardGeneric("datamode"))
setGeneric("datamode<-", function(x, value) standardGeneric("datamode<-"))
setGeneric("paths", function(x) standardGeneric("paths"))
setGeneric("paths<-", function(x, value) standardGeneric("paths<-"))
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

all.indices <- function(x, i, margin) {
	if ( missing(margin) ) {
		all(i == seq_len(length(x)))
	} else {
		all(i == seq_len(dim(x)[margin]))
	}
}

sizeof <- function(type) {
	type <- make.datamode(type, type="C")
	vapply(as.character(type), switch, numeric(1),
		short = 2,
		int = 4,
		long = 8,
		float = 4,
		double = 8)
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
		x <- datamode(x)
	} else if ( class(x) == "list" ) {
		x <- unlist(lapply(x, function(xs)
			datamode(xs)))
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

disk.used <- function(x) {
	if ( is.list(x) ) {
		bytes <- sum(vapply(x, disk.used, numeric(1)))
	} else {
		bytes <- sum(x@extent * sizeof(datamode(x)))
	}
	class(bytes) <- "bytes"
	bytes
}

# based on utils::format.object_size

show.bytes <- function (x, units = "auto", ...)  {
    units <- match.arg(units, c("auto",
				"B", "KB", "MB", "GB", "TB", "PB"))
    if (units == "auto")
        units <- if (x >= 1000^4) 
            "TB"
        else if (x >= 1000^3) 
            "GB"
        else if (x >= 1000^2) 
            "MB"
        else if (x >= 1000) 
            "KB"
        else "B"
    switch(units, 
    	B = c("bytes"=x),
    	KB = c("KB"=round(x/1000, 1L)),
    	MB = c("MB"=round(x/1000^2, 1L)), 
        GB = c("GB"=round(x/1000^3, 1L)),
        TB = c("TB"=round(x/1000^4, 1L)),
        PB = c("PB"=round(x/1000^5, 1L)))
}

format.bytes <- function(x, units = "auto", ...) {
	bytes <- show.bytes(x, units=units)
	paste(bytes, names(bytes))
}

# based on pryr::mem_used and pryr::mem_change

mem <- function(x, reset = FALSE) {
	if ( !missing(x) ) {
		mem <- as.numeric(object.size(x))
		mem <- show.bytes(mem)
	} else {
		cell.size <- c(Ncells=56, Vcells=8)
		mem <- round(colSums(gc(reset=reset)[,c(1,3,5)] * cell.size) / 1000^2, 1)
		names(mem) <- c("used (MB)", "gc trigger (MB)", "max used (MB)")
	}
	mem
}

profile <- function(expr, reset = FALSE) {
	start <- mem(reset = TRUE)
	t.start <- proc.time()
	expr <- substitute(expr)
	eval(expr, parent.frame())
	rm(expr)
	t.end <- proc.time()
	end <- mem()
	mem <- c(start[1], end[1], end[3], end[3] - end[1], t.end[3] - t.start[3])
	names(mem) <- c("start (MB)", "finish (MB)",
		"max used (MB)", "overhead (MB)", "time (sec)")
	mem
}


#### Define atoms class ####
## -------------------------

setClass("atoms",
	slots = c(
		length = "integer",
		source_id = "integer",
		datamode = "integer",
		offset = "numeric", # byte offset from start of file
		extent = "numeric", # number of elements
		index_offset = "numeric", # cumulative index of first element
		index_extent = "numeric"), # cumulative index of one-past-the-end
	validity = function(object) {
		errors <- NULL
		lens <- c(source_id=length(object@source_id),
			datamode=length(object@datamode),
			offset=length(object@offset),
			extent=length(object@extent),
			index_offset=length(object@index_offset),
			index_extent=length(object@index_extent))

		if ( length(unique(lens)) != 1 )
			stop("lengths of 'source_id' [", lens["source_id"], "], ",
				"'datamode' [", lens["datamode"], "], ",
				"'offset' [", lens["offset"], "], ",
				"'extent' [", lens["extent"], "], ",
				"'index_offset' [", lens["index_offset"], "], ",
				"and 'index_extent' [", lens["index_extent"], "], ",
				"must all be equal")
		if ( object@length != unique(lens) )
			errors <- c(errors, "'length' not equal to length of object elements")
		if ( object@index_offset[1] != 0 )
			errors <- c(errors, "'index_offset' must begin at 0")
		# C_datamodes <- levels(make.datamode(type="C"))
		# if ( any(!as.character(object@datamode) %in% C_datamodes) )
		# 	errors <- c(errors, "'datamode' should be one of [",
		# 		paste(C_datamodes, collapse=", "), "]")
		extent <- object@index_extent - object@index_offset
		if ( any(extent != object@extent) )
			errors <- c(errors, "'index_offset' or 'index_extent' incongruent with 'extent'")
		index_offset.drop <-  object@index_offset[-1L]
		index_extent.drop <-  object@index_extent[-length(object@index_extent)]
		if ( any(index_offset.drop != index_extent.drop) )
			errors <- c(errors, "'index_offset' or 'index_extent' are non-contiguous")
		length <- sum(object@extent)
		if ( length != object@index_extent[length(object@index_extent)] )
			errors <- c(errors, "'index_extent' must terminate at sum of 'extent' [", length, "]")
		if ( is.null(errors) ) TRUE else errors
	})

atoms <- function(source_id = as.integer(NA), datamode="double",
					offset = numeric(1), extent = numeric(1))
{
	new("atoms",
		length=as.integer(length(source_id)),
		source_id=as.integer(source_id),
		datamode=as.integer(make.datamode(datamode, type="C")),
		offset=as.numeric(offset),
		extent=as.numeric(extent),
		index_offset=as.numeric(c(0, cumsum(extent)[-length(extent)])),
		index_extent=as.numeric(cumsum(extent)))
}

setMethod("datamode", "atoms", function(x) x@datamode)

setReplaceMethod("datamode", "atoms", function(x, value) {
	x@datamode <- as.integer(make.datamode(value, type="C"))
	x
})

setMethod("combine", "atoms", function(x, y, ...) {
	atoms(source_id=c(x@source_id, y@source_id),
		datamode=c(x@datamode, y@datamode),
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
		source_id=object@source_id,
		datamode=make.datamode(object@datamode, type="C"),
		offset=object@offset,
		extent=object@extent,
		index_offset=object@index_offset,
		index_extent=object@index_extent))
})

#### Define matter VIRTUAL class ####
## ----------------------------------

setClassUnion("atomsORlist", c("atoms", "list"))
setClassUnion("characterORNULL", c("character", "NULL")) # can't find S4Vectors export (???)
setClassUnion("integerORNULL", c("integer", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))

setClass("matter",
	slots = c(
		data = "atomsORlist",
		datamode = "factor",
		paths = "character",
		filemode = "character",
		chunksize = "integer",
		length = "numeric",
		dim = "integerORNULL",
		names = "characterORNULL",
		dimnames = "listORNULL"),
	contains = "VIRTUAL",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@paths) && any(!file.exists(object@paths)) )
			errors <- c(errors, "file [", which(!file.exists(object@paths)), "] does not exist")
		C_readmodes <- c("rb", "rb+")
		if ( length(object@filemode) != 1 || !object@filemode %in% C_readmodes )
			errors <- c(errors, "'filemode' should be one of [",
				paste(C_readmodes, collapse=", "), "]")
		R_datamodes <- levels(make.datamode(type="R"))
		if ( !as.character(object@datamode) %in% R_datamodes )
			errors <- c(errors, "'datamode' should be one of [",
				paste(R_datamodes, collapse=", "), "]")
		if ( !object@chunksize > 0L )
			errors <- c(errors, "chunksize must be positive")
		if ( !is.null(object@dim) && prod(object@dim) != object@length )
			errors <- c(errors, "dims [product ", prod(object@dim), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(object@names) && length(object@names) != object@length )
			errors <- c(errors, "names [length ", length(object@names), "] ",
				"do not match length of object [", object@length, "]")
		if ( !is.null(dimnames) && is.null(dim) )
			errors <- c(errors, "'dimnames' applied to non-array")
		if ( !is.null (object@dimnames) ) {
			if ( is.null(object@dim) )
				errors <- c(errors, "'dimnames' applied to non-array")
			if ( length(object@dimnames) != length(object@dim) )
				errors <- c(errors, "length of 'dimnames' [", length(object@dimnames), "] ",
					"must match that of 'dims' [", length(object@dim), "]")
			for ( i in seq_along(object@dimnames) ) {
				dmn <- object@dimnames[[i]]
				if ( !is.null(dmn) && length(dmn) != object@dim[i] )
					errors <- c(errors, "length of 'dimnames' [", i, "] ",
						"not equal to array extent")
			}
		}
		if ( is.null(errors) ) TRUE else errors
	})

matter <- function(...) {
	dots <- match.call(expand.dots=FALSE)$...
	nm <- names(dots)
	if ( "data" %in% nm ) {
		data <- dots$data
	} else {
		data <- eval(dots[[1]])
	}
	if ( "extent" %in% nm ) {
		uneq.extent <- length(unique(eval(dots$extent))) > 1
	} else {
		uneq.extent <- FALSE
	}
	vec.args <- c("length", "names")
	mat.args <- c("nrow", "ncol", "dimnames", "rowMaj")
	if ( any(vec.args %in% nm ) || is.vector(dots) || uneq.extent ) {
		matter_vec(...)
	} else if ( any(mat.args %in% nm ) || is.matrix(dots) ) {
		matter_mat(...)
	} else {
		matter_mat(...)
	}
}

setMethod("adata", "matter", function(object) object@data)

setMethod("show", "matter", function(object) {
	object.memory <- object.size(object)
	class(object.memory) <- "bytes"
	cat("    sources: ", length(object@paths), "\n", sep="")
	cat("    datamode: ", paste(object@datamode), "\n", sep="")
	cat("    ", format(object.memory, units="auto"), " in-memory\n", sep="")
	cat("    ", format(disk.used(object@data), units="auto"), " on-disk\n", sep="")
})

setMethod("datamode", "matter", function(x) x@datamode)

setReplaceMethod("datamode", "matter", function(x, value) {
	x@datamode <- value
	x
})

setMethod("paths", "matter", function(x) x@paths)

setReplaceMethod("paths", "matter", function(x, value) {
	x@paths <- value
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
	ret <- .Call("C_getSum", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("mean", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getMean", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("var", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getVar", x, na.rm)
	names(ret) <- names(x)
	ret
})

setMethod("sd", "matter", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getVar", x, na.rm))
	names(ret) <- names(x)
	ret
})

setMethod("colSums", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColSums", x, na.rm)
	names(ret) <- colnames(x)
	ret
})

setMethod("colMeans", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColMeans", x, na.rm)
	names(ret) <- colnames(x)
	ret	
})

setMethod("colVars", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getColVars", x, na.rm)
	names(ret) <- colnames(x)
	ret
})

setMethod("colSds", "matter", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getColVars", x, na.rm))
	names(ret) <- colnames(x)
	ret
})

setMethod("rowSums", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowSums", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowMeans", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowMeans", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowVars", "matter", function(x, na.rm = FALSE) {
	ret <- .Call("C_getRowVars", x, na.rm)
	names(ret) <- rownames(x)
	ret
})

setMethod("rowSds", "matter", function(x, na.rm = FALSE) {
	ret <- sqrt(.Call("C_getRowVars", x, na.rm))
	names(ret) <- rownames(x)
	ret
})

#### Define matter<vector> class for vector-like data ####
## --------------------------------------------------------

setClass("matter_vec",
	prototype = prototype(
		data = atoms(),
		datamode = make.datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = NULL,
		names = NULL,
		dimnames = NULL),
	contains = "matter",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@dim) )
			errors <- c(errors, "vector must have NULL 'dim'")
		if ( !is.null(object@dimnames) )
			errors <- c(errors, "vector must have NULL 'dimnames'")
		if ( is.null(errors) ) TRUE else errors
	})

matter_vec <- function(data, datamode = "double", paths = NULL,
					filemode = ifelse(is.null(paths), "rb+", "rb"),
					offset = 0, extent = length, length = 0, names = NULL, ...)
{
	if ( length == 0 && all(extent == 0) )
		return(new("matter_vec"))
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=length(extent))
	if ( is.null(paths) ) {
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
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_vec",
		data=atoms(
			source_id=as.integer(factor(paths)),
			datamode=as.integer(make.datamode(datamode, type="C")),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=widest.datamode(datamode, from="C"),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=as.numeric(sum(extent)),
		dim=NULL,
		names=names,
		dimnames=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_vec", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk binary vector", "\n", sep="")
	callNextMethod(object)
})

getVector <- function(x) {
	y <- .Call("C_getVector", x)
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	y
}

setVector <- function(x, value) {
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	value <- rep(value, length.out=length(x)) # should do this in C++ code
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setVector", x, value)
	if ( validObject(x) )
		invisible(x)
}

getVectorElements <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	y <- .Call("C_getVectorElements", x, i - 1)
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
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setVectorElements", x, i - 1, value)
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
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data@source_id <- as.integer(factor(x@paths[x@data@source_id],
		levels=paths))
	y@data@source_id <- as.integer(factor(y@paths[y@data@source_id],
		levels=paths))
	data <- combine(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest.datamode(data, from="C"),
		paths=paths,
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
		datamode = make.datamode("numeric", type="R"),
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
		datamode = make.datamode("numeric", type="R"),
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
			make.datamode(datamode, type="C"),
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
		datamode=widest.datamode(datamode, from="C"),
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
		if ( !all.indices(x, i, 1) )
			stop("cannot subset column-major matrix as S4 by row")
		subsetMatterCols(x, j)
	} else if ( is(x, "matter_matr") ) {
		if ( !all.indices(x, j, 2) )
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
		datamode=widest.datamode(data, from="C"),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=NULL,
		dimnames=combine.colnames(x,y))
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
		datamode=widest.datamode(data, from="C"),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=NULL,
		dimnames=combine.rownames(x,y))
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
	function(formula, data, ..., chunksize = NULL, fc = NULL) {
		bigglm.matter(formula, data, ...,
			chunksize=chunksize, fc=fc)
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

#### Linear regression for matter matrices ####
## -------------------------------------------

setMethod("prcomp", "matter_mat",
	function(x, n = 3, retx = TRUE, center = TRUE, scale. = FALSE, ...) {
		prcomp.matter(x, n=n, retx=retx, center=center, scale.=scale., ...)
})

# based on code for prcomp_irlba from package:irlba

prcomp.matter <- function(x, n, retx, center, scale., ...) {
    if ( "tol" %in% names(match.call(expand.dots=FALSE)$...) )
        warning("The 'tol' truncation argument from 'prcomp' is not supported\n",
        	"  for class 'matter_mat'. If specified, 'tol' is passed to 'irlba'\n",
        	"  to control that algorithm's convergence tolerance.")
    x <- scale(x, center=center, scale=scale.)
    sv <- irlba(x, nv=n, fastpath=FALSE, ...)
    ans <- list(sdev = sv$d/sqrt(max(1, nrow(x) - 1)), rotation = sv$v)
    colnames(ans$rotation) <- paste0("PC", seq(1, ncol(ans$rotation)))
    if ( !is.null(attr(x, "scaled:center")) ) {
    	ans$center <- attr(x, "scaled:center")
    } else {
    	ans$center <- FALSE
    }
    if ( !is.null(attr(x, "scaled:scale")) ) {
    	ans$scale <- attr(x, "scaled:scale")
    } else {
    	ans$scale <- FALSE
    }
    if ( retx ) {
        ans <- c(ans, list(x = sv$d * sv$u))
        colnames(ans$x) <- paste("PC", seq(1, ncol(ans$rotation)), sep = "")
    }
    class(ans) <- "prcomp"
    ans
}
