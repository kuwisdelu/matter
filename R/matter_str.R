
#### Define matter<str> class for string data ####
## -----------------------------------------------

setClass("matter_str",
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("raw", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = 0,
		names = NULL,
		dimnames = NULL,
		ops = NULL),
	contains = "matter",
	validity = function(object) {
		errors <- NULL
		if ( object@datamode != "raw" )
			errors <- c(errors, "'datamode' must be 'raw'")
		if ( is.null(errors) ) TRUE else errors
	})

matter_str <- function(data, datamode = "raw", paths = NULL,
					filemode = ifelse(is.null(paths), "rb+", "rb"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = dim, dim = 0, names = NULL, ...)
{
	if ( !missing(data) ) {
		if ( is.character(data) )
			stop("data is not a string")
		if ( missing(dim) )
			dim <- nchar(data, type="bytes")
	}
	if ( all(extent == 0) )
		return(new("matter_str"))
	if ( missing(data) ) {
		missingdata <- TRUE
	} else {
		missingdata <- FALSE
	}
	if ( !missingdata && !is.character(data) )
		data <- as.character(data)
	if ( !missingdata && any(sapply(data, nchar, type="bytes") != extent) )
		stop("length of character strings (in bytes) must equal extents")
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( is.null(paths) ) {
		if ( missingdata )
			data <- sapply(extent, raw)
		filemode <- force(filemode)
		paths <- tempfile(fileext=".bin")
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	}
	paths <- normalizePath(paths)
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_str",
		data=atoms(
			group_id=seq_along(extent),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(rep(make_datamode("raw", type="C"),
				length.out=length(extent))),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=make_datamode(datamode[1], type="R"),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=length(extent),
		dim=as.integer(extent),
		names=names,
		dimnames=NULL,
		ops=NULL, ...)
	if ( !missingdata )
		x[] <- data
	x
}

setMethod("show", "matter_str", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk strings", "\n", sep="")
	callNextMethod(object)
})

setAs("character", "matter_str", function(from) matter_str(from))

setAs("factor", "matter_str",
	function(from) as(as.character(from), "matter_str"))

as.matter_str <- function(x) as(x, "matter_str")

setMethod("[",
	c(x = "matter_str", i = "missing", j = "missing"),
	function(x, ...) {
		sapply(getList(x), rawToChar)
})

setReplaceMethod("[",
	c(x = "matter_str", i = "missing", j = "missing"),
	function(x, ..., value) {
		value <- lapply(value, charToRaw)
		setList(x, value)
})

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ...) {
		if ( is.logical(i) )
			i <- logical2index(x, i)
		if ( is.character(i) )
			i <- names2index(x, i)
		y <- new(class(x),
			data=atomdata(x)[i],
			datamode=datamode(x),
			paths=paths(x),
			filemode=filemode(x),
			length=length(i),
			dim=dim(x)[i],
			names=names(x)[i],
			dimnames=dimnames(x)[i],
			ops=NULL)
		y[]
})

setReplaceMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, charToRaw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		for ( k in seq_along(i) )
			x <- setListElements(x, i[k], NULL, value[[k]])
		x
})

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY"),
	function(x, i, j, ...) {
		y <- getListElements(x, i, j)
		sapply(y, rawToChar)
})

setReplaceMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY"),
	function(x, i, j, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, charToRaw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		setListElements(x, i, j, value)
})

setMethod("[[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ...) {
		y <- getListElements(x, i)
		rawToChar(y)
})

setReplaceMethod("[[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( is.character(value) ) {
			value <- charToRaw(value)
		} else {
			value <- as.raw(value)
		}
		setListElements(x, i, NULL, value)
})


