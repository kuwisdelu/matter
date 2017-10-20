
#### Define matter<str> class for string data ####
## -----------------------------------------------

setClass("matter_char",
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

matter_char <- function(data, datamode = "raw", paths = NULL,
					filemode = ifelse(all(file.exists(paths)), "rb", "rb+"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = nchar, nchar = 0, names = NULL, ...)
{
	if ( !missing(data) ) {
		if ( missing(nchar) )
			nchar <- nchar(data, type="bytes")
		if ( !is.character(data) )
			data <- as.character(data)
	}
	if ( all(extent == 0) )
		return(new("matter_char"))
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( is.null(paths) )
		paths <- tempfile(fileext=".bin")
	paths <- normalizePath(paths, mustWork=FALSE)
	if ( !file.exists(paths) ) {
		if ( missing(data) )
			data <- rep(list(" "), length(extent))
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_char",
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
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_char", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk strings", "\n", sep="")
	callNextMethod(object)
})

setAs("character", "matter_char", function(from) matter_char(from))

setAs("factor", "matter_char",
	function(from) as(as.character(from), "matter_char"))

as.matter_char <- function(x) as(x, "matter_char")

setMethod("[",
	c(x = "matter_char", i = "missing", j = "missing"),
	function(x, ...) {
		sapply(getList(x), rawToChar)
})

setReplaceMethod("[",
	c(x = "matter_char", i = "missing", j = "missing"),
	function(x, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, charToRaw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		setList(x, value)
})

setMethod("[",
	c(x = "matter_char", i = "ANY", j = "missing"),
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
	c(x = "matter_char", i = "ANY", j = "missing"),
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
	c(x = "matter_char", i = "ANY", j = "ANY"),
	function(x, i, j, ...) {
		y <- getListElements(x, i, j)
		sapply(y, rawToChar)
})

setReplaceMethod("[",
	c(x = "matter_char", i = "ANY", j = "ANY"),
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
	c(x = "matter_char", i = "ANY", j = "missing"),
	function(x, i, ...) {
		y <- getListElements(x, i)
		rawToChar(y)
})

setReplaceMethod("[[",
	c(x = "matter_char", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( is.character(value) ) {
			value <- charToRaw(value)
		} else {
			value <- as.raw(value)
		}
		setListElements(x, i, NULL, value)
})

setMethod("lengths", "matter_char", function(x, use.names = TRUE) {
	if ( use.names ) {
		setNames(x@dim, x@names)
	} else {
		setNames(x@dim, NULL)
	}
})


