
#### Define matter<str> class for string data ####
## -----------------------------------------------

setClass("matter_str",
	slots = c(
		data = "atoms",
		encoding = "character"),
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
		ops = NULL,
		encoding = "unknown"),
	contains = "matter",
	validity = function(object) {
		errors <- NULL
		if ( object@datamode != "raw" )
			errors <- c(errors, "'datamode' must be 'raw'")
		if ( is.null(errors) ) TRUE else errors
	})

matter_str <- function(data, datamode = "uchar", paths = NULL,
					filemode = ifelse(all(file.exists(paths)), "rb", "rb+"),
					offset = c(0, cumsum(sizeof("uchar") * extent)[-length(extent)]),
					extent = nchar, nchar = 0, names = NULL,
					encoding = "unknown", ...)
{
	if ( !missing(data) ) {
		if ( missing(nchar) )
			nchar <- nchar(data, type="bytes")
		if ( !is.character(data) )
			data <- as.character(data)
		if ( missing(encoding) ) {
			encoding <- unique(Encoding(data))
			if ( length(encoding) != 1 )
				encoding <- "unknown"
		}
	}
	if ( all(extent == 0) )
		return(new("matter_str"))
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
	} else if ( !missing(data) && missing(filemode) ) {
		warning("file already exists")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_str",
		data=atoms(
			group_id=seq_along(extent),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(
				rep(make_datamode("raw", type="C"),
					length.out=length(extent))),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=make_datamode("raw", type="R"),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=length(extent),
		dim=as.integer(extent),
		names=names,
		dimnames=NULL,
		ops=NULL,
		encoding=encoding, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter_str", function(x) "string")

setMethod("show", "matter_str", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		describe_for_display(object), "\n", sep="")
	callNextMethod(object)
	cat("    encoding:", object@encoding, "\n")
})

setAs("character", "matter_str", function(from) matter_str(from, names=names(from)))

setAs("factor", "matter_str",
	function(from) as(as.character(from), "matter_str"))

as.matter_str <- function(x) as(x, "matter_str")

setAs("matter_str", "vector", function(from) from[])

setMethod("as.vector", "matter_str", function(x) as(x, "vector"))

setMethod("Encoding", "matter_str", function(x) x@encoding)

setReplaceMethod("Encoding", "matter_str",
	function(x, value) {
		x@encoding <- value
		x
	})

# x[] subsetting

setMethod("[",
	c(x = "matter_str", i = "missing", j = "missing"),
	function(x, ...) {
		sapply(getList(x), raw2char, encoding=x@encoding)
})

setReplaceMethod("[",
	c(x = "matter_str", i = "missing", j = "missing"),
	function(x, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, char2raw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		setList(x, value)
})

# x[i] subsetting

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ...) subList(x, i)[])

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subList(x, i))

setReplaceMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, char2raw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		for ( k in seq_along(i) )
			x <- setListElements(x, i[k], NULL, value[[k]])
		x
})

# x[i,j] subsetting

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY"),
	function(x, i, j, ...) {
		y <- getListElements(x, i, j)
		sapply(y, raw2char, encoding=x@encoding)
})

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY", drop = "NULL"),
	function(x, i, j, ..., drop) subListElements(x, i, j))

setReplaceMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY"),
	function(x, i, j, ..., value) {
		if ( is.character(value) ) {
			value <- lapply(value, char2raw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		setListElements(x, i, j, value)
})

# additional methods

setMethod("combine", "matter_str", function(x, y, ...) {
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	data <- combine_atoms(x@data, y@data,
		x.paths=x@paths, y.paths=y@paths, new.groups=TRUE)
	if ( is.null(names(x)) && is.null(names(y)) ) {
		names <- NULL
	} else {
		if ( is.null(names(x)) ) names(x) <- character(length(x))
		if ( is.null(names(y)) ) names(y) <- character(length(y))
		names <- c(names(x), names(y))
	}
	new(class(x),
		data=data,
		datamode=make_datamode("raw", type="R"),
		paths=levels(factor(c(x@paths, y@paths))),
		filemode=if ( readonly(x) || readonly(y) ) "rb" else "rb+",
		length=x@length + y@length,
		dim=c(x@dim, y@dim),
		names=names,
		dimnames=NULL,
		ops=NULL)
})

setMethod("lengths", "matter_str", function(x, use.names = TRUE) {
	if ( use.names ) {
		setNames(x@dim, x@names)
	} else {
		setNames(x@dim, NULL)
	}
})


