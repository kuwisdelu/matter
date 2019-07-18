
#### Define matter<str> class for string data ####
## -----------------------------------------------

setClass("matter_str",
	slots = c(
		data = "atoms",
		encoding = "character"),
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("character", type="R"),
		paths = character(),
		filemode = make_filemode("r"),
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
		if ( object@datamode != "character" )
			errors <- c(errors, "'datamode' must be 'character'")
		if ( is.null(errors) ) TRUE else errors
	})

matter_str <- function(data, datamode = "uchar", paths = NULL,
					filemode = ifelse(all(file.exists(paths)), "r", "rw"),
					offset = c(0, cumsum(sizeof("uchar") * extent)[-length(extent)]),
					extent = nchar, nchar = 0, names = NULL,
					encoding = "unknown", ...)
{
	if ( !missing(data) ) {
		if ( missing(nchar) ) {
			nchar <- nchar(data, type="bytes")
			nchar[is.na(nchar)] <- length(char2raw(NA_character_))
		}
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
				rep(make_datamode("character", type="C"),
					length.out=length(extent))),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=make_datamode("character", type="R"),
		paths=levels(factor(paths)),
		filemode=make_filemode(filemode),
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

setMethod("describe_for_display", "matter_str", function(x) "out-of-memory string")

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

# subsetting

getString <- function(x) {
	y <- .Call("C_getString", x, PACKAGE="matter")
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	Encoding(y) <- x@encoding
	y
}

getStringElements <- function(x, i, j, exact) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i, exact)
	y <- .Call("C_getStringElements", x, i - 1, PACKAGE="matter")
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	Encoding(y) <- x@encoding
	y	
}

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ..., drop) {
		if ( !missing(i) ) {
			getStringElements(x, i)
		} else {
			getString(x)
		}
	})

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) {
		y <- subList(x, i)
		y@encoding <- x@encoding
		if ( validObject(y) )
			y
	})

setReplaceMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing", value = "ANY"),
	function(x, i, ..., value) {
		if ( is.character(value) || is.factor(value) ) {
			value <- lapply(value, char2raw)
		} else {
			if ( !is.list(value) )
				value <- list(value)
			value <- lapply(value, as.raw)
		}
		if ( !missing(i) ) {
			for ( k in seq_along(i) )
				x <- setListElements(x, i[k], NULL, value[[k]])
			x
		} else {
			setList(x, value)
		}
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
		datamode=make_datamode("character", type="R"),
		paths=levels(factor(c(x@paths, y@paths))),
		filemode=common_filemode(x@filemode, y@filemode),
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


