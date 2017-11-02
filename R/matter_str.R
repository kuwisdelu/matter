
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
	} else if ( !missing(data) ) {
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

setMethod("show", "matter_str", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk strings", "\n", sep="")
	callNextMethod(object)
	cat("    encoding:", object@encoding, "\n")
})

setAs("character", "matter_str", function(from) matter_str(from, names=names(from)))

setAs("factor", "matter_str",
	function(from) as(as.character(from), "matter_str"))

as.matter_str <- function(x) as(x, "matter_str")

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

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ...) {
		if ( is.logical(i) )
			i <- logical2index(x, i)
		if ( is.character(i) )
			i <- names2index(x, i)
		y <- new(class(x),
			data=atomdata(x)[,i],
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

setMethod("[",
	c(x = "matter_str", i = "ANY", j = "ANY"),
	function(x, i, j, ...) {
		y <- getListElements(x, i, j)
		sapply(y, raw2char, encoding=x@encoding)
})

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

setMethod("[[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ...) {
		y <- getListElements(x, i)
		raw2char(y, encoding=x@encoding)
})

setReplaceMethod("[[",
	c(x = "matter_str", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( is.character(value) ) {
			value <- char2raw(value)
		} else {
			value <- as.raw(value)
		}
		setListElements(x, i, NULL, value)
})

setMethod("combine", "matter_str", function(x, y, ...) {
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data@source_id <- as.integer(factor(x@paths[x@data@source_id[]],
		levels=paths))
	y@data@source_id <- as.integer(factor(y@paths[y@data@source_id[]],
		levels=paths))
	y@data@group_id <- y@data@group_id[] + max(x@data@group_id[])
	data <- combine(x@data, y@data)
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
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
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


