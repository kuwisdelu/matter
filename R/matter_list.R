
#### Define matter<list> class for homogenous list data ####
## ---------------------------------------------------------

setClass("matter_list",
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
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
		if ( object@length != length(object@dim) )
			errors <- c(errors, paste0("length of object [", object@length,
				"] does not match length of dims [", length(object@dim), "]"))
		if ( is.null(errors) ) TRUE else errors
	})

matter_list <- function(data, datamode = "double", paths = NULL,
					filemode = ifelse(is.null(paths), "rb+", "rb"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = dim, dim = 0, names = NULL, dimnames = NULL, ...)
{
	if ( !missing(data) ) {
		if ( !is.list(data) )
			stop("data is not a list")
		if ( missing(datamode) )
			datamode <- typeof(data[[1]])
		if ( missing(dim) )
			dim <- sapply(data, length)
	}
	if ( all(extent == 0) )
		return(new("matter_list"))
	if ( missing(data) ) {
		missingdata <- TRUE
	} else {
		missingdata <- FALSE
	}
	if ( !missingdata && !is.list(data) )
		data <- list(data)
	if ( !missingdata && any(sapply(data, length) != extent) )
		stop("length of data elements must equal extents")
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=length(extent))
	if ( is.null(paths) ) {
		if ( missingdata )
			data <- rep(list(0), length(extent))
		filemode <- force(filemode)
		paths <- tempfile(fileext=".bin")
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	}
	paths <- normalizePath(paths)
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_list",
		data=atoms(
			group_id=seq_along(extent),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(make_datamode(datamode, type="C")),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=widest_datamode(datamode),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=length(extent),
		dim=as.integer(extent),
		names=names,
		dimnames=dimnames,
		ops=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_list", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk list", "\n", sep="")
	callNextMethod(object)
})

setAs("list", "matter_list", function(from) matter_list(from))

as.matter_list <- function(x) as(x, "matter_list")

getList <- function(x) {
	y <- .Call("C_getList", x, PACKAGE="matter")
	if ( !is.null(dimnames(x)) )
		y <- mapply(setNames, y, dimnames(x))
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	y
}

setList <- function(x, value) {
	for ( i in seq_along(value) ) {
		if ( dim(x)[i] %% length(value[[i]]) != 0 )
			warning("number of items to replace is not ",
				"a multiple of replacement length")
		if ( length(value[[i]]) != 1 )
			value[[i]] <- rep(value[[i]], length.out=dim(x)[i])
	}
	for ( i in seq_along(value) ) {
		if ( is.character(value[[i]]) )
			value[[i]] <- as.raw(value[[i]])
	}
	for ( i in seq_along(value) ) {
		if ( is.logical(value[[i]]) )
			value[[i]] <- as.integer(value[[i]])
	}
	.Call("C_setList", x, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getListElements <- function(x, i, j, drop) {
	if ( length(i) != 1 )
		stop("attempt to select more than one element in list")
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( missing(j) || is.null(j) ) {
		y <- .Call("C_getListElements", x, i - 1, NULL, PACKAGE="matter")
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, i)
		if ( is.character(j) )
			j <- dimnames2index(x, j, i)
		y <- .Call("C_getListElements", x, i - 1, j - 1, PACKAGE="matter")
	}
	if ( !is.null(dimnames(x)) )
		y <- mapply(setNames, y, dimnames(x)[i])
	if ( !is.null(names(x)) )
		names(y) <- names(x)[i]
	y	
}

setListElements <- function(x, i, j, value) {
	if ( length(i) != 1 )
		stop("attempt to select more than one element in list")
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( missing(j) || is.null(j) ) {
		if ( dim(x)[i] %% length(value) != 0 )
			warning("number of items to replace is not ",
				"a multiple of replacement length")
		if ( length(value) != 1 )
			value <- rep(value, length.out=dim(x)[i])
		if ( is.logical(value) )
			value <- as.integer(value)
		if ( is.character(value) )
			value <- as.double(value)
		.Call("C_setListElements", x, i - 1, NULL, value, PACKAGE="matter")
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, i)
		if ( is.character(j) )
			j <- dimnames2index(x, j, i)
		if ( length(j) %% length(value) != 0 )
			warning("number of items to replace is not ",
				"a multiple of replacement length")
		if ( length(value) != 1 )
			value <- rep(value, length.out=length(j))
		if ( is.logical(value) )
			value <- as.integer(value)
		if ( is.character(value) )
			value <- as.double(value)
		.Call("C_setListElements", x, i - 1, j - 1, value, PACKAGE="matter")
	}
	if ( validObject(x) )
		invisible(x)	
}

setMethod("[",
	c(x = "matter_list", i = "missing", j = "missing"),
	function(x, ...) getList(x))

setMethod("[",
	c(x = "matter_list", i = "ANY", j = "missing"),
	function(x, i, ...) {
		if ( is.logical(i) )
			i <- logical2index(x, i)
		if ( is.character(i) )
			i <- names2index(x, i)
		new(class(x),
			data=atomdata(x)[i],
			datamode=datamode(x),
			paths=paths(x),
			filemode=filemode(x),
			length=length(i),
			dim=dim(x)[i],
			names=names(x)[i],
			dimnames=dimnames(x)[i],
			ops=NULL)
})

setReplaceMethod("[",
	c(x = "matter_list", i = "missing", j = "missing"),
	function(x, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setList(x, value)
})

setReplaceMethod("[",
	c(x = "matter_list", i = "ANY", j = "missing"),
	function(x, i, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		for ( k in seq_along(i) )
			x <- setListElements(x, i[k], NULL, value[[k]])
		x
})

setMethod("[",
	c(x = "matter_list", i = "ANY", j = "ANY"),
	function(x, i, j, ...) getListElements(x, i, j))

setReplaceMethod("[",
	c(x = "matter_list", i = "ANY", j = "ANY"),
	function(x, i, j, ..., value) setListElements(x, i, j, value))

setMethod("[[",
	c(x = "matter_list", i = "ANY", j = "missing"),
	function(x, i, ...) getListElements(x, i))

setReplaceMethod("[[",
	c(x = "matter_list", i = "ANY", j = "missing"),
	function(x, i, ..., value) setListElements(x, i, NULL, value))


