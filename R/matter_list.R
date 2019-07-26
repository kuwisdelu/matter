
#### Define matter<list> class for list data ####
## ----------------------------------------------

setClass("matter_list",
	slots = c(data = "atoms"),
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = make_filemode("r"),
		chunksize = 1e6L,
		length = 0,
		dim = 0L,
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
					filemode = ifelse(all(file.exists(paths)), "r", "rw"),
					offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
					extent = lengths, lengths = 0, names = NULL, dimnames = NULL, ...)
{
	if ( !missing(data) ) {
		if ( !is.list(data) )
			data <- list(data)
		if ( missing(datamode) )
			datamode <- sapply(data, class)
		if ( missing(lengths) )
			lengths <- sapply(data, length)
		if ( !all(sapply(data, is.atomic)) )
			stop("all list elements must be atomic vectors")
	}
	if ( all(extent == 0) )
		return(new("matter_list"))
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=length(extent))
	R_datamode <- make_datamode(datamode, type="R")
	C_datamode <- make_datamode(datamode, type="C")
	if ( is.null(paths) )
		paths <- tempfile(fileext=".bin")
	paths <- normalizePath(paths, mustWork=FALSE)
	if ( !file.exists(paths) ) {
		if ( missing(data) )
			data <- lapply(as.character(R_datamode), vector, length=1)
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	} else if ( !missing(data) && missing(filemode) ) {
		warning("file already exists")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_list",
		data=atoms(
			group_id=seq_along(extent),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(C_datamode),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=R_datamode,
		paths=levels(factor(paths)),
		filemode=make_filemode(filemode),
		length=length(extent),
		dim=as.integer(extent),
		names=names,
		dimnames=dimnames,
		ops=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

struct <- function(..., filename = NULL, filemode = "rw", offset = 0) {
	args <- list(...)
	if ( any(lengths(args) != 1) )
		stop("all arguments must be length 1")
	if ( any(sapply(args, function(a) is.null(names(a)))) )
		stop("all arguments must be a named scalar")
	if ( !is.null(filename) && length(filename) != 1 )
		stop("'filename' must be length 1")
	names <- names(args)
	types <- sapply(args, names, USE.NAMES=FALSE)
	lens <- as.integer(unlist(args))
	offset <- offset + c(0, cumsum(sizeof(types) * lens)[-length(lens)])
	matter_list(paths=filename, datamode=types, offset=offset,
		lengths=lens, names=names, filemode=filemode)
}

setMethod("describe_for_display", "matter_list", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("out-of-memory list")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_list", function(x) preview_list(x))

setAs("list", "matter_list", function(from) matter_list(from, names=names(from)))

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
	if ( length(value) != length(x) )
		stop("length of replacement is not equal to object length")
	for ( i in seq_along(value) ) {
		if ( dim(x)[i] %% length(value[[i]]) != 0 )
			warning("number of items to replace is not ",
				"a multiple of replacement length")
		if ( length(value[[i]]) != 1 )
			value[[i]] <- rep(value[[i]], length.out=dim(x)[i])
	}
	.Call("C_setList", x, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getListElements <- function(x, i, j, exact) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i, exact)
	if ( length(i) != 1 )
		stop("attempt to select more than one element in list")
	if ( any(is.na(i)) )
		return(NULL)
	if ( missing(j) || is.null(j) ) {
		y <- .Call("C_getListElements", x, i - 1, NULL, PACKAGE="matter")
		if ( !is.null(dimnames(x)) )
			y <- setNames(y, dimnames(x)[i])
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, i)
		if ( is.character(j) )
			j <- dimnames2index(x, j, i)
		y <- .Call("C_getListElements", x, i - 1, j - 1, PACKAGE="matter")
		if ( !is.null(dimnames(x)) )
			y <- setNames(y, dimnames(x)[i][j])
	}
	y	
}

setListElements <- function(x, i, j, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( length(i) != 1 )
		stop("attempt to select more than one element in list")
	if ( missing(j) || is.null(j) ) {
		if ( dim(x)[i] %% length(value) != 0 )
			warning("number of items to replace is not ",
				"a multiple of replacement length")
		if ( length(value) != 1 )
			value <- rep(value, length.out=dim(x)[i])
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
		.Call("C_setListElements", x, i - 1, j - 1, value, PACKAGE="matter")
	}
	if ( validObject(x) )
		invisible(x)	
}

subList <- function(x, i, exact) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i, exact)
	new(class(x),
		data=atomdata(x)[,i],
		datamode=datamode(x)[i],
		paths=paths(x),
		filemode=filemode(x),
		length=length(i),
		dim=dim(x)[i],
		names=names(x)[i],
		dimnames=dimnames(x)[i],
		ops=NULL)
}

subListElements <- function(x, i, j, exact) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i, exact)
	if ( length(i) != 1 )
		stop("attempt to select more than one element in list")
	if ( missing(j) || is.null(j) ) {
		subList(x, i)
	} else {
		if ( is.logical(j) )
			j <- logical2index(x, j, i)
		if ( is.character(j) )
			j <- dimnames2index(x, j, i)
		y <- subList(x, i)
		new(class(y),
			data=subset_atoms_by_index_offset(y@data, j),
			datamode=datamode(y),
			paths=paths(y),
			filemode=filemode(y),
			length=1,
			dim=length(j),
			names=names(y),
			dimnames=if ( is.null(dimnames(y)) ) NULL
				else list(dimnames(y)[[1]][j]),
			ops=NULL)
	}	
}

subListElementAsVector <- function(x, i, exact) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i, exact)
	new("matter_vec",
		data=atomdata(x)[,i],
		datamode=datamode(x)[i],
		paths=paths(x),
		filemode=filemode(x),
		length=dim(x)[i],
		dim=NULL,
		names=dimnames(x)[i],
		dimnames=NULL,
		ops=NULL)
}

# subsetting

setMethod("[",
	c(x = "matter_list", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ..., drop) {
		if ( !missing(i) ) {
			getList(subList(x, i))
		} else {
			getList(x)
		}
	})

setMethod("[",
	c(x = "matter_list", i = "ANY", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subList(x, i))

setReplaceMethod("[",
	c(x = "matter_list", i = "ANY", j = "missing", value = "ANY"),
	function(x, i, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		if ( !missing(i) ) {
			for ( k in seq_along(i) )
				x <- setListElements(x, i[k], NULL, value[[k]])
			x
		} else {
			setList(x, value)
		}
	})

setMethod("[[",
	c(x = "matter_list", i = "ANY", j = "ANY"),
	function(x, i, j, ...) getListElements(x, i, j))

setReplaceMethod("[[",
	c(x = "matter_list", i = "ANY"),
	function(x, i, ..., value) setListElements(x, i, NULL, value))

setMethod("$",
	c(x = "matter_list"),
	function(x, name) getListElements(x, name, exact=FALSE))

setReplaceMethod("$",
	c(x = "matter_list"),
	function(x, name, value) setListElements(x, name, NULL, value))

# additional methods

setMethod("combine", "matter_list", function(x, y, ...) {
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
		datamode=make_datamode(c(x@datamode, y@datamode), type="R"),
		paths=levels(factor(c(x@paths, y@paths))),
		filemode=common_filemode(x@filemode, y@filemode),
		length=x@length + y@length,
		dim=c(x@dim, y@dim),
		names=names,
		dimnames=NULL,
		ops=NULL)
})

setMethod("lengths", "matter_list", function(x, use.names = TRUE) {
	if ( use.names ) {
		setNames(x@dim, x@names)
	} else {
		setNames(x@dim, NULL)
	}
})


