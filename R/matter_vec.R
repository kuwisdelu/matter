
#### Define matter<vector> class for vector-like data ####
## --------------------------------------------------------

setClass("matter_vec",
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1e6L,
		length = 0,
		dim = NULL,
		names = NULL,
		dimnames = NULL,
		ops = NULL),
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
					filemode = ifelse(all(file.exists(paths)), "rb", "rb+"),
					offset = 0, extent = length, length = 0, names = NULL, ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(length) )
			length <- length(data)
	}
	if ( length == 0 && all(extent == 0) )
		return(new("matter_vec"))
	if ( length(offset) != length(extent) )
		stop("length of 'offset' [", length(offset), "] ",
			"must equal length of 'extent' [", length(extent), "]")
	if ( length(datamode) != length(extent) )
		datamode <- rep(datamode, length.out=length(extent))
	if ( is.null(paths) )
		paths <- tempfile(fileext=".bin")
	paths <- normalizePath(paths, mustWork=FALSE)
	if ( !file.exists(paths) ) {
		if ( missing(data) )
			data <- vector(as.character(widest_datamode(datamode)), length=1)
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	} else if ( !missing(data) ) {
		warning("file already exists")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_vec",
		data=atoms(
			group_id=rep.int(1L, length(extent)),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(make_datamode(datamode, type="C")),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=widest_datamode(datamode),
		paths=levels(factor(paths)),
		filemode=filemode,
		length=as.numeric(sum(extent)),
		dim=NULL,
		names=names,
		dimnames=NULL,
		ops=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("show", "matter_vec", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@length, " length> ",
		"on-disk vector", "\n", sep="")
	callNextMethod(object)
})

setAs("raw", "matter_vec",
	function(from) matter_vec(from, datamode="raw", names=names(from)))

setAs("logical", "matter_vec",
	function(from) matter_vec(from, datamode="logical", names=names(from)))

setAs("integer", "matter_vec",
	function(from) matter_vec(from, datamode="integer", names=names(from)))

setAs("numeric", "matter_vec",
	function(from) matter_vec(from, datamode="double", names=names(from)))

setAs("character", "matter_vec",
	function(from) matter_vec(as.numeric(from), datamode="double", names=names(from)))

setAs("factor", "matter_vec",
	function(from) matter_vec(as.integer(from), datamode="int", names=names(from)))

as.matter_vec <- function(x) as(x, "matter_vec")

getVector <- function(x) {
	y <- .Call("C_getArray", x, PACKAGE="matter")
	if ( !is.null(names(x)) )
		names(y) <- names(x)
	y
}

setVector <- function(x, value) {
	if ( length(x) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(x))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setArray", x, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getVectorElements <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	y <- .Call("C_getArrayElements", x, i - 1, PACKAGE="matter")
	if ( !is.null(names(x)) )
		names(y) <- names(x)[i]
	y	
}

setVectorElements <- function(x, i, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( length(i) %% length(value) != 0 )
		warning("number of items to replace is not ",
			"a multiple of replacement length")
	if ( length(value) != 1 )
		value <- rep(value, length.out=length(i))
	if ( is.logical(value) )
		value <- as.integer(value)
	if ( is.character(value) )
		value <- as.double(value)
	.Call("C_setArrayElements", x, i - 1, value, PACKAGE="matter")
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
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	paths <- levels(factor(c(x@paths, y@paths)))
	x@data@source_id <- as.integer(factor(x@paths[x@data@source_id[]],
		levels=paths))
	y@data@source_id <- as.integer(factor(y@paths[y@data@source_id[]],
		levels=paths))
	data <- combine(x@data, y@data)
	new(class(x),
		data=data,
		datamode=widest_datamode(datamode(data)),
		paths=paths,
		filemode=ifelse(all(c(x@filemode, y@filemode) == "rb+"), "rb+", "rb"),
		length=x@length + y@length,
		dim=NULL,
		names=NULL,
		dimnames=NULL,
		ops=NULL)
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
	x@data <- x@data
	x@dim <- c(1L, as.integer(x@length))
	if ( !is.null(x@names) )
		x@dimnames <- list(NULL, x@names)
	x@names <- NULL
	if ( validObject(x) )
		x
})

#### Delayed operations on 'matter_vec' ####
## ----------------------------------------

check_comformable_lengths <- function(x, y, margin = 1) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(x) != length(y) ) {
		warning("argument length unequal to array length and will be recycled")
	}
	TRUE
}

# Arith

setMethod("Arith", c("matter_vec", "matter_vec"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( length(e1) == length(e2) ) {
			register_op(e1, NULL, e2, .Generic)
		} else {
			stop("on-disk vector lengths must match exactly for delayed operation")
		}
})

setMethod("Arith", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1)[1] != "numeric" && typeof(e2) == "double" )
				datamode(e1) <- "numeric"
			e1
		}
})

setMethod("Arith", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( .Generic %in% c("%%", "%/%") )
			stop("unsupported delayed operation type")
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic)
			if ( datamode(e2)[1] != "numeric" && typeof(e1) == "double" )
				datamode(e2) <- "numeric"
			e2
		}
})

# Compare

setMethod("Compare", c("matter_vec", "matter_vec"),
	function(e1, e2) {
		if ( length(e1) == length(e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		} else {
			stop("on-disk vector lengths must match exactly for delayed operation")
		}
})

setMethod("Compare", c("matter_vec", "raw"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("raw", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic)
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

setMethod("Compare", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Compare", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e2 <- register_op(e2, e1, NULL, .Generic)
			if ( datamode(e2) != "logical" )
				datamode(e2) <- "logical"
			e2
		}
})

# Math

setMethod("exp", "matter_vec",
	function(x) {
		x <- register_op(x, NULL, NULL, "^")
		if ( datamode(x) != "numeric" )
			datamode(x) <- "numeric"
		x
})

setMethod("log", "matter_vec",
	function(x, base) {
		if ( missing(base) ) {
			x <- register_op(x, NULL, NULL, "log")
		} else if ( check_comformable_lengths(x, base) ) {
			x <- register_op(x, base, NULL, "log")
		}
		if ( datamode(x) != "numeric" )
			datamode(x) <- "numeric"
		x
})

setMethod("log2", "matter_vec", function(x) log(x, base=2))

setMethod("log10", "matter_vec", function(x) log(x, base=10))

