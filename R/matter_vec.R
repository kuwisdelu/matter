
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
			data <- 0
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

#### Arithmetic and other operators ####
## --------------------------------------

check_comformable_lengths <- function(x, y, margin = 1) {
	if ( is.vector(x) ) {
		return(check_comformable_dims(y, x))
	} else if ( length(y) != 1 && length(x) != length(y) ) {
		warning("argument length unequal to vector length and will be recycled")
	}
	TRUE
}

# setMethod("+", c("matter_vec", "matter_vec"),
# 	function(e1, e2) {
# 		register_op(e1, NULL, e2, "+")
# })

setMethod("+", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e1, NULL, e2, "+")
})

setMethod("+", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e2, e1, NULL, "+")
})

setMethod("-", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e1, NULL, e2, "-")
})

setMethod("-", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e2, e1, NULL, "-")
})

setMethod("*", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e1, NULL, e2, "*")
})

setMethod("*", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e2, e1, NULL, "*")
})

setMethod("/", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e1, NULL, e2, "/")
})

setMethod("/", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e2, e1, NULL, "/")
})

setMethod("^", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e1, NULL, e2, "^")
})

setMethod("^", c("numeric", "matter_vec"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(e2, e1, NULL, "^")
})

setMethod("exp", "matter_vec",
	function(x) {
		if ( check_comformable_lengths(e1, e2) )
			register_op(x, NULL, NULL, "^")
})

setMethod("log", "matter_vec",
	function(x, base) {
		if ( missing(base) ) {
			register_op(x, NULL, NULL, "log")
		} else {
			if ( check_comformable_lengths(e1, e2) )
				register_op(x, base, NULL, "log")
		}
})

setMethod("log2", "matter_vec", function(x) log(x, base=2))

setMethod("log10", "matter_vec", function(x) log(x, base=10))

