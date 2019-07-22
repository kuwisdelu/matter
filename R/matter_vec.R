
#### Define matter<vector> class for vector-like data ####
## --------------------------------------------------------

setClass("matter_vec",
	slot = c(data= "atoms"),
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("numeric", type="R"),
		paths = character(),
		filemode = make_filemode("r"),
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
					filemode = ifelse(all(file.exists(paths)), "r", "rw"),
					offset = 0, extent = length, length = 0, names = NULL, ...)
{
	if ( !missing(data) ) {
		if ( missing(datamode) )
			datamode <- typeof(data)
		if ( missing(length) )
			length <- length(data)
	} else if ( missing(length) && length(datamode) == 1L ) {
		length <- floor(sum(file.size(paths)) / sizeof(datamode))
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
	if ( !all(file.exists(paths)) ) {
		if ( missing(data) )
			data <- vector(as.character(widest_datamode(datamode)), length=1)
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	} else if ( !missing(data) && missing(filemode) ) {
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
		filemode=make_filemode(filemode),
		length=as.numeric(sum(extent)),
		dim=NULL,
		names=names,
		dimnames=NULL,
		ops=NULL, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter_vec", function(x) {
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("out-of-memory ", x@datamode, " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_vec", function(x) preview_vector(x))

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

setReplaceMethod("dim", "matter_vec", function(x, value) {
	if ( is.null(value) ) {
		x
	} else {
		callNextMethod(as(x, "matter_arr"), value)
	}
})

getVector <- function(x) {
	y <- .Call("C_getVector", x, PACKAGE="matter")
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
	.Call("C_setVector", x, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)
}

getVectorElements <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	y <- .Call("C_getVectorElements", x, i - 1, PACKAGE="matter")
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
	.Call("C_setVectorElements", x, i - 1, value, PACKAGE="matter")
	if ( validObject(x) )
		invisible(x)	
}

subVector <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	if ( is.character(i) )
		i <- names2index(x, i)
	if ( !is.null(x@ops) )
		warning("dropping delayed operations")
	new(class(x),
		data=subset_atoms_by_index_offset(x@data, i),
		datamode=x@datamode,
		paths=x@paths,
		chunksize=x@chunksize,
		length=as.numeric(length(i)),
		dim=NULL,
		names=if ( !is.null(x@names) ) x@names[i] else NULL,
		dimnames=NULL,
		ops=NULL)
}

setMethod("[",
	c(x = "matter_vec", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ..., drop) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			getVectorElements(x, i)
		} else {
			getVector(x)
		}
	})

setMethod("[",
	c(x = "matter_vec", i = "ANY", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			subVector(x, i)
		} else {
			x
		}
	})

setReplaceMethod("[",
	c(x = "matter_vec", i = "ANY", j = "missing", value = "ANY"),
	function(x, i, ..., value) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			setVectorElements(x, i, value)
		} else {
			setVector(x, value)
		}
	})

setMethod("combine", "matter_vec", function(x, y, ...) {
	if ( !is.null(x@ops) || !is.null(y@ops) )
		warning("dropping delayed operations")
	data <- combine_atoms(x@data, y@data,
		x.paths=x@paths, y.paths=y@paths, new.groups=FALSE)
	if ( is.null(names(x)) && is.null(names(y)) ) {
		names <- NULL
	} else {
		if ( is.null(names(x)) ) names(x) <- character(length(x))
		if ( is.null(names(y)) ) names(y) <- character(length(y))
		names <- c(names(x), names(y))
	}
	new(class(x),
		data=data,
		datamode=widest_datamode(datamode(data)),
		paths=levels(factor(c(x@paths, y@paths))),
		filemode=common_filemode(x@filemode, y@filemode),
		length=x@length + y@length,
		dim=NULL,
		names=names,
		dimnames=NULL,
		ops=NULL)
})

setMethod("t", "matter_vec", function(x) t(as(x, "matter_mat")))

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
		if ( length(e1) == length(e2) ) {
			register_op(e1, NULL, e2, .Generic)
		} else {
			stop("vector lengths must match exactly for delayed operation")
		}
})

setMethod("Arith", c("matter_vec", "numeric"),
	function(e1, e2) {
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1)[1] != "numeric" && typeof(e2) == "double" )
				datamode(e1) <- "numeric"
			e1
		}
})

setMethod("Arith", c("numeric", "matter_vec"),
	function(e1, e2) {
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
			stop("vector lengths must match exactly for delayed operation")
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

# Logic

setMethod("Logic", c("matter_vec", "matter_vec"),
	function(e1, e2) {
		if ( datamode(e1) != "logical" || datamode(e2) != "logical" )
			warning("datamode is not logical")
		if ( length(e1) == length(e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		} else {
			stop("vector lengths must match exactly for delayed operation")
		}
})

setMethod("Logic", c("matter_vec", "logical"),
	function(e1, e2) {
		if ( datamode(e1) != "logical" )
			warning("datamode is not logical")
		if ( check_comformable_lengths(e1, e2) ) {
			e1 <- register_op(e1, NULL, e2, .Generic)
			if ( datamode(e1) != "logical" )
				datamode(e1) <- "logical"
			e1
		}
})

setMethod("Logic", c("logical", "matter_vec"),
	function(e1, e2) {
		if ( datamode(e2) != "logical" )
			warning("datamode is not logical")
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

