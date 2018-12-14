
#### Define matter<factor> class for categorical data ####
## --------------------------------------------------------

setClass("matter_fc",
	slots = c(levels = "character"),
	prototype = prototype(
		data = new("atoms"),
		datamode = make_datamode("integer", type="R"),
		paths = character(),
		filemode = make_filemode("r"),
		chunksize = 1e6L,
		length = 0,
		dim = NULL,
		names = NULL,
		dimnames = NULL,
		ops = NULL,
		levels = character()),
	contains = "matter_vec",
	validity = function(object) {
		errors <- NULL
		if ( object@datamode != "integer" )
			errors <- c(errors, "'datamode' must be 'integer'")
		if ( is.null(errors) ) TRUE else errors
	})

matter_fc <- function(data, datamode = "int", paths = NULL,
					filemode = ifelse(all(file.exists(paths)), "r", "rw"),
					offset = 0, extent = length, length = 0, names = NULL,
					levels = base::levels(as.factor(data)), ...)
{
	if ( !missing(data) ) {
		if ( missing(length) )
			length <- length(data)
		if ( !is.factor(data) )
			data <- as.factor(data)
	}
	if ( length == 0 && all(extent == 0) )
		return(new("matter_fc"))
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
			data <- NA_integer_
		filemode <- force(filemode)
		result <- file.create(paths)
		if ( !result )
			stop("error creating file")
	} else if ( !missing(data) && missing(filemode) ) {
		warning("file already exists")
	}
	if ( length(paths) != length(extent) )
		paths <- rep(paths, length.out=length(extent))
	x <- new("matter_fc",
		data=atoms(
			group_id=rep.int(1L, length(extent)),
			source_id=as.integer(factor(paths)),
			datamode=as.integer(make_datamode(datamode, type="C")),
			offset=as.numeric(offset),
			extent=as.numeric(extent)),
		datamode=make_datamode("integer", type="R"),
		paths=levels(factor(paths)),
		filemode=make_filemode(filemode),
		length=as.numeric(sum(extent)),
		dim=NULL,
		names=names,
		dimnames=NULL,
		ops=NULL,
		levels=levels, ...)
	if ( !missing(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter_fc", function(x) "factor")

setMethod("show", "matter_fc", function(object) {
	callNextMethod(object)
	cat("    levels:", paste_head(levels(object)), "\n")
})

setAs("raw", "matter_fc",
	function(from) matter_fc(as.factor(from), names=names(from)))

setAs("logical", "matter_fc",
	function(from) matter_fc(as.factor(from), names=names(from)))

setAs("integer", "matter_fc",
	function(from) matter_fc(as.factor(from), names=names(from)))

setAs("numeric", "matter_fc",
	function(from) matter_fc(as.factor(from), names=names(from)))

setAs("character", "matter_fc",
	function(from) matter_fc(as.factor(from), names=names(from)))

setAs("factor", "matter_fc",
	function(from) matter_fc(from, names=names(from)))

as.matter_fc <- function(x) as(x, "matter_fc")

setMethod("levels", "matter_fc", function(x) x@levels)

setReplaceMethod("levels", "matter_fc",
	function(x, value) {
		x@levels <- value
		x
	})

setMethod("[",
	c(x = "matter_fc", i = "ANY", j = "missing", drop = "ANY"),
	function(x, i, ...) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			y <- getVectorElements(x, i)
		} else {
			y <- getVector(x)
		}
		factor(levels(x)[y], levels=levels(x))
	})

setMethod("[",
	c(x = "matter_fc", i = "ANY", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !missing(i) ) {
			y <- subVector(x, i)
			levels(y) <- levels(x)
		}
		y
	})

setReplaceMethod("[",
	c(x = "matter_fc", i = "ANY", j = "missing", value = "ANY"),
	function(x, i, ..., value) {
		if ( length(list(...)) > 0 )
			stop("incorrect number of dimensions")
		if ( !is.character(value) )
			value <- as.character(value)
		value <- match(value, levels(x))
		if ( any(is.na(value)) )
			warning("invalid factor level, NA generated")
		if ( !missing(i) ) {
			setVectorElements(x, i, value)
		} else {
			setVector(x, value)
		}
	})

setMethod("combine", "matter_fc", function(x, y, ...) {
	class(x) <- "matter_vec"
	class(y) <- "matter_vec"
	combine(x, y, ...)
})

#### Delayed operations on 'matter_fc' ####
## ----------------------------------------

# Arith

setMethod("Arith", c("matter_fc", "matter_fc"),
	function(e1, e2) {
		stop(paste0("`", .Generic, "` not meaningful for factors"))
})

setMethod("Arith", c("matter_fc", "numeric"),
	function(e1, e2) {
		stop(paste0("`", .Generic, "` not meaningful for factors"))
})

setMethod("Arith", c("numeric", "matter_fc"),
	function(e1, e2) {
		stop(paste0("`", .Generic, "` not meaningful for factors"))
})

# Compare

setMethod("Compare", c("matter_fc", "matter_fc"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		if ( all(levels(e1) == levels(e2)) ) {
			class(e1) <- "matter_vec"
			class(e2) <- "matter_vec"
			callGeneric(e1, e2)
		} else {
			stop("levels of factors are different")
		}
})

setMethod("Compare", c("matter_fc", "numeric"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		e2 <- as.character(e2)
		if ( all(e2 %in% levels(e1)) ) {
			e2 <- factor(e2, levels=levels(e1))
			class(e1) <- "matter_vec"
			callGeneric(e1, c(e2))
		} else {
			class(e1) <- "matter_vec"
			e1 == 0 # all will be false
		}
})

setMethod("Compare", c("numeric", "matter_fc"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		e1 <- as.character(e1)
		if ( all(e1 %in% levels(e2)) ) {
			e1 <- factor(e1, levels=levels(e2))
			class(e2) <- "matter_vec"
			callGeneric(c(e1), e2)
		} else {
			class(e1) <- "matter_vec"
			e1 == 0 # all will be false
		}
})

setMethod("Compare", c("matter_fc", "character"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		if ( all(e2 %in% levels(e1)) ) {
			e2 <- factor(e2, levels=levels(e1))
			class(e1) <- "matter_vec"
			callGeneric(e1, c(e2))
		} else {
			class(e1) <- "matter_vec"
			e1 == 0 # all will be false
		}
})

setMethod("Compare", c("character", "matter_fc"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		if ( all(e1 %in% levels(e2)) ) {
			e1 <- factor(e1, levels=levels(e2))
			class(e2) <- "matter_vec"
			callGeneric(c(e1), e2)
		} else {
			class(e1) <- "matter_vec"
			e1 == 0 # all will be false
		}
})

setMethod("Compare", c("matter_fc", "factor"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		if ( all(levels(e1) == levels(e2)) ) {
			class(e1) <- "matter_vec"
			callGeneric(e1, c(e2))
		} else {
			stop("levels of factors are different")
		}
})

setMethod("Compare", c("factor", "matter_fc"),
	function(e1, e2) {
		if ( !(.Generic %in% c("==", "!=")) )
			stop(paste0("`", .Generic, "` not meaningful for factors"))
		if ( all(levels(e1) == levels(e2)) ) {
			class(e2) <- "matter_vec"
			callGeneric(c(e1), e2)
		} else {
			stop("levels of factors are different")
		}
})

# Math

setMethod("exp", "matter_fc",
	function(x) {
		stop("`exp` not meaningful for factors")
})

setMethod("log", "matter_fc",
	function(x, base) {
		stop("`log` not meaningful for factors")
})

setMethod("log2", "matter_fc", function(x) log(x, base=2))

setMethod("log10", "matter_fc", function(x) log(x, base=10))

