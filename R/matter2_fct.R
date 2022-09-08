
#### 'matter_chr' class for file-based character vectors ####
## ----------------------------------------------------------

setClass("matter2_fct",
	slots = c(
		levels = "ANY",
		labels = "character"),
	contains = "matter2_vec",
	validity = function(object) {
		errors <- NULL
		if ( !is.null(object@levels) ) {
			if ( length(object@levels) != length(object@labels) )
				errors <- c(errors, "'levels' and 'labels' must be the same length")
			if ( typeof(object@levels) != object@type )
				errors <- c(errors, "type of 'levels' must match object data type")
		}
		if ( is.null(errors) ) TRUE else errors
	})

matter2_fct <- function(data, levels, type = "integer", path = NULL,
	length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, labels = as.character(levels), ...)
{
	if ( !missing(data) ) {
		if ( missing(levels) )
			levels <- sort(unique(data))
		if ( is.na(length) )
			length <- length(data)
		if ( is.null(names) )
			names <- names(data)
		data <- factor(data, levels=levels, labels=labels)
	}
	if ( is.null(path) )
		path <- tempfile(tmpdir=getOption("matter.dump.dir"), fileext=".bin")
	path <- normalizePath(path, mustWork=FALSE)
	exists <- file.exists(path)
	if ( is.na(readonly) )
		readonly <- all(exists)
	if ( any(exists) && !readonly && !missing(data) )
		warning("data may overwrite existing file(s): ",
			paste0(sQuote(path[exists]), collapse=", "))
	if ( all(exists) && missing(data) ) {
		if ( is.na(length) && anyNA(extent) ) {
			sizes <- file.size(path)
			length <- sum((sizes - offset) %/% sizeof(type))
		}
	}
	if ( is.na(length) && anyNA(extent) ) {
		extent <- lengths <- 0
	} else if ( anyNA(extent) ) {
		extent <- length
	} else if ( anyNA(lengths) ) {
		length <- sum(extent)
	}
	if ( length(offset) != length(extent) && length(path) == 1L ) {
		sizes <- sizeof(type) * extent
		offset <- cumsum(c(offset, sizes[-length(sizes)]))
	}
	if ( any(!exists) ) {
		if ( missing(data) && any(extent > 0) )
			warning("creating uninitialized backing file(s): ",
				paste0(sQuote(path[!exists]), collapse=", "))
		success <- file.create(path)
		if ( !all(success) )
			stop("error creating file(s): ",
				paste0(sQuote(path[!success]), collapse=", "))
	}
	x <- new("matter2_fct",
		data=atoms2(
			source=path,
			type=as_Ctype(type),
			offset=offset,
			extent=extent,
			group=0L,
			readonly=readonly),
		type=collapse_Rtype(type),
		dim=length,
		names=names,
		levels=levels,
		labels=labels, ...)
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter2_fct", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory factor")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter2_fct", function(x) {
	preview_vector(x)
	cat("Levels(", nlevels(object), "): ", sep="")
	cat(paste_head(object@levels), "\n")
})

get_matter_fct_elts <- function(x, i = NULL) {
	y <- get_matter_arr_elts(x, i)
	factor(y, levels=levels(x), labels=labels(x))
}

set_matter_fct_elts <- function(x, i = NULL, value = NULL) {
	if ( is.factor(value) )
		value <- labels(x)[value]
	value <- levels(x)[match(value, labels(x))]
	set_matter_arr_elts(x, i, value)
}

setMethod("[", c(x = "matter2_fct"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_fct_elts(x, i, j)
	})

setReplaceMethod("[", c(x = "matter2_fct"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		set_matter_fct_elts(x, i, j, value)
	})

setMethod("labels", "matter2_fct", function(object, ...) x@labels)

setMethod("levels", "matter2_fct", function(x) x@levels)

setReplaceMethod("levels", "matter2_fct",
	function(x, value) {
		if ( is.null(names(value)) ) {
			x@labels <- value
		} else {
			x@levels <- as.vector(value)
			x@labels <- names(value)
		}
		if ( validObject(x) )
			x
	})

