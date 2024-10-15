
#### 'matter_fct' class for file-based character vectors ####
## ----------------------------------------------------------

setClass("matter_fct",
	slots = c(
		levels = "ANY",
		labels = "character"),
	contains = "matter_vec",
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

matter_fct <- function(data, levels, type = typeof(levels), path = NULL,
	length = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, append = FALSE, labels = as.character(levels), ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( is.na(length) )
			length <- length(data)
		if ( is.null(names) )
			names <- names(data)
		if ( missing(levels) )
			levels <- sort(unique(data))
		data <- factor(data, levels=levels, labels=labels)
		levels <- seq_len(nlevels(data))
		labels <- levels(data)
	}
	x <- matter_vec(NULL, type=type, path=path, length=length,
		names=names, offset=offset, extent=extent,
		readonly=readonly, append=append, rowMaj=FALSE, ...)
	x <- as(x, "matter_fct")
	x@levels <- levels
	x@labels <- labels
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	if ( validObject(x) )
		x
}

setMethod("as.vector", "matter_fct",
	function(x, mode = "any")
		as.vector(as.factor(x), mode=mode))

setMethod("as.factor", "matter_fct",
	function(x) {
		names(x) <- NULL
		dimnames(x) <- NULL
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(x)
		} else {
			x[]
		}
	})

setMethod("describe_for_display", "matter_fct", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-core factor")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_fct", function(x) {
	preview_vector(x)
	cat("Levels(", nlevels(x), "): ", sep="")
	cat(paste_head(x@labels), "\n")
})

copy_to_matter_fct <- function(object, type = NULL, path = NULL,
	offset = 0, readonly = TRUE, append = FALSE, ..., BPPARAM)
{
	BPPARAM <- bplocalized(BPPARAM)
	if ( is.matter(object) ) {
		type <- type %||% type(atomdata(object))
		extent <- lengths(atomdata(object))
	} else {
		type <- type %||% type(object)
		extent <- length(object)
	}
	x <- matter_fct(NULL, type=type, path=path,
		length=length(object), names=names(object),
		levels=object@levels, labels=object@labels,
		offset=offset, extent=extent, readonly=FALSE,
		append=append)
	pid <- ipcid()
	FUN <- copy_to_matter_fun(pid, x)
	chunk_lapply(object, FUN, ..., BPPARAM=BPPARAM)
	readonly(x) <- readonly
	ipcremove(pid)
	if ( validObject(x) )
		x
}

setMethod("fetch", "matter_fct",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_fct(object, path=":memory:", ..., BPPARAM=BPPARAM))

setMethod("flash", "matter_fct",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_fct(object, ..., BPPARAM=BPPARAM))

setMethod("fetch", "factor",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_fct(object, path=":memory:", ..., BPPARAM=BPPARAM))

setMethod("flash", "factor",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_fct(object, ..., BPPARAM=BPPARAM))

get_matter_fct_elts <- function(x, i = NULL) {
	y <- get_matter_arr_elts(x, i)
	factor(y, levels=x@levels, labels=x@labels)
}

set_matter_fct_elts <- function(x, i = NULL, value = NULL) {
	if ( is.factor(value) )
		value <- x@labels[value]
	value <- x@levels[match(value, x@labels)]
	set_matter_arr_elts(x, i, value)
}

setMethod("[", c(x = "matter_fct"),
	function(x, i, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		if ( is_null_or_na(drop) ) {
			subset_matter_arr_elts(x, i)
		} else {
			get_matter_fct_elts(x, i)
		}
	})

setReplaceMethod("[", c(x = "matter_fct"),
	function(x, i, ..., value) {
		i <- as_subscripts(i, x)
		set_matter_fct_elts(x, i, value)
	})

setMethod("combine", "matter_fct",
	function(x, y, ...) {
		if ( any(x@levels != y@levels) )
			matter_error("factor levels must match")
		if ( any(x@labels != y@labels) )
			matter_error("factor labels must match")
		new("matter_fct", callNextMethod(),
			levels=x@levels, labels=x@labels)
	})

setMethod("levels", "matter_fct", function(x) x@labels)

setReplaceMethod("levels", "matter_fct",
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

