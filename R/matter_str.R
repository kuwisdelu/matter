
#### 'matter_str' class for file-based character vectors ####
## ----------------------------------------------------------

setClass("matter_str",
	slots = c(encoding = "character"),
	contains = "matter_list",
	validity = function(object) {
		errors <- NULL
		if ( !setequal(object@type, "character") )
			errors <- c(errors, "'type' must be 'character'")
		if ( length(object@encoding) != 1L && length(object@encoding) != length(object) )
			errors <- c(errors, "'encoding' must be a scalar or match data extent")
		if ( is.null(errors) ) TRUE else errors
	})

matter_str <- function(data, encoding, type = "character", path = NULL,
	nchar = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, append = FALSE, ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( !is.character(data) )
			data <- as.character(data)
		if ( anyNA(nchar) )
			nchar <- as.vector(vapply(data, nchar, numeric(1), "bytes"))
		if ( is.null(names) )
			names <- names(data)
		if ( missing(encoding) )
			encoding <- Encoding(data)
	}
	x <- matter_list(NULL, type=type, path=path, lengths=nchar,
		names=names, offset=offset, extent=extent,
		readonly=readonly, append=append, ...)
	x <- as(x, "matter_str")
	x@encoding <- if (missing(encoding)) "unknown" else encoding
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	if ( validObject(x) )
		x
	x
}

setAs("matter_list", "matter_str",
	function(from) {
		x <- new("matter_str",
			data=from@data,
			type=from@type,
			dim=from@dim,
			names=from@names,
			dimnames=NULL,
			encoding="unknown")
		if ( validObject(x) )
			x
	})

setAs("matter_str", "matter_list",
	function(from) {
		x <- new("matter_list",
			data=from@data,
			type=from@type,
			dim=from@dim,
			names=from@names,
			dimnames=NULL)
		if ( validObject(x) )
			x
	})

setMethod("as.vector", "matter_str",
	function(x, mode = "any")
		as.vector(as.character(x), mode=mode))

setMethod("as.character", "matter_str",
	function(x, ...) {
		names(x) <- NULL
		dimnames(x) <- NULL
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(x)
		} else {
			x[]
		}
	})

setMethod("describe_for_display", "matter_str", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory strings")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_str", function(x) preview_vector(x))

setMethod("mem_realized", "matter_str", function(x) {
	size_bytes(sum(lengths(x) * sizeof(type(x)), na.rm=TRUE))
})

copy_to_matter_str <- function(object, path = NULL, ..., BPPARAM)
{
	x <- matter_str(NULL, path=path,
		type=type(atomdata(object)), extent=lengths(atomdata(object)),
		nchar=object@dim, names=names(object),
		encoding=Encoding(object))
	type(x) <- type(object)
	pid <- ipcid()
	FUN <- copy_to_matter_fun(pid, x)
	chunk_lapply(object, FUN, ..., BPPARAM=BPPARAM)
	readonly(x) <- readonly(object)
	ipcremove(pid)
	if ( validObject(x) )
		x
}

setMethod("fetch", "matter_str",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_str(object, path=":memory:", ..., BPPARAM=BPPARAM))

setMethod("flash", "matter_str",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_str(object, ..., BPPARAM=BPPARAM))


subset_matter_str_elts <- function(x, i = NULL)
{
	encoding <- Encoding(x)
	x <- as(x, "matter_list")
	x <- subset_matter_list_sublist(x, i)
	x <- as(x, "matter_str")
	if ( !is.null(i) && length(encoding) != 1L ) {
		Encoding(x) <- encoding[i]
	} else {
		Encoding(x) <- encoding
	}
	x
}

get_matter_str_elts <- function(x, i = NULL, j = NULL) {
	y <- .Call(C_getMatterStrings, x, i, j, PACKAGE="matter")
	if ( !is.null(i) && length(Encoding(x)) != 1L ) {
		Encoding(y) <- Encoding(x)[i]
	} else {
		Encoding(y) <- Encoding(x)
	}
	set_names(y, names(x), i)
}

set_matter_str_elts <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterStrings, x, i, j, value, PACKAGE="matter")
}

setMethod("[", c(x = "matter_str"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( is_null_or_na(drop) ) {
			if ( !is.null(j) )
				matter_warn("ignoring array subscripts")
			subset_matter_str_elts(x, i)
		} else {
			get_matter_str_elts(x, i, j)
		}
	})

setReplaceMethod("[", c(x = "matter_str"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( !is.character(value) )
			value <- as.character(value)
		set_matter_str_elts(x, i, j, value)
	})

setMethod("combine", "matter_str",
	function(x, y, ...) {
		data <- cbind(x@data, y@data)
		if ( length(x@encoding) == 1L && length(y@encoding) == 1L ) {
			if ( x@encoding != y@encoding)
				matter_warn("scalar encodings do not match; inheriting from first one")
			encoding <- x@encoding
		} else {
			len <- max(length(x@encoding), length(y@encoding))
			ex <- rep_len(x@encoding, len)
			ey <- rep_len(y@encoding, len)
			encoding <- c(ex, ey)
		}
		new(class(x), data=data,
			type=x@type,
			dim=c(x@dim, y@dim),
			names=combine_names(x, y),
			dimnames=NULL,
			encoding=encoding)
	})

setMethod("c", "matter_str", combine_any)

setMethod("Encoding", "matter_str", function(x) x@encoding)

setReplaceMethod("Encoding", "matter_str",
	function(x, value) {
		x@encoding <- value
		if ( validObject(x) )
			x
	})

setMethod("dim", "matter_str", function(x) NULL)

setMethod("length", "matter_str", function(x) length(x@dim))

setMethod("lengths", "matter_str", function(x) x@dim)


