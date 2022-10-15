
#### 'matter_chr' class for file-based character vectors ####
## ----------------------------------------------------------

setClass("matter_str",
	slots = c(encoding = "character"),
	contains = "matter_",
	validity = function(object) {
		errors <- NULL
		if ( !setequal(object@type, "character") )
			errors <- c(errors, "'type' must be 'character'")
		if ( length(object@encoding) != 1L )
			errors <- c(errors, "'encoding' must be a scalar (length 1)")
		if ( is.null(errors) ) TRUE else errors
	})

matter_str <- function(data, type = "character", path = NULL,
	nchar = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, encoding = "unknown", ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( !is.character(data) )
			data <- as.character(data)
		if ( anyNA(nchar) )
			nchar <- as.vector(vapply(data, nchar, numeric(1), "bytes"))
		if ( is.null(names) )
			names <- names(data)
	}
	if ( is.null(path) )
		path <- tempfile(tmpdir=getOption("matter.dump.dir"), fileext=".bin")
	path <- normalizePath(path, mustWork=FALSE)
	exists <- file.exists(path)
	if ( is.na(readonly) )
		readonly <- all(exists)
	if ( any(exists) && !readonly && !missing(data) ) {
		overwrite <- offset != file.size(path)
		if ( any(overwrite) )
			warning("data may overwrite existing file(s): ",
				paste0(sQuote(path[overwrite]), collapse=", "))
	}
	if ( anyNA(nchar) && anyNA(extent) ) {
		extent <- nchar <- rep.int(0, length(nchar))
	} else if ( anyNA(extent) ) {
		extent <- nchar
	} else if ( anyNA(nchar) ) {
		nchar <- extent
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
	x <- new("matter_str",
		data=atoms(
			source=path,
			type=as_Ctype(type),
			offset=as.double(offset),
			extent=as.double(extent),
			group=seq_along(extent) - 1L,
			readonly=readonly),
		type=as_Rtype(type),
		dim=nchar,
		names=names,
		encoding=encoding, ...)
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter_str", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory strings")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_str", function(x) preview_vector(x))

get_matter_str_elts <- function(x, i = NULL, j = NULL) {
	y <- .Call(C_getMatterStrings, x, i, j, PACKAGE="matter")
	Encoding(y) <- Encoding(x)
	set_names(y, names(x), i)
}

set_matter_str_elts <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterStrings, x, i, j, value, PACKAGE="matter")
}

subset_matter_str_elts <- function(x, i = NULL) {
	if ( is.null(i) )
		return(x)
	new("matter_str",
		data=x@data[,i],
		type=x@type[i],
		dim=x@dim[i],
		names=x@names[i],
		encoding=x@encoding)
}

setMethod("[", c(x = "matter_str"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_str_elts(x, i, j)
	})

setReplaceMethod("[", c(x = "matter_str"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( !is.character(value) )
			value <- as.character(value)
		set_matter_str_elts(x, i, j, value)
	})

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


