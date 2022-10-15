
#### 'matter_list' class for file-based lists ####
## ----------------------------------------------

setClass("matter_list", contains = "matter_")

matter_list <- function(data, type = "double", path = NULL,
	lengths = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( !is.list(data) )
			data <- list(data)
		if ( missing(type) )
			type <- as.vector(vapply(data, typeof, character(1)))
		if ( anyNA(lengths) ) {
			lengths <- as.vector(vapply(data, length, numeric(1)))
			chr <- type %in% "character"
			nch <- vapply(data[chr],
				function(ch) nchar(ch, "bytes")[1L], numeric(1))
			lengths[chr] <- nch
		}
		if ( is.null(names) )
			names <- names(data)
		if ( !all(vapply(data, is.atomic, logical(1))) )
			stop("all list elements must be atomic vectors")
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
	if ( anyNA(lengths) && anyNA(extent) ) {
		extent <- lengths <- rep.int(0, length(lengths))
	} else if ( anyNA(extent) ) {
		extent <- lengths
	} else if ( anyNA(lengths) ) {
		lengths <- extent
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
	x <- new("matter_list",
		data=atoms(
			source=path,
			type=as_Ctype(type),
			offset=as.double(offset),
			extent=as.double(extent),
			group=seq_along(extent) - 1L,
			readonly=readonly),
		type=as_Rtype(type),
		dim=lengths,
		names=names, ...)
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

struct <- function(..., filename = NULL, readonly = FALSE, offset = 0)
{
	args <- list(...)
	if ( any(lengths(args) != 1L) )
		stop("all arguments must be length 1")
	if ( any(sapply(args, function(a) is.null(names(a)))) )
		stop("all arguments must be a named scalar")
	if ( !is.null(filename) && length(filename) != 1L )
		stop("'filename' must be a scalar string")
	names <- names(args)
	types <- sapply(args, names, USE.NAMES=FALSE)
	lens <- as.integer(unlist(args))
	offset <- offset + c(0, cumsum(sizeof(types) * lens)[-length(lens)])
	matter_list(NULL, path=filename, type=types, offset=offset,
		lengths=lens, names=names, readonly=readonly)
}

setMethod("describe_for_display", "matter_list", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory list")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_list", function(x) preview_list(x))

get_matter_list_elt <- function(x, i = NULL, j = NULL) {
	.Call(C_getMatterListElt, x, i, j, PACKAGE="matter")
}

set_matter_list_elt <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterListElt, x, i, j, value, PACKAGE="matter")
}

get_matter_list_sublist <- function(x, i = NULL, j = NULL) {
	y <- .Call(C_getMatterListSubset, x, i, j, PACKAGE="matter")
	set_names(y, names(x), i)
}

set_matter_list_sublist <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterListSubset, x, i, j, value, PACKAGE="matter")
}

subset_matter_list_elt <- function(x, i = NULL) {
	if ( length(i) != 1 )
		stop("attempt to select more than one element")
	new("matter_vec",
		data=x@data[,i],
		type=x@type[i],
		dim=x@dim[i],
		names=x@names[i])
}

subset_matter_list_sublist <- function(x, i = NULL) {
	if ( is.null(i) )
		return(x)
	new("matter_list",
		data=x@data[,i],
		type=x@type[i],
		dim=x@dim[i],
		names=x@names[i])
}

setMethod("[[", c(x = "matter_list"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_list_elt(x, i, j)
	})

setReplaceMethod("[[", c(x = "matter_list"),
	function(x, i, value) {
		i <- as_subscripts(i, x)
		set_matter_list_elt(x, i, NULL, value)
	})

setMethod("[", c(x = "matter_list"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_list_sublist(x, i, j)
	})

setReplaceMethod("[", c(x = "matter_list"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( !is.list(value) )
			value <- list(value)
		set_matter_list_sublist(x, i, j, value)
	})

setMethod("$", c(x = "matter_list"),
	function(x, name) {
		i <- pmatch(name, names(x))
		if ( !is.na(i) ) {
			get_matter_list_elt(x, i)
		} else {
			NULL
		}
	})

setReplaceMethod("$", c(x = "matter_list"),
	function(x, name, value) {
		i <- match(name, names(x))
		if ( !is.na(i) ) {
			set_matter_list_elt(x, i, NULL, value)
		} else {
			stop("item ", sQuote(name), " to be replaced not found")
		}
	})

setMethod("dim", "matter_list", function(x) NULL)

setMethod("length", "matter_list", function(x) length(x@dim))

setMethod("lengths", "matter_list",
	function(x) ifelse(x@type %in% "character", 1L, x@dim))


