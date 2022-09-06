
#### Define matter<list> class for list data ####
## ----------------------------------------------

setClass("matter2_list", contains = "matter2_")

matter2_list <- function(data, type = "double", path = NULL,
	lengths = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, ...)
{
	if ( !missing(data) ) {
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
	if ( any(exists) && !readonly && !missing(data) )
		warning("data may overwrite existing file(s): ",
			paste0(sQuote(path[exists]), collapse=", "))
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
	x <- new("matter2_list",
		data=atoms2(
			source=path,
			type=as_Ctype(type),
			offset=offset,
			extent=extent,
			group=seq_along(extent) - 1L,
			readonly=readonly),
		type=as_Rtype(type),
		dim=lengths,
		names=names, ...)
	if ( !missing(data) && !is.null(data) )
		x[] <- data
	x
}

setMethod("describe_for_display", "matter2_list", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-memory list")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter2_list", function(x) preview_list(x))

get_matter_list_elt <- function(x, i = NULL, j = NULL) {
	.Call("C_getMatterListElt", x, i, j)
}

set_matter_list_elt <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call("C_setMatterListElt", x, i, j, value)
}

get_matter_list_sublist <- function(x, i = NULL, j = NULL) {
	if ( is.null(i) )
		i <- seq_along(x)
	y <- vector("list", length(i))
	for ( k in seq_along(i) )
		y[[k]] <- get_matter_list_elt(x, i[k], j)
	set_names(y, names(x), i)
}

set_matter_list_sublist <- function(x, i = NULL, j = NULL, value = NULL) {
	if ( is.null(i) )
		i <- seq_along(x)
	for ( k in seq_along(i) )
		set_matter_list_elt(x, i[k], j, value[[k]])
	x
}

subset_matter_list_elt <- function(x, i = NULL) {
	if ( length(i) != 1 )
		stop("attempt to select more than one element")
	new("matter2_vec",
		data=x@data[,i],
		type=x@type[i],
		dim=x@dim[i],
		names=x@names[i])
}

subset_matter_list_sublist <- function(x, i = NULL) {
	if ( is.null(i) )
		return(x)
	new("matter2_list",
		data=x@data[,i],
		type=x@type[i],
		dim=x@dim[i],
		names=x@names[i])
}

setMethod("[[", c(x = "matter2_list"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_list_elt(x, i, j)
	})

setReplaceMethod("[[", c(x = "matter2_list"),
	function(x, i, value) {
		i <- as_subscripts(i, x)
		set_matter_list_elt(x, i, value)
	})

setMethod("[", c(x = "matter2_list"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_list_sublist(x, i, j)
	})

setReplaceMethod("[", c(x = "matter2_list"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( !is.list(value) )
			value <- list(value)
		set_matter_list_sublist(x, i, j, value)
	})

setMethod("$", c(x = "matter2_list"),
	function(x, name) {
		i <- pmatch(name, names(x))
		if ( !is.na(i) ) {
			get_matter_list_elt(x, i)
		} else {
			NULL
		}
	})

setReplaceMethod("$", c(x = "matter2_list"),
	function(x, name, value) {
		i <- match(name, names(x))
		if ( !is.na(i) ) {
			set_matter_list_elt(x, i)
		} else {
			stop("item ", sQuote(name), " to be replaced not found")
		}
	})


setMethod("length", "matter2_list", function(x) length(x@dim))

setMethod("lengths", "matter2_list",
	function(x) ifelse(x@type %in% "character", 1L, x@dim))


