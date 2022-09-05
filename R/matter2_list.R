
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
			type <- vapply(data, typeof, character(1))
		if ( anyNA(lengths) )
			lengths <- vapply(data, length, numeric(1))
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
	if ( length(offset) != length(extent) && length(path) == 1L )
		offset <- cumsum(c(offset,
			sizeof(type) * extent[-length(extent)]))
	if ( any(!exists) ) {
		if ( missing(data) && any(extent > 0) )
			warning("creating uninitialized backing file(s): ",
				paste0(sQuote(path[!exists]), collapse=", "))
		success <- file.create(path)
		if ( !all(success) )
			stop("error creating file(s): ",
				paste0(sQuote(path[!success]), collapse=", "))
	}
	x <- new("matter2_arr",
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
	desc1 <- paste0("<", x@length, " length> ", class(x))
	desc2 <- paste0("out-of-memory list")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter2_list", function(x) preview_list(x))


