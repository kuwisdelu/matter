
#### Define visual encoding classes for 'vizi' ####
## ------------------------------------------------

setClass("vz_",
	contains = "VIRTUAL",
	slots = c(
		data = "ANY",
		encoding = "list_OR_NULL",
		transform = "list_OR_NULL",
		params = "list_OR_NULL"))

setClass("vz_graphic",
	contains = "vz_",
	slots = c(
		layers = "list",
		channels = "list",
		facets = "facet_OR_NULL"))

vizi <- function(data, ..., encoding = NULL, type = NULL)
{
	encoding <- merge_encoding(encoding, as_encoding(...))
	new("vz_graphic", data=data, encoding=encoding)
}

setClass("vz_channel",
	slots = c(
		channel = "character",
		name = "character",
		type = "character",
		scale = "character",
		scheme = "ANY",
		limits = "ANY",
		labels = "character_OR_NULL",
		breaks = "ANY",
		pad = "numeric_OR_NULL",
		guide = "logical"))

channel <- function(plot, channel, name = NULL, type = NULL,
		scale = NULL, scheme = NULL, limits = NULL, labels = NULL,
		pad = NULL, breaks = NULL, guide = TRUE)
{
	ch <- new("vz_channel", channel=channel, name=name, type=type,
		scale=scale, scheme=scheme, limits=limits, labels=labels,
		pad=pad, breaks=breaks, guide=guide)
	ch <- setNames(list(ch), channel)
	plot@channels <- c(plot@channels, ch)
	plot
}

#### Internal functions for encoding channels ####
## -----------------------------------------------

as_encoding <- function(x, y, ..., env = NULL)
{
	args <- list(...)
	if ( !missing(y) )
		args <- c(list(y=y), args)
	if ( !missing(x) )
		args <- c(list(x=x), args)
	encoding <- lapply(args, function(e) {
		if ( is(e, "formula") && !is.null(env) )
			environment(e) <- env
		e
	})
	normalize_encoding(encoding)
}

normalize_encoding <- function(e, check = TRUE) {
	if ( length(e) == 0L )
		return(NULL)
	nms <- names(e)
	nms <- vapply(nms, function(nm) {
		val <- e[[nm]]
		if ( check && !inherits(val, c("formula", "character")) )
			stop("encodings must be a formula or string")
		if ( check && is.character(val) && length(val) != 1L )
			stop("encodings specified as strings must be length-1")
		switch(nm,
			bg = "fill",
			cex = "size",
			col = "color",
			colour = "color",
			fg = "color",
			lend = "lineend",
			ljoin = "linejoin",
			lmitre = "linemitre",
			lty = "linetype",
			lwd = "size",
			pch = "shape",
			nm)
	}, character(1))
	if ( length(e) > 1L )
		names(e) <- nms
	e
}

merge_encoding <- function(e1, e2)
{
	if ( !is.null(e1) && !is.null(e2) ) {
		e <- e1
		for ( nm in names(e2) )
			e[[nm]] <- e2[[nm]]
	} else if ( is.null(e1) && !is.null(e2) ) {
		e <- e2
	} else if ( !is.null(e1) && is.null(e2) ) {
		e <- e1
	} else {
		e <- NULL
	}
	e
}
