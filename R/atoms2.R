

#### Define atoms class ####
## -------------------------

setClassUnion("integer_OR_drle", c("integer", "drle"))
setClassUnion("numeric_OR_drle", c("numeric", "drle"))
setClassUnion("factor_OR_drle", c("factor", "drle_fc"))

setClass("atoms2",
	slots = c(
		source = "factor_OR_drle",  # data sources
		type = "factor_OR_drle",    # data types
		offset = "numeric_OR_drle", # byte offset in data source
		extent = "numeric_OR_drle", # number of elements
		group = "integer_OR_drle",  # organize atoms
		pointers = "integer_OR_drle", # find groups
		readonly = "logical"),
	validity = function(object) {
		errors <- NULL
		lens <- c(
			source=length(object@source),
			type=length(object@type),
			offset=length(object@offset),
			extent=length(object@extent),
			group=length(object@group))
		if ( length(unique(lens)) != 1 )
			errors <- c(errors, paste0("lengths of ",
				"'source' [", lens["source"], "], ",
				"'type' [", lens["type"], "], ",
				"'offset' [", lens["offset"], "], ",
				"'extent' [", lens["extent"], "], ",
				"and 'group' [", lens["group"], "] ",
				"must all be equal"))
		# FIXME: shouldn't need to realize drle objects[]
		# (add supported for these functions on drle directly)
		if ( any(object@offset[] < 0) )
			errors <- c(errors, "'offset' must be non-negative")
		if ( any(object@extent[] < 0) )
			errors <- c(errors, "'extent' must be non-negative")
		if ( object@group[1] != 0 )
			errors <- c(errors, "'group' must start at 0")
		if ( is.unsorted(object@group[]) || any(object@group[] < 0) )
			errors <- c(errors, "'group' must be sorted and non-negative")
		if ( max(object@group[]) + 1 != length(object@pointers) - 1 )
			errors <- c(errors, "'pointers' does not conform with 'group'")
		if ( length(object@readonly) != 1L )
			errors <- c(errors, "'readonly' must be a scalar logical")
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("initialize", "atoms2",
	function(.Object, ...) {
		callNextMethod()
		p <- which(as.logical(diff(.Object@group[]))) # FIXME: shouldn't need to realize groups[]
		p <- c(0L, p, length(.Object@group))
		p <- drle(p, cr_threshold=getOption("matter.compress.atoms"))
		.Object@pointers <- p
		.Object
	})

atoms2 <- function(source, type = "double", offset = 0, extent = 0,
	group = 0L, ..., readonly = TRUE, compress = getOption("matter.compress.atoms"))
{
	n <- max(length(source), length(type),
		length(offset), length(extent), length(group))
	if ( length(source) != n )
		source <- rep_len(source, n)
	if ( length(type) != n )
		type <- rep_len(type, n)
	if ( length(offset) != n )
		offset <- rep_len(offset, n)
	if ( length(extent) != n )
		extent <- rep_len(extent, n)
	if ( length(group) != n )
		group <- rep_len(group, n)
	source <- as.factor(source)
	type <- make_datamode(type, type="C")
	if ( compress > 0 && n > 1 ) {
		source <- drle(source, cr_threshold=compress)
		type <- drle(type, cr_threshold=compress)
		offset <- drle(offset, cr_threshold=compress)
		extent <- drle(extent, cr_threshold=compress)
		group <- drle(group, cr_threshold=compress)
	}
	x <- new("atoms2", source=source, type=type,
		offset=offset, extent=extent,
		group=group, readonly=readonly)
	if ( validObject(x) )
		x
}

setMethod("as.data.frame", "atoms2",
	function(x, ...){
		data.frame(
			source=x@source,
			type=x@type,
			offset=x@offset,
			extent=x@extent,
			group=x@group)
	})

setMethod("as.list", "atoms2",
	function(x, ...) as.list(as.data.frame(x, ...)))

setMethod("length", "atoms2", function(x) sum(x@extent))

setMethod("combine", "atoms2",
	function(x, y, ...) {
		new("atoms2",
			source=c(x@source, y@source),
			type=c(x@type, y@type),
			offset=c(x@offset, y@offset),
			extent=c(x@extent, y@extent),
			group=c(x@group, y@group),
			readonly=x@readonly | y@readonly)
	})

setMethod("[", c(x="atoms2"),
	function(x, i, ...) {
		if ( ...length() > 0 )
			stop("incorect number of dimensions")
		if ( anyNA(i) )
			stop("NAs not allowed when subsetting atoms")
		new("atoms",
			source=x@source[i],
			type=x@type[i],
			offset=x@offset[i],
			extent=x@extent[i],
			group=x@group[i],
			readonly=x@readonly)
	})

setMethod("[[", "atoms2",
	function(x, i, ...) {
		if ( length(i) > 1 )
			stop("attempt to select more than one element")
		x[i]
	})

setMethod("c", "atoms2", function(x, ...)
{
	if ( ...length() > 0 ) {
		do.call(combine, list(x, ...))
	} else {
		x
	}
})

setMethod("describe_for_display", "atoms2", function(x) {
	natoms <- length(x@offset)
	ngroups <- length(x@groups) - 1L
	desc1 <- paste0("<", natoms, " length, ", ngroups, " groups> ", class(x))
	desc2 <- paste0("units of data")
	paste0(desc1, " :: ", desc2)
})

setMethod("show", "atoms2", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	n <- getOption("matter.show.head.n")
	x <- as.data.frame(object)
	x$source <- basename(as.character(x$source))
	print(head(x, n=n))
	if ( nrow(x) > n )
		cat("... and", nrow(x) - n, "more atoms\n")
})

