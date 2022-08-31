
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
		# FIXME: shouldn't need to realize drle vectors[]
		# (add support for these functions on drle directly)
		if ( any(levels(object@type) != levels(make_datamode())) )
			errors <- c(errors, "invalid 'type' factor levels")
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

atoms2 <- function(source = tempfile(), type = "int",
	offset = 0, extent = 0, group = 0L, readonly = TRUE)
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
	if ( !is(source, "factor_OR_drle") )
		source <- as.factor(source)
	if ( !is(type, "factor_OR_drle") )
		type <- make_datamode(type, type="C")
	group <- as.logical(diff(as.integer(group)))
	pointers <- c(0L, which(group), n)
	group <- cumsum(c(0L, group))
	compress <- getOption("matter.compress.atoms")
	if ( compress && n > 1 ) {
		source <- drle(source, cr_threshold=compress)
		type <- drle(type, cr_threshold=compress)
		offset <- drle(offset, cr_threshold=compress)
		extent <- drle(extent, cr_threshold=compress)
		group <- drle(group, cr_threshold=compress)
		pointers <- drle(pointers, cr_threshold=compress)
	}
	x <- new("atoms2", source=source, type=type,
		offset=offset, extent=extent, group=group,
		pointers=pointers, readonly=readonly)
	if ( validObject(x) )
		x
}

setMethod("describe_for_display", "atoms2", function(x) {
	natoms <- length(x@offset)
	ngroups <- length(x@pointers) - 1L
	desc1 <- paste0("<", natoms, " length> ", class(x))
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
	dms <- as.vector(dims(object))
	nrows <- min(dms)
	ncols <- length(dms)
	if ( length(unique(dms)) > 1 ) {
		cat("(", nrows, "+ elements per group | ",
			ncols, " groups)\n", sep="")
	} else {
		cat("(", nrows, " elements per group | ",
			ncols, " groups)\n", sep="")
	}
})

read_atom <- function(x, atom, type = "double")
{
	.Call("C_readAtom", x, as.integer(atom - 1),
		make_datamode(type, type="R"), PACKAGE="matter")
}

write_atom <- function(x, atom, value)
{
	.Call("C_writeAtom", x, as.integer(atom - 1),
		value, PACKAGE="matter")
}

read_atoms <- function(x, i, type = "double", group = 0L)
{
	.Call("C_readAtoms", x, i, make_datamode(type, type="R"),
		as.integer(group), PACKAGE="matter")
}

write_atoms <- function(x, i, value, group = 0L)
{
	.Call("C_writeAtoms", x, i, value,
		as.integer(group), PACKAGE="matter")
}

subset_atoms <- function(x, i) {
	sub <- .Call("C_subsetAtoms", x, i, PACKAGE="matter")
	# FIXME: Make sure drle_fc supports droplevels()
	atoms2(source=droplevels(x@source[sub$index]),
			type=x@type[sub$index],
			offset=sub$offset,
			extent=sub$extent,
			group=x@group[sub$index],
			readonly=x@readonly)
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

setMethod("length", "atoms2", function(x) length(x@offset))

setMethod("lengths", "atoms2",
	function(x, use.names = TRUE) as.double(x@extent))

setMethod("dim", "atoms2",
	function(x) {
		extents <- as.double(x@extent)
		groups <- as.integer(x@group)
		ncols <- length(x@pointers) - 1
		nrows <- unique(tapply(extents, groups, sum))
		if ( length(nrows) > 1 ) {
			nrows <- NA_integer_ # if jagged array
		} else {
			nrows <- as.vector(nrows) # drop names
		}
		c(nrows, ncols)
	})

setMethod("dims", "atoms2",
	function(x, use.names = TRUE) {
		extents <- as.double(x@extent)
		groups <- as.integer(x@group)
		nrows <- tapply(extents, groups, sum)
		if ( use.names ) {
			nms <- seq_along(nrows) - 1L
		} else {
			nms <- NULL
		}
		t(setNames(nrows, nms))
	})

setMethod("cbind2", "atoms2",
	function(x, y, ...) {
		x@group <- as.integer(x@group)
		y@group <- as.integer(y@group)
		y@group <- y@group + max(x@group) + 1L
		atoms2(source=c(x@source, y@source),
			type=c(x@type, y@type),
			offset=c(x@offset, y@offset),
			extent=c(x@extent, y@extent),
			group=c(x@group, y@group),
			readonly=x@readonly || y@readonly)
	})

setMethod("rbind2", "atoms2",
	function(x, y, ...) {
		groups <- c(x@group, y@group)
		o <- order(groups, method="radix")
		atoms2(source=c(x@source, y@source)[o],
			type=c(x@type, y@type)[o],
			offset=c(x@offset, y@offset)[o],
			extent=c(x@extent, y@extent)[o],
			group=groups[o],
			readonly=x@readonly || y@readonly)
	})

setMethod("[", c(x="atoms2"),
	function(x, i, j, ...) {
		if ( nargs() == 2 ) {
			if ( any(i <= 0 | i > length(x)) )
				stop("subscript out of bounds")
			# FIXME: Make sure drle_fc supports droplevels()
			atoms2(source=droplevels(x@source[i]),
				type=x@type[i],
				offset=x@offset[i],
				extent=x@extent[i],
				group=x@group[i],
				readonly=x@readonly)
		} else {
			if ( ...length() > 0 )
				stop("incorrect number of dimensions")
			if ( !missing(j) )
				x <- x[which(as.integer(x@group) %in% (j - 1L))]
			if ( !missing(i) )
				x <- subset_atoms(x, i)
			if ( validObject(x) )
				x
		}
	})

setMethod("[[", "atoms2",
	function(x, i, ...) {
		if ( length(i) > 1 ) {
			stop("attempt to select more than one element")
		} else {
			x[i]
		}
	})

setMethod("c", "atoms2", function(x, ...)
{
	if ( ...length() > 0 ) {
		do.call(rbind, list(x, ...))
	} else {
		x
	}
})



