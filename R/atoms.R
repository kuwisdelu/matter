
#### Define atoms class ####
## -------------------------

setClassUnion("integer_OR_drle", c("integer", "drle"))
setClassUnion("numeric_OR_drle", c("numeric", "drle"))
setClassUnion("factor_OR_drle", c("factor", "drle_fct"))

setClass("atoms",
	slots = c(
		refs = "list_OR_NULL",      # owned resources
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
		if ( n_unique(lens) != 1L )
			errors <- c(errors, paste0("lengths of ",
				"'source' [", lens["source"], "], ",
				"'type' [", lens["type"], "], ",
				"'offset' [", lens["offset"], "], ",
				"'extent' [", lens["extent"], "], ",
				"and 'group' [", lens["group"], "] ",
				"must all be equal"))
		if ( any(levels(object@type) != get_Ctypes()) )
			errors <- c(errors, "invalid 'type' factor levels")
		if ( type(object@offset) != "double" )
			errors <- c(errors, "offset must be of type 'double'")
		if ( type(object@extent) != "double" )
			errors <- c(errors, "extent must be of type 'double'")
		# TODO we shouldn't need to realize drle vectors[]
		# add support for these functions on drle directly
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

atoms <- function(source = tempfile(), type = "int",
	offset = 0, extent = 0, group = 0L,
	readonly = TRUE, refs = NULL)
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
		type <- as_Ctype(type)
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
	x <- new("atoms", source=source, type=type,
		offset=offset, extent=extent, group=group,
		pointers=pointers, readonly=readonly, refs=refs)
	if ( validObject(x) )
		x
}

setMethod("describe_for_display", "atoms", function(x) {
	natoms <- length(x@offset)
	ngroups <- length(x@pointers) - 1L
	desc1 <- paste0("<", natoms, " length> ", class(x))
	desc2 <- paste0("units of data")
	paste0(desc1, " :: ", desc2)
})

setMethod("vm_used", "atoms", function(x) {
	size_bytes(sum(x@extent[] * sizeof(x@type[])))
})

setMethod("shm_used", "atoms", function(x) {
	src <- x@source[]
	shm <- is_shared_memory_pattern(levels(x@source[]))
	i <- as.integer(src) %in% which(shm)
	size_bytes(sum(x@extent[i] * sizeof(x@type[i])))
})

setMethod("show", "atoms", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	n <- getOption("matter.show.head.n")
	x <- as.data.frame(object)
	x$source <- basename(as.character(x$source))
	print(head(x, n=n))
	if ( nrow(x) > n )
		cat("... and", nrow(x) - n, "more atoms\n")
	dms <- as.vector(dims(object))
	n <- sum(as.numeric(object@extent))
	nrows <- min(dms)
	ncols <- length(dms)
	desc1 <- paste0(n, " element", if (n != 1L) "s")
	desc2 <- paste0(nrows, if (n_unique(dms) > 1L) "+" else "", " per group")
	desc3 <- paste0(ncols, " group", if (ncols != 1L) "s" else "")
	cat("(", desc1, " | ", desc2, " | ", desc3, ")\n", sep="")
})

setMethod("path", "atoms", function(object, ...) levels(object@source))

setReplaceMethod("path", "atoms",
	function(object, ..., value) {
		levels(object@source) <- value
		if ( validObject(object) )
			object
	})

setMethod("type", "atoms", function(x) as.factor(x@type))

setMethod("readonly", "atoms", function(x) x@readonly)

setReplaceMethod("readonly", "atoms",
	function(x, value) {
		x@readonly <- value
		if ( validObject(x) )
			x
	})

setMethod("checksum", "character",
	function(x, algo = "sha1", ...) {
		x <- normalizePath(x, mustWork=FALSE)
		hash <- vapply(x, function(name) {
				if ( is_shared_memory_pattern(name) ) {
					NA_character_
				} else {
					digest(name, algo=algo, file=TRUE, ...)
				}
			}, character(1L))
		attr(hash, "algo") <- algo
		hash
	})

setMethod("checksum", "atoms",
	function(x, algo = "sha1", ...) {
		checksum(path(x), algo=algo, ...)
	})

read_atom <- function(x, atom, type = "double")
{
	.Call(C_readAtom, x, as.integer(atom - 1),
		as_Rtype(type), PACKAGE="matter")
}

write_atom <- function(x, atom, value)
{
	.Call(C_writeAtom, x, as.integer(atom - 1),
		value, PACKAGE="matter")
}

read_atoms <- function(x, i, type = "double", group = 0L)
{
	.Call(C_readAtoms, x, i, as_Rtype(type),
		as.integer(group), PACKAGE="matter")
}

write_atoms <- function(x, i, value, group = 0L)
{
	.Call(C_writeAtoms, x, i, value,
		as.integer(group), PACKAGE="matter")
}

subset_atoms1 <- function(x, i = NULL) {
	if ( is.null(i) )
		return(x)
	if ( any(i < 1 | i > length(x)) )
		matter_error("subscript out of bounds")
	atoms(source=droplevels(x@source[i]),
		type=x@type[i],
		offset=x@offset[i],
		extent=x@extent[i],
		group=x@group[i],
		readonly=x@readonly,
		refs=x@refs)
}

subset_atoms2 <- function(x, i = NULL, j = NULL) {
	dms <- dims(x)
	if ( !is.null(j) ) {
		if ( anyNA(j) )
			matter_error("NAs not allowed when subsetting atoms")
		if ( any(j < 1 | j > length(dms)) )
			matter_error("subscript out of bounds")
		grp <- as.integer(x@group) + 1L
		j <- lapply(j, function(g) which(grp %in% g))
		j <- unlist(j)
		x <- subset_atoms1(x, j)
	}
	if ( !is.null(i) ) {
		if ( anyNA(i) )
			matter_error("NAs not allowed when subsetting atoms")
		if ( any(i < 1 | i > min(dms)) )
			matter_error("subscript out of bounds")
		sub <- .Call(C_subsetAtoms, x, i, PACKAGE="matter")
		x <- atoms(source=droplevels(x@source[sub$index]),
			type=x@type[sub$index],
			offset=sub$offset,
			extent=sub$extent,
			group=x@group[sub$index],
			readonly=x@readonly,
			refs=x@refs)
	}
	if ( validObject(x) )
		x
}

regroup_atoms <- function(x, ngroups) {
	if ( length(ngroups) == 1L ) {
		if ( ngroups <= 1L )
			return(x)
		sub <- .Call(C_regroupAtoms, x, ngroups, PACKAGE="matter")
		atoms(source=droplevels(x@source[sub$index]),
			type=x@type[sub$index],
			offset=sub$offset,
			extent=sub$extent,
			group=sub$group,
			readonly=x@readonly,
			refs=x@refs)
	} else {
		atoms(source=x@source,
			type=x@type,
			offset=x@offset,
			extent=x@extent,
			group=ngroups,
			readonly=x@readonly,
			refs=x@refs)
	}
}

ungroup_atoms <- function(x) {
	if ( length(x) <= 1L )
		return(x)
	sub <- .Call(C_ungroupAtoms, x, PACKAGE="matter")
	atoms(source=droplevels(x@source[sub$index]),
		type=x@type[sub$index],
		offset=sub$offset,
		extent=sub$extent,
		group=0L,
		readonly=x@readonly,
		refs=x@refs)
}

requisition_atoms <- function(x) {
	shms <- path(x)[is_shared_memory_pattern(path(x))]
	for ( shm in shms )
	{
		i <- x@source[] %in% shm
		max_extent <- max(x@offset[i] + x@extent[i] * sizeof(x@type[i]))
		if ( max_extent > sizeof_memory_resource(shm) ) {
			if ( readonly(x) ) {
				matter_error("cannot resize read-only shared memory")
			} else {
				newsize <- resize_memory_resource(shm, max_extent)
			}
		}
		if ( newsize < max_extent )
			matter_error("failed to resize shared memory: ", sQuote(shm))
	}
}

setMethod("as.data.frame", "atoms",
	function(x, ...){
		data.frame(
			source=x@source[],
			type=x@type[],
			offset=x@offset[],
			extent=x@extent[],
			group=x@group[])
	})

setMethod("as.list", "atoms",
	function(x, ...) as.list(as.data.frame(x, ...)))

# no. of atoms
setMethod("length", "atoms", function(x) length(x@offset))

# lengths of atoms
setMethod("lengths", "atoms",
	function(x, use.names = TRUE) as.double(x@extent))

# nrows = length of groups, ncols = no. of groups
setMethod("dim", "atoms",
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

# lengths of groups
setMethod("dims", "atoms",
	function(x, use.names = TRUE) {
		extents <- as.double(x@extent)
		groups <- as.integer(x@group)
		nrows <- tapply(extents, groups, sum)
		if ( use.names ) {
			nms <- seq_along(nrows) - 1L
		} else {
			nms <- NULL
		}
		t(set_names(nrows, nms))
	})

setMethod("cbind2", "atoms",
	function(x, y, ...) {
		x@group <- as.integer(x@group)
		y@group <- as.integer(y@group)
		y@group <- y@group + max(x@group) + 1L
		atoms(source=c(x@source, y@source),
			type=c(x@type, y@type),
			offset=c(x@offset, y@offset),
			extent=c(x@extent, y@extent),
			group=c(x@group, y@group),
			readonly=x@readonly || y@readonly,
			refs=c(x@refs, y@refs))
	})

setMethod("rbind2", "atoms",
	function(x, y, ...) {
		groups <- c(x@group, y@group)
		ind <- order(groups, method="radix")
		atoms(source=c(x@source, y@source)[ind],
			type=c(x@type, y@type)[ind],
			offset=c(x@offset, y@offset)[ind],
			extent=c(x@extent, y@extent)[ind],
			group=groups[ind],
			readonly=x@readonly || y@readonly,
			refs=c(x@refs, y@refs))
	})

setMethod("combine", "atoms",
	function(x, y, ...) rbind2(x, y, ...))

setMethod("[", c(x="atoms"),
	function(x, i, j, ..., drop = TRUE) {
		narg <- nargs() - 1L - !missing(drop)
		if ( ...length() > 0 )
			matter_error("incorrect number of dimensions")
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( narg == 1L ) {
			subset_atoms1(x, i)
		} else {
			subset_atoms2(x, i, j)
		}
	})

setMethod("[[", "atoms",
	function(x, i, ...) {
		if ( length(i) > 1 ) {
			matter_error("attempt to select more than one element")
		} else {
			x[i]
		}
	})

