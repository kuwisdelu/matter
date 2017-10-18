

#### Define atoms class ####
## -------------------------

setClassUnion("integer_OR_drle", c("integer", "drle"))
setClassUnion("numeric_OR_drle", c("numeric", "drle"))

setClass("atoms",
	slots = c(
		natoms = "integer",
		ngroups = "integer",
		group_id = "integer_OR_drle",
		source_id = "integer_OR_drle",
		datamode = "integer_OR_drle",
		offset = "numeric_OR_drle", # byte offset from start of file
		extent = "numeric_OR_drle", # number of elements
		index_offset = "numeric_OR_drle", # cumulative index of first element
		index_extent = "numeric_OR_drle"), # cumulative index of one-past-the-end
	prototype = c(
		natoms = 1L,
		ngroups = 1L,
		group_id = 1L,
		source_id = as.integer(NA),
		datamode = make_datamode("double", type="C"),
		offset = numeric(1),
		extent = numeric(1),
		index_extent = numeric(1),
		index_offset = numeric(1)),
	validity = function(object) {
		errors <- NULL
		lens <- c(group_id=length(object@group_id),
			source_id=length(object@source_id),
			datamode=length(object@datamode),
			offset=length(object@offset),
			extent=length(object@extent),
			index_offset=length(object@index_offset),
			index_extent=length(object@index_extent))
		if ( length(unique(lens)) != 1 )
			stop("lengths of 'source_id' [", lens["source_id"], "], ",
				"'datamode' [", lens["datamode"], "], ",
				"'offset' [", lens["offset"], "], ",
				"'extent' [", lens["extent"], "], ",
				"'index_offset' [", lens["index_offset"], "], ",
				"and 'index_extent' [", lens["index_extent"], "], ",
				"must all be equal")
		if ( object@natoms != unique(lens) )
			errors <- c(errors, "'natoms' not equal to the number of elements")
		if ( object@ngroups != max(object@group_id[]) )
			errors <- c(errors, "'ngroups' not equal to the number of groups")
		if ( is.unsorted(object@group_id[]) || any(object@group_id[] <= 0) )
			errors <- c(errors, "'group_id' must be positive and increasing")
		if ( object@index_offset[1] != 0 )
			errors <- c(errors, "'index_offset' must begin at 0")
		if ( is.null(errors) ) TRUE else errors
	})

atoms <- function(group_id = 1L, source_id = as.integer(NA),
					datamode=make_datamode("double", type="C"),
					offset = numeric(1), extent = numeric(1),
					..., compress = TRUE, cr_threshold = 3)
{
	if ( is.unsorted(group_id[]) ) {
		o <- order(group_id[])
		group_id <- group_id[o]
		source_id <- source_id[o]
		datamode <- datamode[o]
		offset <- offset[o]
		extent <- extent[o]
	}
	x <- .Call("C_createAtoms", group_id, source_id, datamode, offset, extent, PACKAGE="matter")
	if ( compress && x@natoms > 1 ) {
		x@group_id <- drle(x@group_id, cr_threshold=cr_threshold)
		x@source_id <- drle(x@source_id, cr_threshold=cr_threshold)
		x@datamode <- drle(x@datamode, cr_threshold=cr_threshold)
		x@offset <- drle(x@offset, cr_threshold=cr_threshold)
		x@extent <- drle(x@extent, cr_threshold=cr_threshold)
		x@index_offset <- drle(x@index_offset, cr_threshold=cr_threshold)
		x@index_extent <- drle(x@index_extent, cr_threshold=cr_threshold)
	}
	x
}

setMethod("length", "atoms", function(x) x@ngroups)

setMethod("datamode", "atoms", function(x) x@datamode)

setMethod("combine", "atoms", function(x, y, ...) {
	atoms(group_id=combine(x@group_id, y@group_id),
		source_id=combine(x@source_id, y@source_id),
		datamode=combine(x@datamode, y@datamode),
		offset=combine(x@offset, y@offset),
		extent=combine(x@extent, y@extent))
})

setMethod("[", "atoms",
	function(x, i, ...) {
		groups <- x@group_id[]
		i2 <- lapply(i, function(g) which(groups == g))
		i <- unlist(i2)
		atoms(group_id=rep.int(seq_len(length(i2)), sapply(i2, length)),
			source_id=x@source_id[i],
			datamode=x@datamode[i],
			offset=x@offset[i],
			extent=x@extent[i])
})

setMethod("[[", "atoms",
	function(x, i, ...) {
		if ( length(i) > 1 )
			stop("attempt to select more than one element")
		x[i]
})

setMethod("c", "atoms", function(x, ..., recursive=FALSE)
{
	dots <- list(...)
	if ( length(dots) == 0 ) {
		x
	} else if ( length(dots) == 1 ) {
		combine(x, dots[[1]])
	} else {
		do.call(combine, list(x, ...))
	}
})

setMethod("show", "atoms", function(object) {
	print(data.frame(
		group_id=object@group_id[],
		source_id=object@source_id[],
		datamode=make_datamode(object@datamode[], type="C"),
		offset=object@offset[],
		extent=object@extent[],
		index_offset=object@index_offset[],
		index_extent=object@index_extent[]))
})

