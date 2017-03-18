

#### Define atoms class ####
## -------------------------

setClassUnion("integerORdrle", c("integer", "drle"))
setClassUnion("numericORdrle", c("numeric", "drle"))

setClass("atoms",
	slots = c(
		natoms = "integer",
		ngroups = "integer",
		group_id = "integerORdrle",
		source_id = "integerORdrle",
		datamode = "integerORdrle",
		offset = "numericORdrle", # byte offset from start of file
		extent = "numericORdrle", # number of elements
		index_offset = "numericORdrle", # cumulative index of first element
		index_extent = "numericORdrle"), # cumulative index of one-past-the-end
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
					..., compress = TRUE, compression_threshold = 0)
{
	if ( is.unsorted(group_id[]) ) {
		o <- order(group_id[])
		group_id <- group_id[o]
		source_id <- source_id[o]
		datamode <- datamode[o]
		offset <- offset[o]
		extent <- extent[o]
	}
	x <- .Call("C_createAtoms", group_id, source_id, datamode, offset, extent)
	if ( compress )
		x <- compressAtoms(x, compression_threshold=compression_threshold)
	x
}

compressAtoms <- function(x, compression_threshold = 0) {
	if ( length(x) > 1 ) {
		x@group_id <- drleCompress(x@group_id,
			compression_threshold=compression_threshold)
		x@source_id <- drleCompress(x@source_id,
			compression_threshold=compression_threshold)
		x@datamode <- drleCompress(x@datamode,
			compression_threshold=compression_threshold)
		x@offset <- drleCompress(x@offset,
			compression_threshold=compression_threshold)
		x@extent <- drleCompress(x@extent,
			compression_threshold=compression_threshold)
		x@index_offset <- drleCompress(x@index_offset,
			compression_threshold=compression_threshold)
		x@index_extent <- drleCompress(x@index_extent,
			compression_threshold=compression_threshold)
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

