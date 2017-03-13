

#### Define atoms class ####
## -------------------------

setClass("atoms",
	slots = c(
		length = "integer",
		source_id = "integer",
		datamode = "integer",
		offset = "numeric", # byte offset from start of file
		extent = "numeric", # number of elements
		index_offset = "numeric", # cumulative index of first element
		index_extent = "numeric"), # cumulative index of one-past-the-end
	validity = function(object) {
		errors <- NULL
		lens <- c(source_id=length(object@source_id),
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
		if ( object@length != unique(lens) )
			errors <- c(errors, "'length' not equal to length of object elements")
		if ( object@index_offset[1] != 0 )
			errors <- c(errors, "'index_offset' must begin at 0")
		# C_datamodes <- levels(make_datamode(type="C"))
		# if ( any(!as.character(object@datamode) %in% C_datamodes) )
		# 	errors <- c(errors, "'datamode' should be one of [",
		# 		paste(C_datamodes, collapse=", "), "]")
		extent <- object@index_extent - object@index_offset
		if ( any(extent != object@extent) )
			errors <- c(errors, "'index_offset' or 'index_extent' incongruent with 'extent'")
		index_offset.drop <-  object@index_offset[-1L]
		index_extent.drop <-  object@index_extent[-length(object@index_extent)]
		if ( any(index_offset.drop != index_extent.drop) )
			errors <- c(errors, "'index_offset' or 'index_extent' are non-contiguous")
		length <- sum(object@extent)
		if ( length != object@index_extent[length(object@index_extent)] )
			errors <- c(errors, "'index_extent' must terminate at sum of 'extent' [", length, "]")
		if ( is.null(errors) ) TRUE else errors
	})

atoms <- function(source_id = as.integer(NA), datamode="double",
					offset = numeric(1), extent = numeric(1))
{
	new("atoms",
		length=as.integer(length(source_id)),
		source_id=as.integer(source_id),
		datamode=as.integer(make_datamode(datamode, type="C")),
		offset=as.numeric(offset),
		extent=as.numeric(extent),
		index_offset=as.numeric(c(0, cumsum(extent)[-length(extent)])),
		index_extent=as.numeric(cumsum(extent)))
}

setMethod("datamode", "atoms", function(x) x@datamode)

setReplaceMethod("datamode", "atoms", function(x, value) {
	x@datamode <- as.integer(make_datamode(value, type="C"))
	x
})

setMethod("combine", "atoms", function(x, y, ...) {
	atoms(source_id=c(x@source_id, y@source_id),
		datamode=c(x@datamode, y@datamode),
		offset=c(x@offset, y@offset),
		extent=c(x@extent, y@extent))
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
		source_id=object@source_id,
		datamode=make_datamode(object@datamode, type="C"),
		offset=object@offset,
		extent=object@extent,
		index_offset=object@index_offset,
		index_extent=object@index_extent))
})

