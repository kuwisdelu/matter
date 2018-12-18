

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
		group_id <- object@group_id[]
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
		if ( object@ngroups != max(group_id) )
			errors <- c(errors, "'ngroups' not equal to the number of groups")
		if ( is.unsorted(group_id) || any(group_id <= 0) )
			errors <- c(errors, "'group_id' must be positive and increasing")
		if ( object@index_offset[1] != 0 )
			errors <- c(errors, "'index_offset' must begin at 0")
		if ( object@offset[object@natoms] < 0 )
			errors <- c(errors, "'offset' contains negative indices")
		if ( object@extent[object@natoms] < 0 )
			errors <- c(errors, "'extent' contains negative indices")
		if ( object@index_offset[object@natoms] < 0 )
			errors <- c(errors, "'index_offset' contains negative indices")
		if ( object@index_extent[object@natoms] < 0 )
			errors <- c(errors, "'index_extent' contains negative indices")
		if ( is.null(errors) ) TRUE else errors
	})

atoms <- function(group_id = 1L, source_id = as.integer(NA),
					datamode=make_datamode("double", type="C"),
					offset = numeric(1), extent = numeric(1),
					..., compress = TRUE, cr_threshold = 3)
{
	n <- max(length(group_id), length(source_id),
		length(datamode), length(offset), length(extent))
	if ( length(group_id) < n )
		group_id <- rep(group_id[], length.out=n)
	if ( length(source_id) < n )
		source_id <- rep(source_id[], length.out=n)
	if ( length(datamode) < n )
		datamode <- rep(datamode[], length.out=n)
	if ( length(offset) < n )
		offset <- rep(offset[], length.out=n)
	if ( length(extent) < n )
		extent <- rep(extent[], length.out=n)
	if ( !(is.integer(datamode) || is.drle(datamode)) )
		datamode <- as.integer(datamode)
	if ( is.unsorted(group_id[]) ) {
		o <- order(group_id[])
		group_id <- group_id[o]
		source_id <- source_id[o]
		datamode <- datamode[o]
		offset <- offset[o]
		extent <- extent[o]
	}
	x <- .Call("C_createAtoms", group_id, source_id,
		datamode, offset, extent, PACKAGE="matter")
	if ( compress && x@natoms > 1 ) {
		x@group_id <- drle(x@group_id, cr_threshold=cr_threshold)
		x@source_id <- drle(x@source_id, cr_threshold=cr_threshold)
		x@datamode <- drle(x@datamode, cr_threshold=cr_threshold)
		x@offset <- drle(x@offset, cr_threshold=cr_threshold)
		x@extent <- drle(x@extent, cr_threshold=cr_threshold)
		x@index_offset <- drle(x@index_offset, cr_threshold=cr_threshold)
		x@index_extent <- drle(x@index_extent, cr_threshold=cr_threshold)
	}
	if ( validObject(x) )
		x
}

subset_atoms_by_index_offset <- function(x, i) {
	if ( anyNA(i) )
		stop("NAs not allowed when subsetting atoms")
	il <- as.list(drle(i))
	io <- x@index_offset[] + 1
	ie <- x@index_extent[]
	y <- mapply(function(val, len, del) {
		if ( del == 1 ) {
			lo <- val
			hi <- val + (len - 1) * del
			wh <- lo <= ie & io <= hi
			if ( !any(wh) )
				stop("subscript out of bounds")
			new_io <- pmax(lo, io[wh])
			new_ie <- pmin(hi, ie[wh])
			datamode <- x@datamode[wh]
			byte_offset <- (new_io - io[wh]) * sizeof(datamode)
			offset <- x@offset[wh] + byte_offset
			extent <- new_ie - new_io + 1
			data.frame(
				group_id=x@group_id[wh],
				source_id=x@source_id[wh],
				datamode=datamode,
				offset=offset,
				extent=extent)
		} else {
			ii <- seq(from=val, by=del, length.out=len)
			yi <- lapply(ii, function(k) {
				wh <- io <= k & k <= ie
				if ( !any(wh) )
					stop("subscript out of bounds")
				new_io <- pmax(k, io[wh])
				datamode <- x@datamode[wh]
				byte_offset <- (new_io - io[wh]) * sizeof(datamode)
				offset <- x@offset[wh] + byte_offset
				extent <- rep(1, length.out=sum(wh))
				data.frame(
					group_id=x@group_id[wh],
					source_id=x@source_id[wh],
					datamode=datamode,
					offset=offset,
					extent=extent)
			})
			do.call("rbind", yi)
		}
	}, il$values, il$lengths, il$deltas, SIMPLIFY=FALSE)
	y <- do.call("rbind", y)
	atoms(
		group_id=y$group_id,
		source_id=y$source_id,
		datamode=y$datamode,
		offset=y$offset,
		extent=y$extent)
}

drop_groups_from_atoms <- function(x) {
	atoms(group_id=rep(1L, x@natoms),
		source_id=x@source_id,
		datamode=x@datamode,
		offset=x@offset,
		extent=x@extent)
}

combine_atoms <- function(x, y, x.paths, y.paths, new.groups = FALSE) {
	if ( new.groups )
		y@group_id <- y@group_id[] + max(x@group_id[])
	if ( !missing(x.paths) && !missing(y.paths) ) {
		paths <- levels(factor(c(x.paths, y.paths)))
		x@source_id <- as.integer(factor(x.paths[x@source_id[]],
			levels=paths))
		y@source_id <- as.integer(factor(y.paths[y@source_id[]],
			levels=paths))
	}
	atoms(group_id=combine(x@group_id, y@group_id),
		source_id=combine(x@source_id, y@source_id),
		datamode=combine(x@datamode, y@datamode),
		offset=combine(x@offset, y@offset),
		extent=combine(x@extent, y@extent))
}

setMethod("as.data.frame", "atoms", function(x, ...){
	adata <- data.frame(
		group_id=x@group_id[],
		source_id=x@source_id[],
		datamode=make_datamode(x@datamode[], type="C"),
		offset=x@offset[],
		extent=x@extent[],
		index_offset=x@index_offset[],
		index_extent=x@index_extent[])	
})

setMethod("as.list", "atoms", function(x, ...)
	as.list(as.data.frame(x, ...)))

setMethod("dim", "atoms", function(x) c(x@natoms, x@ngroups))

setMethod("length", "atoms", function(x) x@ngroups)

setMethod("datamode", "atoms", function(x) x@datamode)

setMethod("combine", "atoms", function(x, y, ...)
	combine_atoms(x, y, new.groups=FALSE))

setMethod("[", c(x="atoms", j="missing"),
	function(x, i, ...) {
		if ( anyNA(i) )
			stop("NAs not allowed when subsetting atoms")
		atoms(group_id=x@group_id[i],
			source_id=x@source_id[i],
			datamode=x@datamode[i],
			offset=x@offset[i],
			extent=x@extent[i])
})

setMethod("[", c(x="atoms", i="missing"),
	function(x, j, ...) {
		if ( anyNA(j) )
			stop("NAs not allowed when subsetting atoms")
		groups <- x@group_id[]
		j2 <- lapply(j, function(g) which(groups == g))
		j <- unlist(j2)
		g <- seq_len(length(j2))
		times <- lengths(j2)
		if ( any(times == 0) )
			stop("subscript out of bounds")
		atoms(group_id=rep.int(g, times),
			source_id=x@source_id[j],
			datamode=x@datamode[j],
			offset=x@offset[j],
			extent=x@extent[j])
})

setMethod("[", c(x="atoms"),
	function(x, i, j, ...) {
		x[i,][,j]
})

setMethod("[[", "atoms",
	function(x, i, ...) {
		if ( length(i) > 1 )
			stop("attempt to select more than one element")
		x[,i]
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
	n <- 10L
	adata <- as.data.frame(object)
	print(head(adata, n=n))
	if ( nrow(adata) > n )
		cat("and", nrow(adata) - n, "more atoms\n")
})

