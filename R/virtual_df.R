
#### Define matter<data frame> class for creating data frames ####
## --------------------------------------------------------------

setClass("virtual_df",
	prototype = prototype(
		data = list(),
		datamode = make_datamode("virtual", type="R"),
		paths = character(),
		filemode = make_filemode(),
		chunksize = 1e6L,
		length = 0,
		dim = c(0L, 0L),
		names = character(),
		dimnames = list(NULL, character()),
		ops = NULL),
	contains = c("matter_vt", "virtual_tbl"),
	validity = function(object) {
		errors <- NULL
		if ( object@length != length(object@data) )
			errors <- c(errors, paste0("length of object [", object@length,
				"] does not match length of data [", length(object@data), "]"))
		if ( is.null(names(object@data)) && object@length > 0 )
			errors <- c(errors, "elements of 'data' must be named")
		if ( any(names(object@data) != object@names) )
			errors <- c(errors, "'names' must match names of data columns")
		if ( !identical(object@names, object@dimnames[[2]]) )
			errors <- c(errors, "'names' must match 'dimnames[[2]]'")
		lens <- sapply(object@data, length)
		neq <- which(lens != object@dim[1])
		if ( length(neq) > 0 ) {
			errors <- c(errors, paste0("length of '", object@names[neq[1]], " [",
				lens[neq[1]], "] does not match number of rows [", object@dim[1], "]"))
		}
		if ( is.null(errors) ) TRUE else errors
	})

setClass("matter_df",
	contains = "virtual_df",
	validity = function(object) {
		errors <- NULL
		atms <- sapply(object@data, function(x) is.matter(x) )
		if ( any(!atms) )
			errors <- c(errors, "columns must be matter objects")
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("describe_for_display", "matter_df", function(x) {
	desc1 <- paste0("<", x@dim[[1]], " row, ", x@dim[[2]], " column> ", class(x))
	desc2 <- paste0("out-of-memory data frame")
	paste0(desc1, " :: ", desc2)
})

virtual_df <- function(..., row.names = NULL, stringsAsFactors = FALSE)
{
	data <- list(...)
	nm <- names(data)
	if ( length(data) == 1 && is.list(data[[1]]) ) {
		args <- list(row.names=row.names, stringsAsFactors=stringsAsFactors)
		return(do.call("virtual_df", c(data[[1]], args)))
	}
	if ( is.null(nm) || any(sapply(nm, nchar) == 0) )
		stop("all arguments must be named")
	nm <- make.unique(nm)
	if ( stringsAsFactors )
		data <- lapply(data, stringsToFactors)
	new("virtual_df",
		data=set_names(data, nm),
		datamode=make_datamode("virtual", type="R"),
		paths=character(),
		filemode=make_filemode(),
		length=length(data),
		dim=c(length(data[[1]]), length(data)),
		names=nm,
		dimnames=list(row.names, nm),
		ops=NULL)
}

matter_df <- function(..., row.names = NULL, stringsAsFactors = FALSE)
{
	data <- list(...)
	args <- list(row.names=row.names, stringsAsFactors=stringsAsFactors)
	if ( length(data) == 1 && is.list(data[[1]]) )
		data <- data[[1]]
	if ( stringsAsFactors )
		data <- lapply(data, stringsToFactors)
	data <- lapply(data, as.matter)
	out <- do.call("virtual_df", c(data, args))
	as(out, "matter_df")
}

setAs("list", "virtual_df", # also works for data.frame
	function(from) {
		do.call("virtual_df", from)
})

setAs("list", "matter_df", # also works for data.frame
	function(from) {
		from <- lapply(from, as.matter) # coerce columns to matter
		do.call("matter_df", from)
})

setAs("virtual_df", "matter_df",
	function(from) {
		data <- lapply(atomdata(from), as.matter) # coerce columns to matter
		atomdata(from) <- data
		class(from) <- "matter_df"
		if ( validObject(from) )
			from
})

as.virtual_df <- function(x) as(x, "virtual_df")

as.matter_df <- function(x) as(x, "matter_df")

getDataFrame <- function(x) {
	y <- lapply(atomdata(x), "[")
	data.frame(set_names(y, names(x)),
		row.names=rownames(x),
		check.names=FALSE,
		stringsAsFactors=FALSE)
}

setDataFrame <- function(x, value) {
	for ( i in seq_along(value) )
		x[[i]] <- value[[i]]
	for ( i in seq_along(value) )
		x[[i]] <- value[[i]]
	if ( validObject(x) )
		invisible(x)
}

getDataFrameRows <- function(x, i, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- lapply(names(x), function(nm) atomdata(x)[[nm]][i])
	if ( drop && length(y) == 1 ) {
		y[[1]]
	} else {
		data.frame(set_names(y, names(x)),
			row.names=rownames(x)[i],
			check.names=FALSE,
			stringsAsFactors=FALSE)
	}
}

setDataFrameRows <- function(x, i, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	for ( j in seq_len(ncol(x)) )
		atomdata(x)[[j]][i] <- value[i,j]
	if ( validObject(x) )
		invisible(x)
}

getDataFrameCols <- function(x, j, drop=TRUE) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	y <- lapply(j, function(jj) atomdata(x)[[jj]][])
	if ( drop && length(y) == 1 ) {
		y[[1]]
	} else {
		data.frame(set_names(y, names(x)[j]),
			row.names=rownames(x),
			check.names=FALSE,
			stringsAsFactors=FALSE)
	}
}

setDataFrameCols <- function(x, j, value) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	for ( j2 in j )
		atomdata(x)[[j2]][] <- value[[j2]]
	if ( validObject(x) )
		invisible(x)
}

getDataFrameElements <- function(x, i, j, drop=TRUE) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	y <- lapply(j, function(jj) atomdata(x)[[jj]][i])
	if ( drop && length(j) == 1 ) {
		y[[1]]
	} else {
		data.frame(set_names(y, names(x)[j]),
			row.names=rownames(x)[i],
			check.names=FALSE,
			stringsAsFactors=FALSE)
	}
}

setDataFrameElements <- function(x, i, j, value) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	for ( j2 in j )
		atomdata(x)[[j2]][i] <- value[[j2]][i]
	if ( validObject(x) )
		invisible(x)
}

subDataFrameRows <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- lapply(names(x), function(nm) atomdata(x)[[nm]][i,drop=NULL])
	new(class(x),
		data=set_names(y, names(x)),
		datamode=datamode(x),
		paths=paths(x),
		filemode=filemode(x),
		length=length(x),
		dim=c(length(i), ncol(x)),
		names=names(x),
		dimnames=list(rownames(x)[i], names(x)),
		ops=NULL)
}

subDataFrameCols <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- atomdata(x)[j]
	new(class(x),
		data=set_names(y, names(x)[j]),
		datamode=datamode(x),
		paths=paths(x),
		filemode=filemode(x),
		length=length(j),
		dim=c(nrow(x), length(j)),
		names=names(x)[j],
		dimnames=list(rownames(x), names(x)[j]),
		ops=NULL)
}

subDataFrameElements <- function(x, i, j) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	y <- lapply(j, function(jj) atomdata(x)[[jj]][i,drop=NULL])
	new(class(x),
		data=set_names(y, names(x)[j]),
		datamode=datamode(x),
		paths=paths(x),
		filemode=filemode(x),
		length=length(j),
		dim=c(length(i), length(j)),
		names=names(x)[j],
		dimnames=list(rownames(x)[i], names(x)[j]),
		ops=NULL)
}

# data frame getter methods

setMethod("[",
	c(x = "virtual_df", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, j, ..., drop) {
		narg <- nargs() - 1 - !missing(drop)
		if ( !missing(i) && narg == 1 ) {
			# linear indexing
			return(subDataFrameCols(x, i))
		}
		if ( narg > 1 && narg != length(dim(x)) )
			stop("incorrect number of dimensions")
		if ( !missing(i) && !missing(j) ) {
			getDataFrameElements(x, i, j, drop)
		} else if ( !missing(i) ) {
			getDataFrameRows(x, i, drop)
		} else if ( !missing(j) ) {
			getDataFrameCols(x, j, drop)
		} else {
			getDataFrame(x)
		}
	})

setMethod("[",
	c(x = "virtual_df", i = "ANY", j = "ANY", drop = "NULL"),
	function(x, i, j, ..., drop) {
		narg <- nargs() - 1 - !missing(drop)
		if ( !missing(i) && narg == 1 ) {
			# linear indexing
			return(subDataFrameCols(x, i))
		}
		if ( narg > 1 && narg != length(dim(x)) )
			stop("incorrect number of dimensions")
		if ( !missing(i) && !missing(j) ) {
			subDataFrameElements(x, i, j)
		} else if ( !missing(i) ) {
			subDataFrameRows(x, i)
		} else if ( !missing(j) ) {
			subDataFrameCols(x, j)
		} else {
			x
		}
	})

# data frame setter methods

setReplaceMethod("[",
	c(x = "virtual_df", i = "ANY", j = "ANY", value = "ANY"),
	function(x, i, j, ..., value) {
		narg <- nargs() - 2
		if ( !is.list(value) )
			value <- list(value)
		if ( !missing(i) && narg == 1 ) {
			# linear indexing
			return(setDataFrameCols(x, i, value))
		}
		if ( narg > 1 && narg != length(dim(x)) )
			stop("incorrect number of dimensions")
		if ( !missing(i) && !missing(j) ) {
			setDataFrameElements(x, i, j, value)
		} else if ( !missing(i) ) {
			setDataFrameRows(x, i, value)
		} else if ( !missing(j) ) {
			setDataFrameCols(x, j, value)
		} else {
			setDataFrame(x, value)
		}
	})

# x[[i]] subsetting

setMethod("[[",
	c(x = "virtual_df", j = "missing"),
	function(x, i, ...) atomdata(x)[[i]])

setReplaceMethod("[[",
	c(x = "virtual_df", j = "missing"),
	function(x, i, ..., value) {
		atomdata(x)[[i]] <- value
		if ( validObject(x) )
			x
})

# x$name subsetting

setMethod("$",
	c(x = "virtual_df"),
	function(x, name) atomdata(x)[[name, exact=FALSE]])

setReplaceMethod("$",
	c(x = "virtual_df"),
	function(x, name, value) {
		atomdata(x)[[name]] <- value
		if ( validObject(x) )
			x
})

# combine by rows

setMethod("combine_by_rows", c("virtual_df", "virtual_df"),
	function(x, y, ...)
{
	if ( ncol(x) != ncol(y) )
		stop("number of columns of data frames must match")
	data <- bind_elements(x@data, y@data)
	dmn <- combine_rownames(x,y)
	new(class(x),
		data=data,
		datamode=x@datamode,
		paths=x@paths,
		filemode=x@filemode,
		length=x@length,
		dim=c(x@dim[1] + y@dim[1], x@dim[2]),
		names=dmn[[2]],
		dimnames=dmn,
		ops=NULL)
})

setMethod("combine_by_rows", c("virtual_df", "data.frame"),
	function(x, y, ...)	combine_by_rows(x, as(y, "virtual_df")))

setMethod("combine_by_rows", c("data.frame", "virtual_df"),
	function(x, y, ...)	combine_by_rows(as(x, "virtual_df"), y))

# combine by cols

setMethod("combine_by_cols", c("virtual_df", "virtual_df"),
	function(x, y, ...)
{
	if ( nrow(x) != nrow(y) )
		stop("number of rows of data frames must match")
	data <- c(x@data, y@data)
	dmn <- combine_colnames(x,y)
	new(class(x),
		data=data,
		datamode=x@datamode,
		paths=x@paths,
		filemode=x@filemode,
		length=x@length + y@length,
		dim=c(x@dim[1], x@dim[2] + y@dim[2]),
		names=dmn[[2]],
		dimnames=dmn,
		ops=NULL)
})

setMethod("combine_by_cols", c("virtual_df", "data.frame"),
	function(x, y, ...)	combine_by_cols(x, as(y, "virtual_df")))

setMethod("combine_by_cols", c("data.frame", "virtual_df"),
	function(x, y, ...)	combine_by_cols(as(x, "virtual_df"), y))


