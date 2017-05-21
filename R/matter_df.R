
#### Define matter<data-frame> class for creating data frames ####
## --------------------------------------------------------------

setClass("matter_df",
	prototype = prototype(
		data = list(),
		datamode = make_datamode("list", type="R"),
		paths = character(),
		filemode = "rb",
		chunksize = 1L,
		length = 0,
		dim = 0L,
		names = NULL,
		dimnames = NULL,
		ops = NULL),
	contains = "matter",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@names) )
			errors <- c(errors, "array must have non-NULL 'names'")
		if ( object@length != length(object@data) )
			errors <- c(errors, paste0("length of object [", object@length,
				"] does not match length of data [", length(object@data), "]"))
		if ( any(names(object@data) != object@names) )
			errors <- c(errors, "'names' must match names of data columns")
		if ( any(object@names != object@dimnames[[2]]) )
			errors <- c(errors, "'names' and column names do not match")
		lens <- sapply(object@data, length)
		neq <- which(lens != object@dim[1])
		if ( length(neq) > 0 ) {
			errors <- c(errors, paste0("length of '", object@names[neq[1]], " [",
				lens[neq[1]], "] does not match number of rows [", object@dim[1], "]"))
		}
		atms <- sapply(object@data, function(x) is.matter(x) || is.atomic(x))
		if ( any(!atms) )
			errors <- c(errors, "columns must be matter objects or atomic vectors")
		if ( is.null(errors) ) TRUE else errors
	})

matter_df <- function(..., row.names = NULL) {
	data <- list(...)
	if ( is.null(names(data)) ) {
		vars <- paste0("Var", seq_along(data))
	} else {
		vars <- make.unique(make.names(names(data)))
	}
	new("matter_df",
		data=setNames(data, vars),
		datamode=make_datamode("list", type="R"),
		paths=character(),
		filemode="rb",
		length=length(data),
		dim=c(length(data[[1]]), length(data)),
		names=vars,
		dimnames=list(row.names, vars),
		ops=NULL)
}

setMethod("head", "matter_df",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    n <- if (n < 0L) 
	        max(nrow(x) + n, 0L)
	    else min(n, nrow(x))
	    x[seq_len(n),,drop=FALSE]
})

setMethod("tail", "matter_df",
	function(x, n = 6L, ...) {
		stopifnot(length(n) == 1L)
	    nrx <- nrow(x)
	    n <- if (n < 0L) 
	        max(nrx + n, 0L)
	    else min(n, nrx)
	    x[seq.int(to=nrx, length.out=n),,drop=FALSE]
})

setMethod("show", "matter_df", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		"data frame", "\n", sep="")
	m <- sum(sapply(atomdata(object), is.matter))
	cat("    ", length(object) - m, " variables in-memory\n", sep="")
	cat("    ",  m, " variables on-disk\n", sep="")
	# n <- 6L
	# print(head(object, n=n))
	# if ( nrow(object) > n )
	# 	cat("and ", nrow(object) - n, " more rows", "\n", sep="")
})

setAs("data.frame", "matter_df",
	function(from) {
		from <- lapply(from, function(x) {
			if ( is.character(x) ) {
				matter_str(x)
			} else {
				matter_vec(x)
			}
		})
		do.call("matter_df", c(from, row.names=row.names(from)))
})

setAs("data.frame", "matter_df",
	function(from) {
		from <- lapply(from, as.matter)
		do.call("matter_df", from)
})

as.matter_df <- function(x) as(x, "matter_df")

setReplaceMethod("names", "matter_df", function(x, value) {
	x@names <- value
	if ( is.null(x@dimnames) ) {
		x@dimnames <- list(NULL, value)
	} else {
		x@dimnames[[2]] <- value
	}
	if ( validObject(x) )
		x
})

setReplaceMethod("dimnames", "matter_df", function(x, value) {
	x@names <- value[[2]]
	x@dimnames <- value[[2]]
	if ( validObject(x) )
		x
})

getDataFrame <- function(x) {
	y <- lapply(atomdata(x), "[")
	data.frame(setNames(y, names(x)),
		row.names=rownames(x),
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

getDataFrameRows <- function(x, i, drop) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	y <- lapply(names(x), function(nm) atomdata(x)[[nm]][i])
	if ( drop && ncol(x) == 1 ) {
		y[[1]]
	} else {
		data.frame(setNames(y, names(x)),
			row.names=rownames(x),
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

getDataFrameColumns <- function(x, j, drop) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	if ( drop && length(j) == 1 ) {
		atomdata(x)[[j]]
	} else {
		new("matter_df",
			data=atomdata(x)[j],
			datamode=datamode(x),
			paths=paths(x),
			filemode=filemode(x),
			length=length(j),
			dim=c(nrow(x), length(j)),
			names=names(x)[j],
			dimnames=c(rownames(x), names(x)[j]),
			ops=NULL)
	}
}

setDataFrameColumns <- function(x, j, value) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	for ( j2 in j )
		atomdata(x)[[j2]][] <- value[[j2]]
	if ( validObject(x) )
		invisible(x)
}

getDataFrameElements <- function(x, i, j, drop) {
	if ( is.logical(i) )
		i <- logical2index(x, i, 1)
	if ( is.character(i) )
		i <- dimnames2index(x, i, 1)
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	y <- lapply(j, function(nm) atomdata(x)[[nm]][i])
	if ( drop && length(j) == 1 ) {
		y[[1]]
	} else {
		data.frame(setNames(y, names(x)[j]),
			row.names=rownames(x),
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

# data frame getter methods

setMethod("[",
	c(x = "matter_df", i = "missing", j = "missing"),
	function(x, ...) getDataFrame(x))

setMethod("[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., drop = TRUE) getDataFrameRows(x, i, drop))

setMethod("[",
	c(x = "matter_df", i = "missing"),
	function(x, j, ..., drop = TRUE) getDataFrameColumns(x, j, drop))

setMethod("[",
	c(x = "matter_df"),
	function(x, i, j, ..., drop = TRUE) getDataFrameElements(x, i, j, drop))

setMethod("[[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ...) getDataFrameColumns(x, i, drop=TRUE))

setMethod("$",
	c(x = "matter_df"),
	function(x, name) getDataFrameColumns(x, name, drop=TRUE))

# data frame setter methods

setReplaceMethod("[",
	c(x = "matter_df", i = "missing", j = "missing"),
	function(x, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrame(x)
})

setReplaceMethod("[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameRows(x, i, value)
})

setReplaceMethod("[",
	c(x = "matter_df", i = "missing"),
	function(x, j, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameColumns(x, j, value)
})

setReplaceMethod("[",
	c(x = "matter_df"),
	function(x, i, j, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameElements(x, i, j, value)
})

setReplaceMethod("[[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., value) {
		atomdata(x)[[i]] <- value
		if ( validObject(x) )
			x
})

setReplaceMethod("$",
	c(x = "matter_df"),
	function(x, name, value) {
		atomdata(x)[[name]] <- value
		if ( validObject(x) )
			x
})


