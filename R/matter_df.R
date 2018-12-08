
#### Define matter<data frame> class for creating data frames ####
## --------------------------------------------------------------

setClass("matter_df",
	prototype = prototype(
		data = list(),
		datamode = make_datamode("virtual", type="R"),
		paths = character(),
		filemode = "",
		chunksize = 1e6L,
		length = 0,
		dim = c(0L, 0L),
		names = character(),
		dimnames = list(NULL, character()),
		ops = NULL),
	contains = c("matter_vt", "matter_tbl"),
	validity = function(object) {
		errors <- NULL
		if ( object@length != length(object@data) )
			errors <- c(errors, paste0("length of object [", object@length,
				"] does not match length of data [", length(object@data), "]"))
		if ( is.null(names(object@data)) && object@length > 0 )
			errors <- c(errors, "elements of 'data' must be named")
		if ( any(names(object@data) != object@names) )
			errors <- c(errors, "'names' must match names of data columns")
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
	nm <- names(data)
	if ( length(data) == 1 && is.list(data[[1]]) )
		return(as.matter_df(data[[1]]))
	if ( is.null(nm) || any(sapply(nm, nchar) == 0) )
		stop("all arguments must be named")
	nm <- make.unique(nm)
	new("matter_df",
		data=setNames(data, nm),
		datamode=make_datamode("virtual", type="R"),
		paths=character(),
		filemode="",
		length=length(data),
		dim=c(length(data[[1]]), length(data)),
		names=nm,
		dimnames=list(row.names, nm),
		ops=NULL)
}

setMethod("describe_for_display", "matter_df", function(x) "data frame")

setMethod("show", "matter_df", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	cat("  <", object@dim[[1]], " row, ", object@dim[[2]], " column> ",
		describe_for_display(object), "\n", sep="")
	m <- sum(sapply(atomdata(object), is.matter))
	object.memory <- num_bytes(object.size(object))
	cat("    ", format(object.memory, units="auto"), " real memory: ",
		length(object) - m, " variables\n", sep="")
	cat("    ", format(vm_used(object), units="auto"), " virtual memory: ",
		m, " variables\n", sep="")
	cat("\n")
	classinfo <- sapply(atomdata(object), function(x)
		paste0("<", class(x)[1], ">"), USE.NAMES=FALSE)
	print_tabular_data(object, classinfo)
})

setAs("list", "matter_df", # also for data.frame
	function(from) {
		from <- lapply(from, as.matter)
		do.call("matter_df", from)
})

as.matter_df <- function(x) as(x, "matter_df")

setAs("matter_df", "data.frame", function(from) from[])

setMethod("as.data.frame", "matter_df", function(x) as(x, "data.frame"))

getDataFrame <- function(x) {
	y <- lapply(atomdata(x), "[")
	data.frame(setNames(y, names(x)),
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
		data.frame(setNames(y, names(x)),
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

getDataFrameColumns <- function(x, j, drop=TRUE) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)	
	y <- lapply(j, function(jj) atomdata(x)[[jj]][])
	if ( drop && length(y) == 1 ) {
		y[[1]]
	} else {
		data.frame(setNames(y, names(x)[j]),
			row.names=rownames(x),
			check.names=FALSE,
			stringsAsFactors=FALSE)
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
		data.frame(setNames(y, names(x)[j]),
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
	new("matter_df",
		data=setNames(y, names(x)),
		datamode=datamode(x),
		paths=paths(x),
		filemode=filemode(x),
		length=length(x),
		dim=c(length(i), ncol(x)),
		names=names(x),
		dimnames=list(rownames(x)[i], names(x)),
		ops=NULL)
}

subDataFrameColumns <- function(x, j) {
	if ( is.logical(j) )
		j <- logical2index(x, j, 2)
	if ( is.character(j) )
		j <- dimnames2index(x, j, 2)
	y <- atomdata(x)[j]
	new("matter_df",
		data=setNames(y, names(x)[j]),
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
	new("matter_df",
		data=setNames(y, names(x)[j]),
		datamode=datamode(x),
		paths=paths(x),
		filemode=filemode(x),
		length=length(j),
		dim=c(length(i), length(j)),
		names=names(x)[j],
		dimnames=list(rownames(x)[i], names(x)[j]),
		ops=NULL)
}

# x[] subsetting

setMethod("[",
	c(x = "matter_df", i = "missing", j = "missing"),
	function(x, ...) getDataFrame(x))

setReplaceMethod("[",
	c(x = "matter_df", i = "missing", j = "missing"),
	function(x, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrame(x)
})

# x[i,] subsetting

setMethod("[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., drop) getDataFrameRows(x, i, drop))

setMethod("[",
	c(x = "matter_df", j = "missing", drop = "NULL"),
	function(x, i, ..., drop) subDataFrameRows(x, i))

setReplaceMethod("[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameRows(x, i, value)
})

# x[,j] subsetting

setMethod("[",
	c(x = "matter_df", i = "missing"),
	function(x, j, ..., drop) getDataFrameColumns(x, j, drop))

setMethod("[",
	c(x = "matter_df", i = "missing", drop = "NULL"),
	function(x, j, ..., drop) subDataFrameColumns(x, j))

setReplaceMethod("[",
	c(x = "matter_df", i = "missing"),
	function(x, j, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameColumns(x, j, value)
})

# x[i,j] subsetting

setMethod("[",
	c(x = "matter_df"),
	function(x, i, j, ..., drop) getDataFrameElements(x, i, j, drop))

setMethod("[",
	c(x = "matter_df", drop = "NULL"),
	function(x, i, j, ..., drop) subDataFrameElements(x, i, j))

setReplaceMethod("[",
	c(x = "matter_df"),
	function(x, i, j, ..., value) {
		if ( !is.list(value) )
			value <- list(value)
		setDataFrameElements(x, i, j, value)
})

# x[[i]] subsetting

setMethod("[[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ...) getDataFrameColumns(x, i, drop=TRUE))

setReplaceMethod("[[",
	c(x = "matter_df", j = "missing"),
	function(x, i, ..., value) {
		atomdata(x)[[i]] <- value
		if ( validObject(x) )
			x
})

# x$name subsetting

setMethod("$",
	c(x = "matter_df"),
	function(x, name) getDataFrameColumns(x, name, drop=TRUE))

setReplaceMethod("$",
	c(x = "matter_df"),
	function(x, name, value) {
		atomdata(x)[[name]] <- value
		if ( validObject(x) )
			x
})


