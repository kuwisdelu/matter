
#### Chunk-Apply classes ####
## ---------------------------

setClass("chunked",
	slots = c(
		data = "ANY",
		index = "list",
		local = "logical"),
	contains = "VIRTUAL",
	validity = function(object) {
		errors <- NULL
		if ( is.null(object@data) )
			errors <- c(errors, "'data' must not be NULL")
		index_ok <- vapply(object@index, is.numeric, logical(1L))
		if ( !all(index_ok) )
			errors <- c(errors, "'index' must be a list of numeric vectors")
		if ( length(object@local) != 1L )
			errors <- c(errors, "'local' must be a scalar logical")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("chunked_vector", contains = "chunked")

setClass("chunked_array",
	slots = c(margin = "integer"),
	contains = "chunked")

setClass("chunked_list", contains = "chunked")

setAs("chunked", "list", function(from) from[])

setMethod("as.list", "chunked", function(x) as(x, "list"))

setMethod("describe_for_display", "chunked", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0(class(x@data)[1L], " chunks")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "chunked", function(x) preview_list(x))

setMethod("show", "chunked", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	n <- getOption("matter.show.head.n")
	if ( getOption("matter.show.head") )
		try(preview_for_display(object), silent=TRUE)
	cat("with", sum(lengths(object@index)), "total items\n")
})

setMethod("length", "chunked", function(x) length(x@index))

setMethod("lengths", "chunked", function(x) lengths(x@index))

setMethod("[",
	c(x = "chunked", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop = TRUE)
	{
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		i <- as_subscripts(i, x)
		if ( is_null_or_na(drop) ) {
			if ( is.null(i) ) {
				if ( is.null(dim(x@data)) ) {
					new(class(x),
						data=x@data,
						index=x@index[i],
						local=x@local)
				} else {
					new(class(x),
						data=x@data,
						index=x@index[i],
						local=x@local,
						margin=x@margin)
				}
			} else {
				x
			}
		} else {
			if ( is.null(i) )
				i <- seq_along(x)
			ans <- vector("list", length(i))
			for ( j in seq_along(i) )
				ans[[j]] <- x[[i[j]]]
			ans
		}
	})

chunked_vector <- function(x, nchunks = NA,
	local = FALSE, depends = NULL)
{
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	index <- chunkify(seq_along(x), nchunks=nchunks, depends=depends)
	new("chunked_vector", data=x,
		index=index, local=local)
}

chunked_matrix <- function(x, margin, nchunks = NA,
	local = FALSE, depends = NULL)
{
	if ( length(dim(x)) != 2L )
		stop("'x' must have exactly 2 dimensions")
	if ( !margin %in% c(1L, 2L) )
		stop("'margin' must be 1 or 2")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	index <- switch(margin,
		chunkify(seq_len(nrow(x)), nchunks=nchunks, depends=depends),
		chunkify(seq_len(ncol(x)), nchunks=nchunks, depends=depends))
	new("chunked_array", data=x, margin=margin,
		index=index, local=local)
}

chunked_list <- function(..., nchunks = NA,
	local = FALSE, depends = NULL)
{
	xs <- list(...)
	if ( length(xs) > 1L ) {
		len <- vapply(xs, length, integer(1L))
		if ( n_unique(len) != 1L ) {
			max.len <- max(len)
			if ( max.len && any(len == 0L) )
				stop("zero-length and non-zero length inputs cannot be mixed")
			if ( any(max.len %% len) )
				warning("longer argument not a multiple of length of vector")
			xs <- lapply(xs, rep_len, length.out=max.len)
		}
	}
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	index <- chunkify(seq_along(xs[[1L]]), nchunks=nchunks, depends=depends)
	new("chunked_list", data=xs,
		index=index, local=local)
}

setMethod("[[", c(x = "chunked_list"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		if ( isTRUE(x@local) && is.matter(x@data) ) {
			y <- lapply(x@data, `[`, x@index[[i]], drop=NULL)
		} else {
			y <- lapply(x@data, `[`, x@index[[i]], drop=FALSE)
		}
		y <- set_attr(y, attributes(x@index[[i]]))
		y
	})

setMethod("[[", c(x = "chunked_vector"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		if ( is.matter(x@data) && isTRUE(x@local) ) {
			y <- x@data[x@index[[i]],drop=NULL]
		} else {
			y <- x@data[x@index[[i]],drop=FALSE]
		}
		y <- set_attr(y, attributes(x@index[[i]]))
		y
	})

setMethod("[[", c(x = "chunked_array"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		if ( isTRUE(x@local) && is.matter(x@data) ) {
			y <- switch(x@margin,
				x@data[x@index[[i]],,drop=NULL],
				x@data[,x@index[[i]],drop=NULL])
		} else {
			y <- switch(x@margin,
				x@data[x@index[[i]],,drop=FALSE],
				x@data[,x@index[[i]],drop=FALSE])
		}
		y <- set_attr(y, attributes(x@index[[i]]))
		attr(y, "margin") <- x@margin
		y
	})

chunkify <- function(x, nchunks = 20L, depends = NULL) {
	if ( !is.null(depends) && length(depends) != length(x) )
		stop("length of 'depends' must match extent of 'x'")
	nchunks <- min(ceiling(length(x) / 2L), nchunks)
	index <- seq_along(x)
	if ( nchunks > 1L ) {
		index <- split(index, cut(index, nchunks))
	} else {
		index <- list(index)
	}
	ans <- vector("list", length(index))
	for ( i in seq_along(index) )
	{
		if ( !is.null(depends) ) {
			di <- depends[index[[i]]]
			ind <- c(index[[i]], unlist(di))
			ind <- sort(unique(ind))
			if ( any(ind < 1L | ind > length(x)) )
				stop("'depends' subscript out of bounds")
			dep <- lapply(di, match, ind)
			dep <- dep[match(ind, index[[i]])]
		} else {
			ind <- index[[i]]
			dep <- NULL
		}
		n <- length(index[[i]])
		ans[[i]] <- x[ind]
		attr(ans[[i]], "index") <- c(ind)
		attr(ans[[i]], "depends") <- c(dep)
		attr(ans[[i]], "chunkid") <- i
		attr(ans[[i]], "chunksize") <- n
		attr(ans[[i]], "nchunks") <- nchunks
	}
	names(ans) <- names(x)
	ans
}
