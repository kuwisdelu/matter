
#### Chunk-Apply functions over matrices ####
## -------------------------------------------

chunkApply <- function(X, MARGIN, FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( !MARGIN %in% c(1L, 2L) )
		stop("MARGIN must be 1 or 2")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- chunk_writer(pid, outpath)
	} else {
		put <- NULL
	}
	CHUNKFUN <- chunk_loop_fun(FUN, type="array", margin=MARGIN, put=put)
	if ( MARGIN == 1L ) {
		ans.list <- chunk_rowapply(X, CHUNKFUN, ...,
			verbose=verbose, BPPARAM=BPPARAM)
		names(ans.list) <- dimnames(X)[[1L]]
	} else {
		ans.list <- chunk_colapply(X, CHUNKFUN, ...,
			verbose=verbose, BPPARAM=BPPARAM)
		names(ans.list) <- dimnames(X)[[2L]]
	}
	if ( outfile )
		ipcremove(pid)
	if ( outfile && isTRUE(simplify) ) {
		ans.list <- simplify2matter(ans.list)
	} else if ( is.function(simplify) || is.character(simplify) ) {
		ans.list <- match.fun(simplify)(ans.list)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	}
	ans.list
}

chunk_apply <- function(X, MARGIN, FUN, ...)
{
	if ( !MARGIN %in% c(1L, 2L) )
		stop("MARGIN must be 1 or 2")
	switch(MARGIN,
		chunk_rowapply(X, FUN, ...),
		chunk_colapply(X, FUN, ...))
}

chunk_rowapply <- function(X, FUN, ...,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		stop("X must have exactly 2 dimensions")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_matrix(X, margin=1L, nchunks=nchunks,
		depends=depends, local=has_localworkers(BPPARAM))
	if ( has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="array",
		rngseeds=rngseeds, progress=progress)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

chunk_colapply <- function(X, FUN, ...,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		stop("X must have exactly 2 dimensions")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_matrix(X, margin=2L, nchunks=nchunks,
		depends=depends, local=has_localworkers(BPPARAM))
	if ( has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="array",
		rngseeds=rngseeds, progress=progress)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

#### Chunk-Apply functions over lists ####
## ----------------------------------------

chunkLapply <- function(X, FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- chunk_writer(pid, outpath)
	} else {
		put <- NULL
	}
	CHUNKFUN <- chunk_loop_fun(FUN, type="vector", put=put)
	ans.list <- chunk_lapply(X, CHUNKFUN, ..., verbose=verbose, BPPARAM=BPPARAM)
	names(ans.list) <- names(X)
	if ( outfile )
		ipcremove(pid)
	if ( outfile && isTRUE(simplify) ) {
		ans.list <- simplify2matter(ans.list)
	} else if ( is.function(simplify) || is.character(simplify) ) {
		ans.list <- match.fun(simplify)(ans.list)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	}
	ans.list
}

chunk_lapply <- function(X, FUN, ...,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_vector(X, nchunks=nchunks,
		depends=depends, local=has_localworkers(BPPARAM))
	if ( has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="vector",
		rngseeds=rngseeds, progress=progress)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

#### Chunk-Apply functions over multiple lists ####
## ------------------------------------------------

chunkMapply <- function(FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- chunk_writer(pid, outpath)
	} else {
		put <- NULL
	}
	CHUNKFUN <- chunk_loop_fun(FUN, type="list", put=put)
	ans.list <- chunk_mapply(CHUNKFUN, ..., verbose=verbose, BPPARAM=BPPARAM)
	names(ans.list) <- names(...elt(1L))
	if ( outfile )
		ipcremove(pid)
	if ( outfile && isTRUE(simplify) ) {
		ans.list <- simplify2matter(ans.list)
	} else if ( is.function(simplify) || is.character(simplify) ) {
		ans.list <- match.fun(simplify)(ans.list)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	}
	ans.list
}

chunk_mapply <- function(FUN, ..., MoreArgs = NULL,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	simplify <- match.fun(simplify)
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_list(..., nchunks=nchunks,
		depends=depends, local=has_localworkers(BPPARAM))
	if ( has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="list",
		rngseeds=rngseeds, progress=progress,
		MoreArgs=MoreArgs)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, BPPARAM=BPPARAM)
	do.call(simplify, ans)
}

#### Chunk-Apply classes and exported utilities ####
## -------------------------------------------------

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
		if ( is_nil(drop) ) {
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
			for ( j in i )
				ans[[j]] <- x[[j]]
			ans
		}
	})

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

chunk_writer <- function(id, path) {
	function(x, i = 0L) {
		while ( i && ipcvalue(id) != i ) {
			Sys.sleep(0.1)
		}
		ipclock(id)
		ans <- matter_list(x, path=path, append=TRUE)
		ipcunlock(id)
		if ( i )
			ipcyield(id)
		ans
	}
}

#### Chunk-Apply low-level utilities ####
## --------------------------------------

chunk_fun <- function(FUN, type,
	rngseeds, progress, MoreArgs = NULL)
{
	function(X, ...)
	{
		id <- attr(X, "chunkid")
		if ( !is.null(rngseeds) ) {
			oseed <- getRNGStream()
			on.exit(setRNGStream(oseed))
			setRNGStream(rngseeds[[id]])
		}
		if ( progress )
			print_chunk_progress(X)
		if ( type == "list" ) {
			X[[1L]] <- set_attr(X[[1L]], attributes(X))
			do.call(FUN, c(X, list(MoreArgs=MoreArgs)))
		} else {
			FUN(X, ...)
		}
	}
}

chunk_loop_fun <- function(FUN, type,
	margin = NULL, put = NULL)
{
	function(X, ..., MoreArgs)
	{
		id <- attr(X, "chunkid")
		if ( type == "list" )
			X <- set_attr(list(X, ...), attributes(X))
		N <- switch(type,
			list=length(X[[1L]]),
			vector=length(X),
			array=switch(margin, nrow(X), ncol(X)))
		ans <- vector("list", attr(X, "chunksize"))
		ii <- 1L
		for ( i in seq_len(N) )
		{
			dep <- attr(X, "depends")
			if ( is.null(dep) ) {
				j <- i
				get_subset <- `[[`
			} else {
				j <- dep[[i]]
				get_subset <- `[`
			}
			if ( is.null(j) )
				next
			xi <- switch(type,
				list=lapply(X, get_subset, j),
				vector=get_subset(X, j),
				array=switch(margin,
					X[j,,drop=FALSE],
					X[,j,drop=FALSE]))
			if ( type == "list" ) {
				xi[[1L]] <- set_attr(xi[[1L]], attributes(X))
				ans[[ii]] <- do.call(FUN, c(xi, MoreArgs))
			} else {
				ans[[ii]] <- FUN(xi, ...)
			}
			seed <- getRNGStream()
			if ( seed$kind == "L'Ecuyer-CMRG" )
			{
				seed$seed <- nextRNGSubStream(seed$seed)
				setRNGStream(seed)
			}
			ii <- ii + 1L
		}
		if ( is.null(put) ) {
			ans
		} else {
			put(ans, id)
		}
		ans
	}
}

print_chunk_progress <- function(X) {
	message("processing chunk ",
		attr(X, "chunkid"), "/", attr(X, "nchunks"),
		" (", attr(X, "chunksize"), " items)")
}

has_progressbar <- function(BPPARAM) {
	!is.null(BPPARAM) && bpprogressbar(BPPARAM)
}

has_RNGseed <- function(BPPARAM) {
	!is.null(BPPARAM) && !is.null(bpRNGseed(BPPARAM))
}

has_localworkers <- function(BPPARAM) {
	local_bp <- inherits(BPPARAM, c("NULL", "SerialParam", "MulticoreParam"))
	local_cl <- is(BPPARAM, "SnowParam") && is.numeric(bpworkers(BPPARAM))
	local_bp || local_cl
}

simplify2matter <- function(ans) {
	adims <- dim(atomdata(ans))
	if ( anyNA(adims) ) {
		ans
	} else if ( any(adims == 1L) ) {
		as(ans, "matter_vec")
	} else {
		as(ans, "matter_mat")
	}
}

