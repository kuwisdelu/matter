
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

#### Chunk-Apply utilities ####
## -----------------------------

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

