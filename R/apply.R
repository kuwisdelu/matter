
#### Chunk-Apply functions over matrices ####
## -------------------------------------------

chunkApply <- function(X, MARGIN, FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = NA, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( !MARGIN %in% c(1L, 2L) )
		matter_error("MARGIN must be 1 or 2")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			matter_error("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		put <- chunk_writer(pid, outpath)
		outpath <- normalizePath(outpath, mustWork=TRUE)
		matter_log("writing output to path = ", sQuote(outpath),
			verbose=verbose)
	} else {
		put <- NULL
	}
	CHUNKFUN <- chunk_loop_fun(FUN, type="matrix", margin=MARGIN, put=put)
	ans.list <- chunk_apply(X, MARGIN, CHUNKFUN, ...,
		verbose=verbose, BPPARAM=BPPARAM)
	names(ans.list) <- dimnames(X)[[MARGIN]]
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
		matter_error("MARGIN must be 1 or 2")
	switch(MARGIN,
		chunk_rowapply(X, FUN, ...),
		chunk_colapply(X, FUN, ...))
}

chunk_rowapply <- function(X, FUN, ...,
	simplify = "c", depends = NULL, permute = FALSE,
	RNG = FALSE, verbose = NA, chunkopts = list(),
	BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	simplify <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		matter_error("X must have exactly 2 dimensions")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( "nchunks" %in% ...names() ) {
		.Deprecated(old="nchunks", new="chunkopts")
		chunkopts$nchunks <- list(...)$nchunks
	}
	drop <- get_chunked_drop(X, chunkopts, BPPARAM)
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_mat(X, margin=1L,
		permute=permute, depends=depends,
		nchunks=get_nchunks(chunkopts),
		chunksize=get_chunksize(chunkopts),
		verbose=progress, drop=drop)
	if ( !RNG || has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="matrix", rngseeds=rngseeds)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, ..., BPPARAM=BPPARAM)
	matter_log("# collecting ", sum(lengths(CHUNKS)), " results ",
		"from ", length(CHUNKS), " chunks", verbose=progress)
	do.call(simplify, ans)
}

chunk_colapply <- function(X, FUN, ...,
	simplify = "c", depends = NULL, permute = FALSE,
	RNG = FALSE, verbose = NA, chunkopts = list(),
	BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	simplify <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		matter_error("X must have exactly 2 dimensions")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( "nchunks" %in% ...names() ) {
		.Deprecated(old="nchunks", new="chunkopts")
		chunkopts$nchunks <- list(...)$nchunks
	}
	drop <- get_chunked_drop(X, chunkopts, BPPARAM)
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_mat(X, margin=2L,
		permute=permute, depends=depends,
		nchunks=get_nchunks(chunkopts),
		chunksize=get_chunksize(chunkopts),
		verbose=progress, drop=drop)
	if ( !RNG || has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="matrix", rngseeds=rngseeds)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, ..., BPPARAM=BPPARAM)
	matter_log("# collecting ", sum(lengths(CHUNKS)), " results ",
		"from ", length(CHUNKS), " chunks", verbose=progress)
	do.call(simplify, ans)
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
			matter_error("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		put <- chunk_writer(pid, outpath)
		outpath <- normalizePath(outpath, mustWork=TRUE)
		matter_log("writing output to path = ", sQuote(outpath),
			verbose=verbose)
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
	simplify = "c", depends = NULL, permute = FALSE,
	RNG = FALSE, verbose = NA, chunkopts = list(),
	BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	simplify <- match.fun(simplify)
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( "nchunks" %in% ...names() ) {
		.Deprecated(old="nchunks", new="chunkopts")
		chunkopts$nchunks <- list(...)$nchunks
	}
	drop <- get_chunked_drop(X, chunkopts, BPPARAM)
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_vec(X,
		permute=permute, depends=depends,
		nchunks=get_nchunks(chunkopts),
		chunksize=get_chunksize(chunkopts),
		verbose=progress, drop=drop)
	if ( !RNG || has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="vector", rngseeds=rngseeds)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, ..., BPPARAM=BPPARAM)
	matter_log("# collecting ", sum(lengths(CHUNKS)), " results ",
		"from ", length(CHUNKS), " chunks", verbose=progress)
	do.call(simplify, ans)
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
			matter_error("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		put <- chunk_writer(pid, outpath)
		outpath <- normalizePath(outpath, mustWork=TRUE)
		matter_log("writing output to path = ", sQuote(outpath),
			verbose=verbose)
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
	simplify = "c", depends = NULL, permute = FALSE,
	RNG = FALSE, verbose = NA, chunkopts = list(),
	BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	simplify <- match.fun(simplify)
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( "nchunks" %in% ...names() ) {
		.Deprecated(old="nchunks", new="chunkopts")
		chunkopts$nchunks <- list(...)$nchunks
	}
	drop <- get_chunked_drop(...elt(1L), chunkopts, BPPARAM)
	progress <- verbose && !has_progressbar(BPPARAM)
	CHUNKS <- chunked_list(...,
		permute=permute, depends=depends,
		nchunks=get_nchunks(chunkopts),
		chunksize=get_chunksize(chunkopts),
		verbose=progress, drop=drop)
	if ( !RNG || has_RNGseed(BPPARAM) ) {
		rngseeds <- NULL
	} else {
		rngseeds <- RNGStreams(size=lengths(CHUNKS))
	}
	CHUNKFUN <- chunk_fun(FUN, type="list",
		rngseeds=rngseeds, MoreArgs=MoreArgs)
	ans <- bplapply_int(CHUNKS, CHUNKFUN, BPPARAM=BPPARAM)
	matter_log("# collecting ", sum(lengths(CHUNKS)), " results ",
		"from ", length(CHUNKS), " chunks", verbose=progress)
	do.call(simplify, ans)
}

#### Chunk-Apply utilities ####
## -----------------------------

chunk_fun <- function(FUN, type, rngseeds, MoreArgs = NULL)
{
	isoclos(function(X, ...)
	{
		chunkinfo <- attr(X, "chunkinfo")
		X <- switch(type,
			list=lapply(X, as.vector),
			vector=as.vector(X),
			matrix=as.matrix(X))
		id <- chunkinfo$chunkid
		if ( !is.null(rngseeds) ) {
			oseed <- getRNGStream()
			on.exit(setRNGStream(oseed))
			setRNGStream(rngseeds[[id]])
		}
		if ( type == "list" ) {
			X[[1L]] <- update_attr(X[[1L]], chunkinfo)
			do.call(FUN, c(X, list(MoreArgs=MoreArgs)))
		} else {
			X <- update_attr(X, chunkinfo)
			FUN(X, ...)
		}
	}, matter_env())
}

chunk_loop_fun <- function(FUN, type, margin = NULL, put = NULL)
{
	isoclos(function(X, ..., MoreArgs)
	{
		id <- attr(X, "chunkid")
		ans <- vector("list", attr(X, "chunklen"))
		dep <- attr(X, "depends")
		N <- switch(type,
			list=length(X),
			vector=length(X),
			matrix=switch(margin, nrow(X), ncol(X)))
		X <- switch(type, list=list(X, ...), X)
		ii <- 1L
		for ( i in seq_len(N) )
		{
			if ( is.null(dep) ) {
				j <- i
				drop <- TRUE
				get_subset <- `[[`
			} else {
				j <- dep[[i]]
				drop <- FALSE
				get_subset <- `[`
			}
			if ( is.null(j) )
				next
			xi <- switch(type,
				list=lapply(X, get_subset, j),
				vector=get_subset(X, j),
				matrix=switch(margin,
					X[j,,drop=drop],
					X[,j,drop=drop]))
			iseed <- getRNGStream()
			if ( type == "list" ) {
				ans[[ii]] <- do.call(FUN, c(xi, MoreArgs))
			} else {
				ans[[ii]] <- FUN(xi, ...)
			}
			if ( iseed$kind == "L'Ecuyer-CMRG" )
				iseed$seed <- nextRNGSubStream(iseed$seed)
			setRNGStream(iseed)
			ii <- ii + 1L
		}
		if ( is.null(put) ) {
			ans
		} else {
			put(ans, id)
		}
	}, matter_env())
}

chunk_writer <- function(id, path)
{
	if ( !file.exists(path) ) {
		if ( !file.create(path) )
			matter_error("failed to create file: ", sQuote(path))
		path <- normalizePath(path, mustWork=TRUE)
	}
	isoclos(function(x, i = 0L) {
		while ( i && ipcvalue(id) != i ) {
			Sys.sleep(0.1)
		}
		ipclock(id)
		ans <- matter_list(x, path=path, append=TRUE)
		ipcunlock(id)
		if ( i )
			ipcyield(id)
		ans
	}, matter_env())
}

get_nchunks <- function(options) chunk_option(options, "nchunks")

get_chunksize <- function(options) chunk_option(options, "chunksize")

get_serialize <- function(options) chunk_option(options, "serialize")

chunk_option <- function(options, name) {
	if ( !is.null(options) && !is.list(options) )
		matter_error("chunk options must be a list or NULL")
	ans <- options[[name]]
	if ( is.null(ans) )
		ans <- NA
	ans
}

get_chunked_drop <- function(X, chunkopts, BPPARAM)
{
	serialize <- get_serialize(chunkopts)
	if ( is.na(serialize) )
		serialize <- getOption("matter.default.serialize")
	if ( isTRUE(serialize) || !has_matter_data(X) ) {
		drop <- FALSE
	} else {
		if ( isFALSE(serialize) || has_local_cluster(BPPARAM) ) {
			drop <- NULL
		} else {
			drop <- FALSE
		}
	}
	drop
}

has_matter_data <- function(X) {
	is(X, "matter_") || (is(X, "matter") && is.matter(atomdata(X)))
}

has_local_cluster <- function(BPPARAM) {
	known_cl <- inherits(BPPARAM, c("SnowParam", "MulticoreParam"))
	known_cl && is.numeric(bpworkers(BPPARAM))
}

has_progressbar <- function(BPPARAM) {
	!is.null(BPPARAM) && bpprogressbar(BPPARAM)
}

has_RNGseed <- function(BPPARAM) {
	!is.null(BPPARAM) && !is.null(bpRNGseed(BPPARAM))
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

