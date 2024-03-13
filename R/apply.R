
#### Chunk-Apply functions over vectors and arrays ####
## ----------------------------------------------------

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
	}
	CHUNKFUN <- function(X, ...) {
		dn <- switch(MARGIN, nrow(X), ncol(X))
		cid <- attr(X, "chunkid")
		ans <- vector("list", attr(X, "chunksize"))
		ii <- 1L
		for ( i in seq_len(dn) ) {
			di <- attr(X, "index")
			dp <- attr(X, "depends")
			if ( is.null(dp) ) {
				xi <- switch(MARGIN,
					X[i,,drop=TRUE],
					X[,i,drop=TRUE])
			} else {
				if ( is.null(dp[[i]]) )
					next
				xi <- switch(MARGIN,
					X[dp[[i]],,drop=FALSE],
					X[,dp[[i]],drop=FALSE])
			}
			ans[[ii]] <- FUN(xi, ...)
			ii <- ii + 1L
		}
		if ( outfile ) {
			put(ans, cid)
		} else {
			ans
		}
	}
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
	INDEX <-  chunkify(seq_len(nrow(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && (is.null(BPPARAM) || !bpprogressbar(BPPARAM)) )
			print_chunk_progress(i, length(INDEX))
		xi <- as.matrix(X[i,,drop=FALSE])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
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
	INDEX <-  chunkify(seq_len(ncol(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && (is.null(BPPARAM) || !bpprogressbar(BPPARAM)) )
			print_chunk_progress(i, length(INDEX))
		xi <- as.matrix(X[,i,drop=FALSE])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

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
	}
	CHUNKFUN <- function(X, ...) {
		cid <- attr(X, "chunkid")
		ans <- vector("list", attr(X, "chunksize"))
		ii <- 1L
		for ( i in seq_along(X) ) {
			di <- attr(X, "index")
			dp <- attr(X, "depends")
			if ( is.null(dp) ) {
				xi <- X[[i]]
			} else {
				if ( is.null(dp[[i]]) )
					next
				xi <- X[dp[[i]]]
			}
			ans[[ii]] <- FUN(xi, ...)
			ii <- ii + 1L
		}
		if ( outfile ) {
			put(ans, cid)
		} else {
			ans
		}
	}
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
	INDEX <-  chunkify(seq_len(length(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && (is.null(BPPARAM) || !bpprogressbar(BPPARAM)) )
			print_chunk_progress(i, length(INDEX))
		xi <- as.vector(X[i])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

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
	}
	CHUNKFUN <- function(..., MoreArgs) {
		XS <- list(...)
		cid <- attr(XS[[1L]], "chunkid")
		ans <- vector("list", attr(XS[[1L]], "chunksize"))
		ii <- 1L
		for ( i in seq_along(XS[[1L]]) ) {
			di <- attr(XS[[1L]], "index")
			dp <- attr(XS[[1L]], "depends")
			if ( is.null(dp) ) {
				xsi <- lapply(XS, `[[`, i)
			} else {
				if ( is.null(dp[[i]]) )
					next
				xsi <- lapply(XS, `[`, dp[[i]])
			}
			ans[[ii]] <- do.call(FUN, c(xsi, MoreArgs))
			ii <- ii + 1L
		}
		if ( outfile ) {
			put(ans, cid)
		} else {
			ans
		}
	}
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
	BIND <- match.fun(simplify)
	XS <- list(...)
	if ( length(XS) > 1L ) {
		len <- vapply(XS, length, integer(1L))
		if ( n_unique(len) != 1L ) {
			max.len <- max(len)
			if ( max.len && any(len == 0L) )
				stop("zero-length and non-zero length inputs cannot be mixed")
			if ( any(max.len %% len) ) 
				warning("longer argument not a multiple of length of vector")
			XS <- lapply(XS, rep_len, length.out=max.len)
		}
	}
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	INDEX <-  chunkify(seq_len(length(XS[[1L]])), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && (is.null(BPPARAM) || !bpprogressbar(BPPARAM)) )
			print_chunk_progress(i, length(INDEX))
		xsi <- lapply(XS, `[`, i)
		xsi[[1L]] <- set_attr(xsi[[1L]], attributes(i))
		do.call(FUN, c(xsi, list(MoreArgs=MoreArgs)))
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

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
	for ( i in seq_along(index) ) {
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
	}
	names(ans) <- names(x)
	ans
}

chunk_writer <- function(pid, path) {
	function(x, i = 0L) {
		while ( i && ipcvalue(pid) != i ) {
			Sys.sleep(0.1)
		}
		ipclock(pid)
		ans <- matter_list(x, path=path, append=TRUE)
		ipcunlock(pid)
		if ( i )
			ipcyield(pid)
		ans
	}
}

print_chunk_progress <- function(i, nchunks) {
	message("processing chunk ",
		attr(i, "chunkid"), "/", nchunks,
		" (", length(i), " items)")
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

