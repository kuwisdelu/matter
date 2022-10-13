
#### Chunk-Apply functions over vectors and arrays ####
## ----------------------------------------------------

setMethod("apply", "matter_mat",
	function(X, MARGIN, FUN, ..., simplify = TRUE)
	{
		.Deprecated("chunkApply")
		chunkApply(X, MARGIN, FUN, ..., simplify=simplify)
	})

chunkApply <- function(X, MARGIN, FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( !MARGIN %in% c(1L, 2L) )
		stop("MARGIN must be 1 or 2")
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- remote_writer(pid, outpath)
	}
	CHUNKFUN <- function(X, ...) {
		dn <- switch(MARGIN, nrow(X), ncol(X))
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
			if ( outfile )
				ans[[ii]] <- put(ans[[ii]])
			ii <- ii + 1L
		}
		ans
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
	if ( outfile ) {
		ans.list <- remote_collect(ans.list, outpath, simplify)
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
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		stop("X must have exactly 2 dimensions")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	INDEX <-  chunkify(seq_len(nrow(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ",
				attr(i, "chunkid"), "/", length(INDEX))
		xi <- as.matrix(X[i,,drop=FALSE])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

chunk_colapply <- function(X, FUN, ...,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.null(dim(X)) || length(dim(X)) != 2L )
		stop("X must have exactly 2 dimensions")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	INDEX <-  chunkify(seq_len(ncol(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ",
				attr(i, "chunkid"), "/", length(INDEX))
		xi <- as.matrix(X[,i,drop=FALSE])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

chunkLapply <- function(X, FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- remote_writer(pid, outpath)
	}
	CHUNKFUN <- function(X, ...) {
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
			if ( outfile )
				ans[[ii]] <- put(ans[[ii]])
			ii <- ii + 1L
		}
		ans
	}
	ans.list <- chunk_lapply(X, CHUNKFUN, ..., verbose=verbose, BPPARAM=BPPARAM)
	names(ans.list) <- names(X)
	if ( outfile ) {
		ans.list <- remote_collect(ans.list, outpath, simplify)
	} else if ( is.function(simplify) || is.character(simplify) ) {
		ans.list <- match.fun(simplify)(ans.list)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	}
	ans.list
}

chunk_lapply <- function(X, FUN, ...,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	INDEX <-  chunkify(seq_len(length(X)), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ",
				attr(i, "chunkid"), "/", length(INDEX))
		xi <- as.vector(X[i])
		xi <- set_attr(xi, attributes(i))
		FUN(xi, ...)
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

chunkMapply <- function(FUN, ...,
	simplify = FALSE, outpath = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	outfile <- !is.null(outpath)
	pid <- ipcid()
	if ( outfile ) {
		if ( !is.character(outpath) || length(outpath) != 1L )
			stop("'outpath' must be a scalar string (or NULL)")
		outpath <- normalizePath(outpath, mustWork=FALSE)
		if ( verbose )
			message("writing output to path = ", sQuote(outpath))
		put <- remote_writer(pid, outpath)
	}
	CHUNKFUN <- function(..., MoreArgs) {
		XS <- list(...)
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
			if ( outfile )
				ans[[ii]] <- put(ans[[ii]])
			ii <- ii + 1L
		}
		ans
	}
	ans.list <- chunk_mapply(CHUNKFUN, ..., verbose=verbose, BPPARAM=BPPARAM)
	names(ans.list) <- names(...elt(1L))
	if ( outfile ) {
		ans.list <- remote_collect(ans.list, outpath, simplify)
	} else if ( is.function(simplify) || is.character(simplify) ) {
		ans.list <- match.fun(simplify)(ans.list)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	}
	ans.list
}

chunk_mapply <- function(FUN, ..., MoreArgs = NULL,
	simplify = "c", nchunks = NA, depends = NULL,
	verbose = FALSE, BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	BIND <- match.fun(simplify)
	XS <- list(...)
	if ( length(XS) > 1L ) {
		len <- vapply(XS, length, integer(1L))
		if ( length(unique(len)) != 1L ) {
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
	INDEX <-  chunkify(seq_len(length(XS[[1L]])), nchunks, depends)
	CHUNKFUN <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ",
				attr(i, "chunkid"), "/", length(INDEX))
		xsi <- lapply(XS, `[`, i)
		xsi[[1L]] <- set_attr(xsi[[1L]], attributes(i))
		do.call(FUN, c(xsi, list(MoreArgs=MoreArgs)))
	}
	ans <- bplapply_int(INDEX, CHUNKFUN, ..., BPPARAM=BPPARAM)
	do.call(BIND, ans)
}

chunkify <- function(x, nchunks = 20L, depends = NULL) {
	nchunks <- min(ceiling(length(x) / 2L), nchunks)
	index <- seq_along(x)
	index <- split(index, cut(index, nchunks))
	ans <- vector("list", length(index))
	for ( i in seq_along(index) ) {
		if ( !is.null(depends) ) {
			di <- depends[index[[i]]]
			ind <- c(index[[i]], unlist(di))
			ind <- sort(unique(ind))
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

remote_writer <- function(pid, path) {
	fun <- function(x) {
		ipclock(pid)
		eof <- file.size(path)
		eof <- if (is.na(eof)) 0 else eof
		if ( !is.atomic(x) || is.complex(x) || is.character(x) )
			stop(paste0("file output must be of type ",
				"'raw', 'logical', 'integer', or 'double' ",
					"(", sQuote(class(x)), " provided)"))
		ans <- matter_arr(x, type=typeof(x),
			path=path, offset=eof, readonly=FALSE)
		ipcunlock(pid)
		as.double(c(
			type=type(ans),
			offset=eof,
			extent=length(ans)))
	}
	fun
}

remote_collect <- function(ans, path, simplify) {
	nms <- names(ans)
	dnm <- if (is.null(nms)) NULL else list(NULL, nms)
	ans <- do.call(rbind, ans)
	type <- as_Rtype(ans[,1])
	offset <- ans[,2]
	extent <- ans[,3]
	maybe_vector <- all(extent == 1L)
	maybe_matrix <- length(unique(extent)) == 1L
	simplify <- isTRUE(simplify)
	if ( simplify && maybe_vector ) {
		x <- matter_vec(type=type, path=path,
			offset=offset, extent=extent, names=nms)
	} else if ( simplify && maybe_matrix ) {
		x <- matter_mat(type=type, path=path,
			offset=offset, extent=extent, dimnames=dnm)
	} else {
		x <- matter_list(type=type, path=path,
			offset=offset, extent=extent, names=nms)
	}
	x
}

