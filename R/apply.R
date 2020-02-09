
#### Chunk-Apply functions over vectors and arrays ####
## ----------------------------------------------------

chunk_apply <- function(X, FUN, MARGIN, ..., simplify = FALSE,
						chunks = NA, view = c("element", "chunk"),
						attr = list(), alist = list(), pattern = NULL,
						outfile = NULL, verbose = FALSE,
						BPREDO = list(), BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	if ( !is.null(dim(X)) && missing(MARGIN) )
		stop("must specify MARGIN when X is array-like")
	if ( !missing(MARGIN) && is.character(MARGIN) )
		MARGIN <- match(MARGIN, names(dimnames(X)))
	if ( !is.null(pattern) ) {
		if ( !missing(view) )
			warning("'view' ignored with non-NULL 'pattern'")
		view <- "pattern"
	} else {
		view <- match.arg(view)
	}
	chunks <- get_nchunks(X, chunks, MARGIN)
	if ( view == "pattern" ) {
		index <- chunk_pattern(pattern, chunks)
	} else {
		index <- chunk_along(X, chunks, MARGIN)
	}
	index <- chunk_label(index)
	fout <- !is.null(outfile)
	pid <- ipcid()
	if ( fout ) {
		if ( !is.character(outfile) || length(outfile) != 1L )
			stop("outfile must be a length-1 character vector or NULL")
		if ( verbose )
			message("writing output to path = ", outfile)
		rwrite <- remote_writer(pid, outfile)
	}
	chunkfun <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ", attr(i, "idx"), "/", length(index))
		if ( !is.null(dim(X)) ) {
			if ( MARGIN == 1L ) {
				xi <- X[i,,drop=FALSE]
				dn <- dim(xi)[1L]
			} else if ( MARGIN == 2L ) {
				xi <- X[,i,drop=FALSE]
				dn <- dim(xi)[2L]
			} else {
				stop("only MARGIN = 1 or 2 supported")
			}
			xi <- as.matrix(xi)
		} else {
			xi <- X[i,drop=FALSE]
			dn <- length(xi)
		}
		if ( view == "element" ) {
			ans <- vector("list", dn)
			for ( j in 1L:dn ) {
				if ( !is.null(dim(X)) ) {
					xj <- switch(MARGIN, drop(xi[j,]), drop(xi[,j]))
				} else {
					xj <- xi[[j]]
				}
				xx <- chunk_attr(xj, i[j], attr, alist, view)
				ans[[j]] <- FUN(xx, ...)
			}
		} else if ( view == "pattern" ) {
			dp <- length(attr(i, "pattern"))
			ans <- vector("list", dp)
			for ( j in 1L:dp ) {
				j2 <- attr(i, "pattern")[[j]]
				if ( !is.null(dim(X)) ) {
					xj <- switch(MARGIN,
						as.matrix(xi[j2,,drop=FALSE]),
						as.matrix(xi[,j2,drop=FALSE]))
				} else {
					xj <- xi[j2]
				}
				xx <- chunk_attr(xj, i[j], attr, alist, view)
				ans[[j]] <- FUN(xx, ...)
			}
		} else {
			xx <- chunk_attr(xi, i, attr, alist, view)
			ans <- FUN(xx, ...)
		}
		if ( fout ) {
			if ( view %in% c("element", "pattern") ) {
				ans <- lapply(ans, rwrite)
			} else {
				ans <- rwrite(ans)
			}
		}
		ans
	}
	ans.list <- bplapply(index, chunkfun, ..., BPREDO=BPREDO, BPPARAM=BPPARAM)
	if ( view %in% c("element", "pattern") ) {
		ans.list <- do.call(c, ans.list)
		if ( !is.null(dim(X)) ) {
			if ( MARGIN == 1L ) {
				names(ans.list) <- dimnames(X)[[1L]]
			} else if ( MARGIN == 2L ) {
				names(ans.list) <- dimnames(X)[[2L]]
			}
		} else {
			names(ans.list) <- names(X)
		}
	}
	if ( fout ) {
		ans.list <- remote_collect(ans.list, outfile, simplify)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	} else if ( is.function(simplify) ) {
		ans.list <- simplify(ans.list)
	}
	ans.list
}

chunk_mapply <- function(FUN, ..., MoreArgs = NULL, simplify = FALSE,
						chunks = NA, view = c("element", "chunk"),
						attr = list(), alist = list(),
						outfile = NULL, verbose = FALSE,
						BPREDO = list(), BPPARAM = bpparam())
{
	FUN <- match.fun(FUN)
	view <- match.arg(view)
	dots <- list(...)
	if ( length(dots) > 1L ) {
		len <- vapply(dots, length, integer(1L))
		if ( !all(len == len[1L]) ) {
			max.len <- max(len)
			if ( max.len && any(len == 0L) )
				stop("zero-length and non-zero length inputs cannot be mixed")
			if ( any(max.len %% len) ) 
				warning("longer argument not a multiple of length of vector")
			dots <- lapply(dots, rep_len, length.out = max.len)
		}
	}
	chunks <- get_nchunks(dots[[1L]], chunks)
	index <- chunk_along(dots[[1L]], chunks)
	index <- chunk_label(index)
	fout <- !is.null(outfile)
	pid <- ipcid()
	if ( fout ) {
		if ( !is.character(outfile) || length(outfile) != 1L )
			stop("outfile must be a length-1 character vector or NULL")
		if ( verbose )
			message("writing output to path = ", outfile)
		rwrite <- remote_writer(pid, outfile)
	}
	chunkfun <- function(i, ...) {
		if ( verbose && !bpprogressbar(BPPARAM) )
			message("processing chunk ", attr(i, "idx"), "/", length(index))
		dd <- lapply(dots, `[`, i, drop=FALSE)
		dn <- length(dd[[1L]])
		if ( view == "element" ) {
			if ( length(attr) > 0L || length(alist) > 0L ) {
				ans <- vector("list", dn)
				for ( j in 1L:dn ) {
					ddd <- lapply(dd, `[[`, j, drop=FALSE)
					ddd[[1L]] <- chunk_attr(ddd[[1L]], i[j], attr, alist, view)
					ans[[j]] <- do.call(FUN, c(ddd, MoreArgs))
				}
			} else {
				ans <- .mapply(FUN, dd, MoreArgs)
			}
		} else {
			dd[[1L]] <- chunk_attr(dd[[1L]], i, attr, alist, view)
			ans <- do.call(FUN, c(dd, MoreArgs))
		}
		if ( fout ) {
			if ( view == "element" ) {
				ans <- lapply(ans, rwrite)
			} else {
				ans <- rwrite(ans)
			}
		}
		ans
	}
	ans.list <- bplapply(index, chunkfun, ..., BPREDO=BPREDO, BPPARAM=BPPARAM)
	if ( view == "element" ) {
		ans.list <- do.call(c, ans.list)
		names(ans.list) <- names(dots[[1L]])
	}
	if ( fout ) {
		ans.list <- remote_collect(ans.list, outfile, simplify)
	} else if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	} else if ( is.function(simplify) ) {
		ans.list <- simplify(ans.list)
	}
	ans.list
}

#### Chunk-Apply chunking utilities ####
## --------------------------------------

chunk_len <- function(length.out, nchunks) {
	size <- max(1L, length.out / nchunks)
	n <- floor(length.out / size) + 1L
	index <- floor(seq(from=1L, to=length.out + 1L, length.out=n))
	i1 <- index[-length(index)]
	i2 <- index[-1L] - 1L
	mapply(`:`, i1, i2, SIMPLIFY=FALSE)
}

chunk_along <- function(x, nchunks, margin) {
	if ( !is.null(dim(x)) ) {
		if ( margin == 1L ) {
			length.out <- nrow(x)
		} else if ( margin == 2L ) {
			length.out <- ncol(x)
		} else {
			stop("only MARGIN = 1 or 2 supported")
		}
	} else {
		length.out <- length(x)
	}
	chunk_len(length.out, nchunks)
}

chunk_pattern <- function(pattern, nchunks) {
	i <- chunk_len(length(pattern), nchunks)
	lapply(i, function(j) {
		pp <- pattern[j]
		index <- sort(unique(unlist(pp)))
		attr(index, "pattern") <- lapply(pp, match, index)
		index
	})
}

get_nchunks <- function(x, chunks, margin) {
	if ( !is.numeric(chunks) || is.na(chunks) ) {
		chunks <- nchunks(x, margin=margin)
	} else {
		chunks
	}
}

setMethod("nchunks", "ANY",
	function(object, size = NA, margin = NA, ...)
	{
		if ( is.na(size) )
			size <- getOption("matter.default.chunksize")
		if ( is.null(dim(object)) ) {
			nchunk_list(object, size=size, ...)
		} else {
			nchunk_mat(object, size=size, margin=margin, ...)
		}
	}
)

setMethod("nchunks", "matter",
	function(object, size = chunksize(object), margin = NA, ...)
	{
		if ( is.null(dim) ) {
			nchunk_vec(object, size=size, ...)
		} else {
			nchunk_mat(object, size=size, margin = margin, ...)
		}
	}
)

setMethod("nchunks", "matter_list",
	function(object, size = chunksize(object), margin = NA, ...)
	{
		nchunk_list(object, size=size, ...)
	}
)

nchunk_list <- function(x, size) {
	elts_per_chunk <- max(1L, floor(size / median(lengths(x))))
	ceiling(length(x) / elts_per_chunk)
}

nchunk_vec <- function(x, size) {
	ceiling(length(x) / size)
}

nchunk_mat <- function(x, size, margin) {
	if ( is.na(margin) )
		margin <- 1L
	if ( margin == 1L ) {
		elts_per_chunk <- max(1L, floor(size / ncol(x)))
		n <- ceiling(nrow(x) / elts_per_chunk)
	} else if ( margin == 2L ) {
		elts_per_chunk <- max(1L, floor(size / nrow(x)))
		n <- ceiling(ncol(x) / elts_per_chunk)
	} else {
		stop("only MARGIN = 1 or 2 supported")
	}
	n
}

chunk_split <- function(x, nchunks) {
	i <- chunk_len(length(x), nchunks)
	lapply(i, function(j) x[j])
}

chunk_label <- function(index) {
	for ( i in seq_along(index) )
		attr(index[[i]], "idx") <- i
	index
}

chunk_attr <- function(x, i, attr, alist, view) {
	if ( length(attr) > 0L )
		for ( nm in names(attr) )
			attr(x, nm) <- attr[[nm]]
	if ( length(alist) > 0L )
		for ( nm in names(alist) ) {
			if ( view %in% c("element", "pattern") ) {
				attr(x, nm) <- alist[[nm]][[i]]
			} else {
				attr(x, nm) <- alist[[nm]][i,drop=FALSE]
			}
		}
	x
}

#### Chunk-Apply i/o utilities ####
## ---------------------------------

remote_writer <- function(pid, path) {
	fun <- function(x) {
		ipclock(pid)
		eof <- file.size(path)
		eof <- ifelse(is.na(eof), 0, eof)
		if ( !is.atomic(x) || is.complex(x) || is.character(x) )
			stop(paste0("output for remote writing must be of type ",
				"'raw', 'logical', 'integer', or 'numeric'"))
		res <- matter_vec(x, datamode=typeof(x), filemode="rw",
							offset=eof, paths=path)
		ipcunlock(pid)
		# [,1] = mode; [,2] = offset; [,3] = extent
		c(datamode(res), eof, length(res))
	}
	fun
}

remote_collect <- function(ans, path, simplify) {
	nms <- names(ans)
	dnm <- list(NULL, nms)
	ans <- do.call(rbind, ans)
	mode <- make_datamode(ans[,1], type="R")
	mode <- as.character(mode)
	offset <- ans[,2]
	extent <- ans[,3]
	vector_ok <- all(extent == 1L)
	matrix_ok <- length(unique(extent)) == 1L
	simplify <- isTRUE(simplify)
	if ( simplify && vector_ok ) {
		if ( is.sorted(offset) ) {
			offset <- 0
			extent <- nrow(ans)
			mode <- mode[1L]
		}
		x <- matter_vec(datamode=mode, filemode="rw", names=nms,
						offset=offset, extent=extent, paths=path)
	} else if ( simplify && matrix_ok ) {
		x <- matter_mat(datamode=mode, filemode="rw", dimnames=dnm,
						offset=offset, extent=extent, paths=path)
	} else {
		x <- matter_list(datamode=mode, filemode="rw", names=nms,
						offset=offset, extent=extent, paths=path)
	}
	x
}

#### Apply functions over matter matrices ####
## -------------------------------------------

setMethod("apply", "matter_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		chunk_apply(X, FUN, MARGIN, ..., simplify=TRUE, BPPARAM=BPPARAM)
})

setMethod("apply", "sparse_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		chunk_apply(X, FUN, MARGIN, ..., simplify=TRUE, BPPARAM=BPPARAM)
})

setMethod("apply", "virtual_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		chunk_apply(X, FUN, MARGIN, ..., simplify=TRUE, BPPARAM=BPPARAM)
})

#### List-Apply functions over matter lists and data frames ####
## ------------------------------------------------------------

setMethod("lapply", "matter_list",
	function(X, FUN, ..., BPPARAM = bpparam())
	{
		chunk_apply(X, FUN, ..., simplify=FALSE, BPPARAM=BPPARAM)
	}
)

setMethod("sapply", "matter_list",
	function(X, FUN, ..., BPPARAM = bpparam(),
		simplify = TRUE, USE.NAMES = TRUE)
	{
		chunk_apply(X, FUN, ..., simplify=simplify, BPPARAM=BPPARAM)
	}
)

setMethod("lapply", "virtual_df",
	function(X, FUN, ..., BPPARAM = bpparam())
	{
		chunk_apply(X, FUN, ..., simplify=FALSE, BPPARAM=BPPARAM)
	}
)

setMethod("sapply", "virtual_df",
	function(X, FUN, ..., BPPARAM = bpparam(),
		simplify = TRUE, USE.NAMES = TRUE)
	{
		chunk_apply(X, FUN, ..., simplify=simplify, BPPARAM=BPPARAM)
	}
)

