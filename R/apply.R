
#### Chunk-Apply functions over vectors and arrays ####
## ----------------------------------------------------

chunk_apply <- function(X, FUN, MARGIN, ..., simplify = FALSE,
						chunks = NA, view = c("element", "chunk"),
						attr = list(), alist = list(), outfile = NULL,
						verbose = FALSE, BPREDO = list(), BPPARAM = bpparam())
{
	view <- match.arg(view)
	if ( !is.null(dim(X)) && missing(MARGIN) )
		stop("must specify MARGIN when X is array-like")
	if ( !missing(MARGIN) && is.character(MARGIN) )
		MARGIN <- match(MARGIN, names(dimnames(X)))
	index <- chunkify(X, chunks, MARGIN)
	index <- chunklabel(index)
	chunkfun <- function(i, ...) {
		if ( verbose )
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
			for ( j in 1L:dn ){
				if ( !is.null(dim(X)) ) {
					xj <- switch(MARGIN, drop(xi[j,]), drop(xi[,j]))
				} else {
					xj <- xi[[j]]
				}
				xx <- chunkattr(xj, i[j], attr, alist, view)
				ans[[j]] <- FUN(xx, ...)
			}
		} else {
			xx <- chunkattr(xi, i, attr, alist, view)
			ans <- FUN(xx, ...)
		}
		ans
	}
	ans.list <- bplapply(index, chunkfun, ..., BPREDO=BPREDO, BPPARAM=BPPARAM)
	if ( view == "element" ) {
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
	if ( isTRUE(simplify) ) {
		ans.list <- simplify2array(ans.list)
	} else if ( is.function(simplify) ) {
		ans.list <- simplify(ans.list)
	}
	ans.list
}

#### Chunk-Apply internal utilities ####
## --------------------------------------

chunkify <- function(x, chunks, margin) {
	if ( !is.numeric(chunks) || is.na(chunks) )
		chunks <- nchunks(x, margin=margin)
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
	chunkindex(length.out, chunks)
}

setMethod("nchunks", "ANY",
	function(object, size = NA, margin = NA, ...)
	{
		if ( is.na(size) )
			size <- getOption("matter.default.chunksize")
		if ( is.null(dim) ) {
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

chunkindex <- function(length.out, nchunks) {
	size <- max(1L, length.out / nchunks)
	n <- floor(length.out / size) + 1L
	index <- floor(seq(from=1L, to=length.out + 1L, length.out=n))
	i1 <- index[-length(index)]
	i2 <- index[-1L] - 1L
	mapply(`:`, i1, i2, SIMPLIFY=FALSE)
}

chunklabel <- function(index) {
	for ( i in seq_along(index) )
		attr(index[[i]], "idx") <- i
	index
}

chunkattr <- function(x, i, attr, alist, view) {
	if ( length(attr) > 0L )
		for ( nm in names(attr) )
			attr(x, nm) <- attr[[nm]]
	if ( length(alist) > 0L )
		for ( nm in names(alist) ) {
			if ( view == "element" ) {
				attr(x, nm) <- alist[[nm]][[i]]
			} else {
				attr(x, nm) <- alist[[nm]][i,drop=FALSE]
			}
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

