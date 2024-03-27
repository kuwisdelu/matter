
#### FastMap Projection ####
## -------------------------

fastmap <- function(x, k = 3L, distfun = NULL,
	transpose = FALSE, niter = 3L, verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.null(distfun) )
		distfun <- if (transpose) colDistFun else rowDistFun
	k <- min(k, dim(x))
	# prepare matrices
	j <- seq_len(k)
	if ( transpose ) {
		N <- NCOL(x)
		snames <- colnames(x)
	} else {
		N <- NROW(x)
		snames <- rownames(x)
	}
	scores <- matrix(0, nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	pivots <- matrix(nrow=k, ncol=3L,
		dimnames=list(paste0("C", j), c("p1", "p2", "dist")))
	fx <- distfun(x, x, ...)
	for ( i in j )
	{
		if ( verbose )
			message("fitting FastMap component ", i)
		# find pivots
		p1 <- sample.int(N, 1L)
		p2 <- sample.int(N, 1L)
		plast <- c(p1, p2)
		if ( verbose )
			message("finding pivots")
		for ( iter in seq_len(niter) )
		{
			# get distances to pivot 1
			d1x <- fx(p1)
			d1proj <- rowdist_at(scores, p1)[[1L]]
			d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
			# update pivot 2
			p2 <- which.max(d1)
			# get distances to pivot 2
			d2x <- fx(p2)
			d2proj <- rowdist_at(scores, p2)[[1L]]
			d2 <- sqrt(pmax(d2x^2 - d2proj^2, 0))
			# update pivot 1
			p1 <- which.max(d2)
			# get distance between pivots
			d12 <- d2[p1]
			if ( verbose )
				message("pivot distance = ",
					format.default(d12), " on iteration ", iter)
			if ( setequal(c(p1, p2), plast) ) {
				break
			} else {
				plast <- c(p1, p2)
			}
		}
		# finally, get distances to pivot 1
		d1x <- fx(p1)
		d1proj <- rowdist_at(scores, p1)[[1L]]
		d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
		if ( verbose )
			message("found pivots: ", p1, ", ", p2)
		# project current component
		if ( verbose )
			message("projecting component ", i)
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		scores[,i] <- xi
		pivots[i,] <- c(p1, p2, d12)
	}
	pindex <- as.vector(pivots[,1:2])
	if ( transpose ) {
		xp <- x[,pindex,drop=FALSE]
	} else {
		xp <- x[pindex,,drop=FALSE]
	}
	ans <- list(x=scores, sdev=apply(scores, 2L, sd),
		pivots=pivots, pivot.array=xp, distfun=distfun)
	names(ans$sdev) <- paste0("C", j)
	colnames(ans$x) <- paste0("C", j)
	rownames(ans$pivots) <- paste0("C", j)
	ans$transpose <- transpose
	class(ans) <- "fastmap"
	ans
}

print.fastmap <- function(x, print.x = FALSE, ...)
{
	cat(sprintf("FastMap (k=%d)\n", ncol(x$x)))
	if ( !is.null(x$sdev) ) {
		cat(sprintf("\nStandard deviations (1, .., k=%d):\n", length(x$sdev)))
	    preview_vector(x$sdev, ...)
	}
	if ( print.x && !is.null(x$x) ) {
		cat("\nProjected variables:\n")
		preview_matrix(x$x, ...)
	}
	invisible(x)
}

predict.fastmap <- function(object, newdata, ...)
{
	if ( missing(newdata) )
		return(object$x)
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	k <- nrow(object$pivots)
	xp <- object$pivot.array
	if (object$transpose) {
		N <- NCOL(newdata)
		snames <- colnames(newdata)
	} else {
		N <- NROW(newdata)
		snames <- rownames(newdata)
	}
	j <- seq_len(k)
	pred <- matrix(0, nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	fx <- object$distfun(xp, newdata, ...)
	for ( i in j ) {
		p1 <- object$pivots[i,1L]
		p2 <- object$pivots[i,2L]
		d12 <- object$pivots[i,3L]
		# get distances to pivot 1
		d1x <- fx(i)
		if ( i > 1L ) {
			proj1 <- object$x[p1,1L:(i - 1L),drop=FALSE]
			d1proj <- rowdist(pred[,1L:(i - 1L),drop=FALSE], proj1)
			d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
		} else {
			d1 <- d1x
		}
		# get distances to pivot 2
		d2x <- fx(i + k)
		if ( i > 1L ) {
			proj2 <- object$x[p2,1L:(i - 1L),drop=FALSE]
			d2proj <- rowdist(pred[,1L:(i - 1L),drop=FALSE], proj2)
			d2 <- sqrt(pmax(d2x^2 - d2proj^2, 0))
		} else {
			d2 <- d2x
		}
		# project current component
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		pred[,i] <- xi
	}
	pred
}

rowDistFun <- function(x, y, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", paste0(i, collapse=" "))
		rowDists(y, x[i,,drop=FALSE], metric=metric, p=p, weights=weights,
			verbose=verbose, nchunks=nchunks,
			BPPARAM=BPPARAM)
	}
}

colDistFun <- function(x, y, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", paste0(i, collapse=" "))
		colDists(y, x[,i,drop=FALSE], metric=metric, p=p, weights=weights,
			verbose=verbose, nchunks=nchunks,
			BPPARAM=BPPARAM)
	}
}
