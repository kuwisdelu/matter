
#### FastMap Projection ####
## -------------------------

fastmap <- function(x, k = 3L, group = NULL, distfun = NULL,
	transpose = FALSE, pivots = 10L, niter = 10L, verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.null(distfun) )
		distfun <- if (transpose) colDists else rowDists
	k <- min(max(k), dim(x))
	# prepare matrices
	j <- seq_len(k)
	if ( transpose ) {
		N <- NCOL(x)
		snames <- colnames(x)
	} else {
		N <- NROW(x)
		snames <- rownames(x)
	}
	npivots <- max(2L, min(pivots, N))
	scores <- matrix(0, nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	pivots <- matrix(nrow=k, ncol=3L,
		dimnames=list(paste0("C", j), c("p1", "p2", "dist")))
	for ( i in j )
	{
		matter_log("fitting FastMap component ", i, verbose=verbose)
		# find pivots
		pv <- sample.int(N, npivots)
		for ( iter in seq_len(niter) )
		{
			# get distances to pivot candidates
			if ( transpose ) {
				ds <- distfun(x, x[,pv,drop=FALSE], verbose=FALSE, ...)
			} else {
				ds <- distfun(x, x[pv,,drop=FALSE], verbose=FALSE, ...)
			}
			if ( is.function(ds) )
				.Defunct(msg="distfun requirements have changed; see ?fastmap")
			dproj <- do.call(cbind, rowdist_at(scores, pv))
			di <- sqrt(pmax(ds^2 - dproj^2, 0))
			di <- apply(di, 2L, function(d) d - min(d))
			pvnew <- choose_pivots(pv, di, group)
			pvdist <- attr(pvnew, "distance")
			if ( iter < niter )
			{
				matter_log("iteration ", iter, ": ", "max pivot distance",
					" (", pvnew[1L], ", ", pvnew[2L], ")",
					" = ", format(pvdist), verbose=verbose)
				if ( setequal(pvnew[1:2], pv[1:2]) )
					break
				pv <- pvnew
			}
		}
		# project current component
		p1 <- pv[1L]
		p2 <- pv[2L]
		d1 <- di[,1L]
		d2 <- di[,2L]
		d12 <- max(d1[p2], d2[p1])
		matter_log("projecting component ", i, " using ",
			"pivots: ", p1, ", ", p2, verbose=verbose)
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		xi <- ifelse(is.finite(xi), xi, 0)
		scores[,i] <- xi
		pivots[i,] <- c(p1, p2, d12)
	}
	pindex <- as.vector(pivots[,1:2])
	if ( transpose ) {
		pa <- x[,pindex,drop=FALSE]
	} else {
		pa <- x[pindex,,drop=FALSE]
	}
	ans <- list(x=scores, sdev=apply(scores, 2L, sd),
		pivots=pivots, pivot.array=pa, distfun=distfun)
	names(ans$sdev) <- paste0("C", j)
	colnames(ans$x) <- paste0("C", j)
	rownames(ans$pivots) <- paste0("C", j)
	ans$transpose <- transpose
	class(ans) <- "fastmap"
	ans
}

choose_pivots <- function(pivots, dists, group)
{
	if ( !is.null(group) )
	{
		for ( j in seq_along(pivots) ) {
			same <- group %in% group[pivots[j]]
			dists[same,j] <- -Inf
		}
	}
	i <- which.max(apply(dists, 2L, max))
	p1 <- pivots[i]
	p2 <- which.max(dists[,i])
	np <- length(pivots)
	if ( np > 2L ) {
		pivots <- c(p1, p2, sample.int(nrow(dists), np - 2L))
	} else {
		pivots <- c(p1, p2)
	}
	structure(pivots, distance=dists[p2,i])
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
		matter_error("'newdata' must be a matrix or data frame")
	k <- nrow(object$pivots)
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
	ds <- object$distfun(newdata, object$pivot.array, ...)
	for ( i in j ) {
		p1 <- object$pivots[i,1L]
		p2 <- object$pivots[i,2L]
		d12 <- object$pivots[i,3L]
		# get distances to pivot 1
		d1x <- ds[,i]
		if ( i > 1L ) {
			proj1 <- object$x[p1,1L:(i - 1L),drop=FALSE]
			d1proj <- rowdist(pred[,1L:(i - 1L),drop=FALSE], proj1)
			d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
		} else {
			d1 <- d1x
		}
		d1 <- d1 - min(d1)
		# get distances to pivot 2
		d2x <- ds[,i + k]
		if ( i > 1L ) {
			proj2 <- object$x[p2,1L:(i - 1L),drop=FALSE]
			d2proj <- rowdist(pred[,1L:(i - 1L),drop=FALSE], proj2)
			d2 <- sqrt(pmax(d2x^2 - d2proj^2, 0))
		} else {
			d2 <- d2x
		}
		d2 <- d2 - min(d2)
		# project current component
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		pred[,i] <- xi
	}
	pred
}

