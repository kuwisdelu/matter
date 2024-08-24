
#### FastMap Projection ####
## -------------------------

fastmap <- function(x, k = 3L, distfun = NULL,
	transpose = FALSE, niter = 10L, verbose = NA, ...)
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
	scores <- matrix(0, nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	pivots <- matrix(nrow=k, ncol=3L,
		dimnames=list(paste0("C", j), c("p1", "p2", "dist")))
	for ( i in j )
	{
		matter_log("fitting FastMap component ", i, verbose=verbose)
		# find pivots
		pv <- sample.int(N, 2L)
		for ( iter in seq_len(niter) )
		{
			# get distances to pivot candidates
			if ( transpose ) {
				ds <- distfun(x, x[,pv,drop=FALSE], ...)
			} else {
				ds <- distfun(x, x[pv,,drop=FALSE], ...)
			}
			if ( is.function(ds) )
				.Defunct(msg="distfun requirements have changed; see ?fastmap")
			# get pivot 1 distances
			d1x <- ds[,1L]
			d1proj <- rowdist_at(scores, pv[1L])[[1L]]
			d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
			# get pivot 2 distances
			d2x <- ds[,2L]
			d2proj <- rowdist_at(scores, pv[2L])[[1L]]
			d2 <- sqrt(pmax(d2x^2 - d2proj^2, 0))
			# get distance between pivots
			d12 <- d1[pv[2L]]
			# calculate best pivots for next iteration
			pvnew <- c(which.max(d2), which.max(d1))
			if ( iter < niter )
			{
				# update pivots
				if ( d1[pvnew[2L]] > d2[pvnew[1L]] ) {
					matter_log("iteration ", iter, ": ", "max pivot distance",
						" (", pv[1L], ", ", pvnew[2L], ") = ",
						format.default(d1[pvnew[2L]]), verbose=verbose)
					pvnew[1L] <- pv[1L]
				} else if ( d2[pvnew[1L]] > d1[pvnew[2L]] ) {
					matter_log("iteration ", iter, ": ", "max pivot distance",
						" (", pvnew[1L], ", ", pv[2L], ") = ",
						format.default(d2[pvnew[1L]]), verbose=verbose)
					pvnew[2L] <- pv[2L]
				}
				if ( setequal(pvnew, pv) )
					break
				pv <- pvnew
			}
		}
		# project current component
		matter_log("using pivots: ", pv[1L], ", ", pv[2L], verbose=verbose)
		matter_log("projecting component ", i, verbose=verbose)
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		scores[,i] <- xi
		pivots[i,] <- c(pv, d12)
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
		# get distances to pivot 2
		d2x <- ds[,i + k]
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

