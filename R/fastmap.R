
#### FastMap Projection ####
## -------------------------

setMethod("fastmap", "matrix",
	function(x, k = 3L, transpose = FALSE, ...)
{
	if ( transpose ) {
		distfun <- coldistfun
		MARGIN <- 2L
	} else {
		distfun <- rowdistfun
		MARGIN <- 1L
	}
	fastmap_fun(x, k=k, distfun=distfun, MARGIN=MARGIN, ...)
})

fastmap_fun <- function(x, k = 3L, distfun = rowdistfun,
	MARGIN = 1L, niter = 3L, verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(k, dim(x))
	# prepare matrices
	j <- seq_len(k)
	N <- dim(x)[MARGIN]
	snames <- dimnames(x)[[MARGIN]]
	scores <- matrix(0, nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	pivots <- matrix(nrow=k, ncol=2L,
		dimnames=list(paste0("C", j), NULL))
	fx <- distfun(x, seq_len(N), ...)
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
			d1x <- fx(x, p1)
			d1proj <- rowdist_at(scores, p1)[[1L]]
			d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
			# update pivot 2
			p2 <- which.max(d1)
			# get distances to pivot 2
			d2x <- fx(x, p2)
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
		d1x <- fx(x, p1)
		d1proj <- rowdist_at(scores, p1)[[1L]]
		d1 <- sqrt(pmax(d1x^2 - d1proj^2, 0))
		if ( verbose )
			message("found pivots: ", p1, ", ", p2)
		# project current component
		if ( verbose )
			message("projecting component ", i)
		xi <- (d1^2 + d12^2 - d2^2) / (2 * d12)
		scores[,i] <- xi
		pivots[i,] <- c(p1, p2)
	}
	ans <- list(x=scores, pivots=pivots, distfun=distfun(x, pivots, ...))
	colnames(ans$x) <- paste0("C", j)
	rownames(ans$pivots) <- paste0("C", j)
	class(ans) <- "fastmap"
	ans
}

rowdistfun <- function(y, j = NULL, metric = "euclidean", p = 2)
{
	if ( !is.null(j) )
		y <- y[j,,drop=FALSE]
	function(x, i) {
		rowdist_at(x, i, y, metric=metric, p=p)[[1L]]
	}
}

coldistfun <- function(y, j = NULL, metric = "euclidean", p = 2)
{
	if ( !is.null(j) )
		y <- y[,j,drop=FALSE]
	function(x, i) {
		coldist_at(x, i, y, metric=metric, p=p)[[1L]]
	}
}
