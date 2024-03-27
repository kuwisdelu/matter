
#### Nearest shrunken centroids ####
## ---------------------------------

nscentroids <- function(x, y, s = 0, distfun = NULL,
	priors = table(y), center = NULL, transpose = FALSE,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( is.null(distfun) )
		distfun <- if (transpose) colDistFun else rowDistFun
	y <- as.factor(y)
	k <- nlevels(y)
	if ( k == 1L )
		stop("need at least 2 classes")
	priors <- rep_len(as.vector(priors), k)
	priors <- priors / sum(priors)
	names(priors) <- levels(y)
	# calculate global centroid
	if ( is.null(center) ) {
		if ( verbose )
			message("calculating global centroid")
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM)
		}
	}
	# calculate class centroids
	if ( verbose )
		message("calculating class centroids")
	if ( transpose ) {
		centers <- rowStats(x, stat="mean", group=y, na.rm=TRUE,
			nchunks=nchunks, verbose=FALSE,
			BPPARAM=BPPARAM)
	} else {
		centers <- colStats(x, stat="mean", group=y, na.rm=TRUE,
			nchunks=nchunks, verbose=FALSE,
			BPPARAM=BPPARAM)
	}
	# calculate t-statistics
	if ( verbose )
		message("calculating class statistics")
	if ( transpose ) {
		xc <- rowsweep(x, STATS=centers, group=y)
		wcss <- rowStats(xc^2, stat="sum", group=y, na.rm=TRUE,
			nchunks=nchunks, verbose=FALSE,
			BPPARAM=BPPARAM)
	} else {
		xc <- colsweep(x, STATS=centers, group=y)
		wcss <- colStats(xc^2, stat="sum", group=y, na.rm=TRUE,
			nchunks=nchunks, verbose=FALSE,
			BPPARAM=BPPARAM)
	}
	sd <- sqrt(rowSums(wcss, na.rm=TRUE) / (length(y) - nlevels(y)))
	s0 <- median(sd, na.rm=TRUE)
	mk <- sqrt((1 / table(y)) - (1 / length(y)))
	se <- (sd + s0) * rep(mk, each=length(sd))
	dim(se) <- c(length(sd), nlevels(y))
	statistic <- (centers - center) / se
	# calculate fitted values
	s <- sort(s)
	ans <- vector("list", length=length(s))
	for ( i in seq_along(s) )
	{
		if ( verbose )
			message("fitting values for s = ", s[i])
		s_statistic <- soft(statistic, s[i])
		s_centers <- center + se * s_statistic
		if ( !any(abs(s_statistic) > 0) )
			warning("model is fully sparse; 's' is too large")
		if ( transpose ) {
			fx <- distfun(s_centers, x, weights=1 / (sd + s0)^2,
				nchunks=nchunks, BPPARAM=BPPARAM, ...)
		} else {
			fx <- distfun(t(s_centers), x, weights=1 / (sd + s0)^2,
				nchunks=nchunks, BPPARAM=BPPARAM, ...)
		}
		ds <- fx(seq_len(k))
		scores <- ds^2 - 2 * log(rep(priors, each=nrow(ds)))
		colnames(scores) <- levels(y)
		prob <- exp(-scores / 2)
		prob <- pmax(prob / rowSums(prob, na.rm=TRUE), 0)
		class <- predict_class(prob)
		ans[[i]] <- list(class=class, probability=prob,
			centers=s_centers, statistic=s_statistic,
			sd=sd, priors=priors, s=s[i], distfun=distfun,
			transpose=transpose)
		class(ans[[i]]) <- "nscentroids"
	}
	if ( length(ans) > 1L ) {
		ans
	} else {
		ans[[1L]]
	}
}

print.nscentroids <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
	cat(sprintf("Nearest shrunken centroids (s=%.2f) with %d classes\n",
		x$s, nlevels(x$class)))
	if ( !is.null(x$priors) ) {
		cat(sprintf("\nPriors (1, .., k=%d):\n", length(x$priors)))
	    preview_vector(x$priors, ...)
	}
	if ( !is.null(x$statistic) ) {
		cat("\nStatistics:\n")
		preview_matrix(x$statistic, ..., zero.print=".")
	} else {
		cat("No statistics\n")
	}
	invisible(x)
}

fitted.nscentroids <- function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	if ( type == "class" ) {
		object$class
	} else {
		object$probability
	}
}

predict.nscentroids <- function(object, newdata,
	type = c("response", "class"), ...)
{
	type <- match.arg(type)
	if ( missing(newdata) )
		return(fitted(object, type=type))
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	priors <- object$priors
	k <- length(priors)
	sd <- object$sd
	s0 <- median(sd, na.rm=TRUE)
	if ( object$transpose ) {
		fx <- object$distfun(object$centers, newdata,
			weights=1 / (sd + s0)^2, ...)
	} else {
		fx <- object$distfun(t(object$centers), newdata,
			weights=1 / (sd + s0)^2, ...)
	}
	ds <- fx(seq_len(k))
	scores <- ds^2 - 2 * log(rep(priors, each=nrow(ds)))
	colnames(scores) <- names(priors)
	prob <- exp(-scores / 2)
	prob <- pmax(prob / rowSums(prob, na.rm=TRUE), 0)
	if ( type == "class" ) {
		predict_class(prob)
	} else {
		prob
	}
}

logLik.nscentroids <- function(object, ...)
{
	phat <- apply(object$probability, 1L, max, na.rm=TRUE)
	logp <- sum(log(phat), na.rm=TRUE)
	df <- sum(abs(object$statistic) > 0) + nrow(object$statistic)
	structure(logp, df=df, nobs=nrow(object$probability),
		class="logLik")
}

