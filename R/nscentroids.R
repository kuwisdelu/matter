
#### Nearest shrunken centroids ####
## ---------------------------------

nscentroids <- function(x, y, s = 0, distfun = NULL,
	priors = table(y), center = NULL, transpose = FALSE,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.null(distfun) )
		distfun <- if (transpose) colDists else rowDists
	y <- as.factor(y)
	k <- nlevels(y)
	priors <- rep_len(as.vector(priors), k)
	priors <- priors / sum(priors)
	names(priors) <- levels(y)
	# calculate global centroid
	if ( is.null(center) ) {
		matter_log("calculating global centroid", verbose=verbose)
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, BPPARAM=BPPARAM, ...)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, BPPARAM=BPPARAM, ...)
		}
	}
	# calculate class centroids
	matter_log("calculating class centroids", verbose=verbose)
	if ( transpose ) {
		centers <- rowStats(x, stat="mean", group=y, na.rm=TRUE,
			verbose=verbose, BPPARAM=BPPARAM, ...)
	} else {
		centers <- colStats(x, stat="mean", group=y, na.rm=TRUE,
			verbose=verbose, BPPARAM=BPPARAM, ...)
	}
	if ( !is.matrix(centers) ) {
		centers <- as.matrix(centers)
		colnames(centers) <- levels(y)
	}
	# calculate t-statistics
	matter_log("calculating class statistics", verbose=verbose)
	if ( transpose ) {
		xc <- rowsweep(x, STATS=centers, group=y)
		wcss <- rowStats(xc^2, stat="sum", group=y, na.rm=TRUE,
			verbose=verbose, BPPARAM=BPPARAM, ...)
	} else {
		xc <- colsweep(x, STATS=centers, group=y)
		wcss <- colStats(xc^2, stat="sum", group=y, na.rm=TRUE,
			verbose=verbose, BPPARAM=BPPARAM, ...)
	}
	if ( !is.matrix(wcss) ) {
		wcss <- as.matrix(wcss)
		colnames(wcss) <- levels(y)
	}
	sd <- sqrt(rowSums(wcss, na.rm=TRUE) / (length(y) - nlevels(y)))
	s0 <- median(sd, na.rm=TRUE)
	mk <- sqrt((1 / table(y)) - (1 / length(y)))
	se <- (sd + s0) * rep(mk, each=length(sd))
	dim(se) <- c(length(sd), nlevels(y))
	statistic <- (centers - center) / se
	if ( anyNA(statistic) )
		statistic <- replace(statistic, is.na(statistic), 0)
	# calculate fitted values
	s <- sort(s)
	ans <- vector("list", length=length(s))
	for ( i in seq_along(s) )
	{
		matter_log("fitting values for s = ", s[i], verbose=verbose)
		s_statistic <- soft(statistic, s[i])
		s_centers <- center + se * s_statistic
		if ( k > 1L && !any(abs(s_statistic) > 0) )
			matter_warn("model is fully sparse; 's' is too large")
		if ( transpose ) {
			ds <- distfun(x, s_centers,
				weights=1 / (sd + s0)^2, BPPARAM=BPPARAM, ...)
		} else {
			ds <- distfun(x, t(s_centers),
				weights=1 / (sd + s0)^2, BPPARAM=BPPARAM, ...)
		}
		if ( is.function(ds) )
			.Defunct(msg="distfun requirements have changed; see ?nscentroids")
		if ( !is.matrix(ds) ) {
			ds <- as.matrix(ds)
			colnames(ds) <- levels(y)
		}
		scores <- ds^2 - 2 * log(rep(priors, each=nrow(ds)))
		colnames(scores) <- levels(y)
		prob <- exp(-scores / 2)
		prob <- pmax(prob / rowSums(prob, na.rm=TRUE), 0)
		for ( j in seq_len(nrow(prob)) ) {
			if ( anyNA(prob[j,]) ) {
				prob[j,] <- 1
				prob[j,which.min(scores[j,])] <- 1
			}
		}
		class <- predict_class(prob)
		ans[[i]] <- list(class=class, probability=prob, scores=scores,
			centers=s_centers, statistic=s_statistic, sd=sd,
			priors=priors, s=s[i], distfun=distfun,
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
	type = c("response", "class"), priors = NULL, ...)
{
	type <- match.arg(type)
	if ( missing(newdata) )
		return(fitted(object, type=type))
	if ( length(dim(newdata)) != 2L )
		matter_error("'newdata' must be a matrix or data frame")
	if ( is.null(priors) ) {
		priors <- object$priors
	} else {
		priors <- rep_len(priors, ncol(object$centers))
	}
	priors <- priors / sum(priors)
	sd <- object$sd
	s0 <- median(sd, na.rm=TRUE)
	if ( object$transpose ) {
		ds <- object$distfun(newdata, object$centers,
			weights=1 / (sd + s0)^2, ...)
	} else {
		ds <- object$distfun(newdata, t(object$centers),
			weights=1 / (sd + s0)^2, ...)
	}
	scores <- ds^2 - 2 * log(rep(priors, each=nrow(ds)))
	colnames(scores) <- colnames(object$centers)
	prob <- exp(-scores / 2)
	prob <- pmax(prob / rowSums(prob, na.rm=TRUE), 0)
	for ( j in seq_len(nrow(prob)) ) {
		if ( anyNA(prob[j,]) ) {
			prob[j,] <- 0
			prob[j,which.min(scores[j,])] <- 1
		}
	}
	if ( type == "class" ) {
		predict_class(prob)
	} else {
		prob
	}
}

logLik.nscentroids <- function(object, ...)
{
	prob <- object$probability
	prob <- replace(prob, is.na(prob), 0)
	loglik <- sum(log(apply(prob, 1L, max)))
	stat <- object$statistic
	df <- sum(abs(stat) > 0) + nrow(stat)
	structure(loglik, df=df, nobs=nrow(prob),
		class="logLik")
}

