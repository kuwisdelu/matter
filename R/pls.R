
#### Partial least squares ####
## ----------------------------

# the idea is to decompose x and y
# x = t * p.t
# y = u * q.t
# using the cross-covariance s(x,y)
# where
# w, c are weights
# p, q are loadings
# t, u are scroes

# NIPALS
pls_nipals <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-5, verbose = NA, ...)
{
	x <- as_real_memory_matrix(x)
	k <- min(k, dim(x))
	# center and scale x
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xt0 <- xt
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
		x0 <- x
	}
	# prepare matrices
	y <- as.matrix(y)
	j <- seq_len(k)
	weights <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	scores <- matrix(nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	y.loadings <- matrix(nrow=ncol(y), ncol=k,
		dimnames=list(colnames(y), paste0("C", j)))
	y.scores <- matrix(nrow=nrow(y), ncol=k,
		dimnames=list(rownames(y), paste0("C", j)))
	y0 <- y
	for ( i in j )
	{
		u <- y[,which.max(colSums(y)),drop=FALSE]
		du <- Inf
		iter <- 1
		# calculate projections vectors
		while ( iter <= niter && du > tol )
		{
			if ( transpose ) {
				w <- xt %*% u / drop(crossprod(u, u))
				w <- w / sqrt(sum(w^2))
				t <- crossprod(xt, w) / drop(crossprod(w, w))
			} else {
				w <- crossprod(x, u) / drop(crossprod(u, u))
				w <- w / sqrt(sum(w^2))
				t <- x %*% w / drop(crossprod(w, w))
			}
			q <- crossprod(y, t) / drop(crossprod(t, t))
			unew <- tcrossprod(y, q) / drop(crossprod(q, q))
			du <- sqrt(sum((unew - u)^2))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > niter && du > tol )
			warning("NIPALS did not converge in ",
				iter - 1, " iterations for component ", i)
		# deflate x and y based on projection
		if ( transpose ) {
			p <- xt %*% t / drop(crossprod(t, t))
			c <- crossprod(u, t) / drop(crossprod(t, t))
			xt <- xt - tcrossprod(p, t)
			y <- y - drop(c) * tcrossprod(t, q)
		} else {
			p <- crossprod(x, t) / drop(crossprod(t, t))
			c <- crossprod(u, t) / drop(crossprod(t, t))
			x <- x - tcrossprod(t, p)
			y <- y - drop(c) * tcrossprod(t, q)
		}
		weights[,i] <- w
		loadings[,i] <- p
		scores[,i] <- t
		y.loadings[,i] <- q
		y.scores[,i] <- u
	}
	# calculate regression coefficients
	hat <- weights %*% solve(crossprod(loadings, weights))
	b <- tcrossprod(hat, y.loadings)
	if ( transpose ) {
		yhat <- t(t(b) %*% xt0)
	} else {
		yhat <- x0 %*% b
	}
	# return results
	ans <- list(coefficients=b, residuals=y0 - yhat,
		fitted.values=yhat, weights=weights, loadings=loadings,
		scores=scores, y.loadings=y.loadings, y.scores=y.scores)
	ans$transpose <- transpose
	if ( is.null(center) ) {
		ans$center <- FALSE
	} else {
		ans$center <- center
	}
	if ( is.null(scale) ) {
		ans$scale <- FALSE
	} else {
		ans$scale <- scale
	}
	ans$algorithm <- "nipals"
	class(ans) <- "pls"
	ans
}

predict.pls <- function(object, newdata, ...)
{
	if ( missing(newdata) )
		return(object$x)
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	nm <- rownames(object$loadings)
	if ( !is.null(nm) ) {
		if ( !all(nm %in% colnames(newdata)) )
			stop("'newdata' does not have named columns ",
				"matching one of more of the original columns")
		newdata <- newdata[,nm,drop=FALSE]
	} else {
		if ( ncol(newdata) != nrow(object$loadings) )
			stop("'newdata' does not have the correct number of columns")
	}
	if ( object$transpose ) {
		xt <- rowscale(newdata, center=object$center, scale=object$scale)
		yhat <- t(t(object$coefficients) %*% xt)
	} else {
		x <- colscale(newdata, center=object$center, scale=object$scale)
		yhat <- x %*% object$coefficients
	}
	yhat
}

print.pls <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
	cat(sprintf("Partial least squares (k = %d)\n", nrow(x$loadings)))
	if (length(coef(x))) {
		cat("\nCoefficients:\n")
		print.default(format(t(coef(x)), digits = digits), print.gap = 2L, 
			quote = FALSE)
	} else {
		cat("No coefficients\n")
	}
	cat("\n")
	invisible(x)
}


#### Orthogonal partial least squares ####
## ---------------------------------------

# improve interpretation of pls model:
# 1st, preprocessing the data matrix
# by removing non-correlated variation
# 2nd, do pls with 1 component (not here)

# NIPALS
opls_nipals <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-9, verbose = NA, ...)
{
	x <- as_real_memory_matrix(x)
	k <- min(k, dim(x))
	# center and scale x
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xt0 <- xt
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
		x0 <- x
	}
	# prepare matrices
	y <- as.matrix(y)
	j <- seq_len(k)
	weights <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	scores <- matrix(nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	y0 <- y
	if ( transpose ) {
		w0 <- (xt %*% y) / drop(crossprod(y, y))
	} else {
		w0 <- crossprod(x, y) / drop(crossprod(y, y))
	}
	tw <- prcomp(w0, center=FALSE)$x
	tw <- tw[,colSums(tw^2) / sum(w0^2) > tol,drop=FALSE]
	for ( i in j )
	{
		u <- y[,which.max(colSums(y)),drop=FALSE]
		du <- Inf
		iter <- 1
		# projection same as regular pls
		while ( iter <= niter && du > tol )
		{
			if ( transpose ) {
				w <- xt %*% u / drop(crossprod(u, u))
				w <- w / sqrt(sum(w^2))
				t <- crossprod(xt, w) / drop(crossprod(w, w))
			} else {
				w <- crossprod(x, u) / drop(crossprod(u, u))
				w <- w / sqrt(sum(w^2))
				t <- x %*% w / drop(crossprod(w, w))
			}
			q <- crossprod(y, t) / drop(crossprod(t, t))
			unew <- tcrossprod(y, q) / drop(crossprod(q, q))
			du <- sqrt(sum((unew - u)^2))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > niter && du > tol )
			warning("NIPALS did not converge in ",
				iter - 1, " iterations for component ", i)
		# orthogonalize projection
		if ( transpose ) {
			p <- xt %*% t / drop(crossprod(t, t))
			wo <- p - colsweep(tw, crossprod(tw, p) / drop(crossprod(tw, tw)), "*")
			wo <- wo / sqrt(sum(wo^2))
			to <- t(crossprod(wo, xt)) / drop(crossprod(wo, wo))
			po <- (xt %*% to) / drop(crossprod(to, to))
			xt <- xt - tcrossprod(po, to)
		} else {
			p <- crossprod(x, t) / drop(crossprod(t, t))
			wo <- p - colsweep(tw, crossprod(tw, p) / drop(crossprod(tw, tw)), "*")
			wo <- wo / sqrt(sum(wo^2))
			to <- (x %*% wo) / drop(crossprod(wo, wo))
			po <- crossprod(x, to) / drop(crossprod(to, to))
			x <- x - tcrossprod(to, po)
		}
		weights[,i] <- wo
		loadings[,i] <- po
		scores[,i] <- to
	}
	# return results
	ans <- list(weights=weights, loadings=loadings, scores=scores)
	ans$x <- if (transpose) xt else x
	ans$transpose <- transpose
	if ( is.null(center) ) {
		ans$center <- FALSE
	} else {
		ans$center <- center
	}
	if ( is.null(scale) ) {
		ans$scale <- FALSE
	} else {
		ans$scale <- scale
	}
	ans$algorithm <- "nipals"
	class(ans) <- "opls"
	ans
}

