
#### Partial least squares ####
## ----------------------------

# basic idea is to decompose x and y
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
	# center and scale x and y
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
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
	cvar <- setNames(numeric(k), paste0("C", j))
	inner <- numeric(k)
	y0 <- y
	for ( i in j )
	{
		u <- y[,which.max(colSums(y)),drop=FALSE]
		du <- Inf
		iter <- 1
		# calculate projections vectors
		while ( iter <= niter && du > tol )
		{
			uu <- sum(u^2)
			if ( transpose ) {
				w <- xt %*% u / uu
				w <- w / sqrt(sum(w^2))
				t <- crossprod(xt, w) / sum(w^2)
			} else {
				w <- crossprod(x, u) / uu
				w <- w / sqrt(sum(w^2))
				t <- x %*% w / sum(w^2)
			}
			tt <- sum(t^2)
			q <- crossprod(y, t) / tt
			unew <- (y %*% q) / sum(q^2)
			du <- sqrt(sum((unew - u)^2))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > niter && du > tol )
			warning("NIPALS did not converge in ",
				iter - 1, " iterations for component ", i)
		# deflate x and y based on projection
		if ( transpose ) {
			p <- xt %*% t / tt
			c <- crossprod(u, t) / tt
			xt <- xt - tcrossprod(p, t)
			y <- y - drop(c) * tcrossprod(t, q)
		} else {
			p <- crossprod(x, t) / tt
			c <- crossprod(u, t) / tt
			x <- x - tcrossprod(t, p)
			y <- y - drop(c) * tcrossprod(t, q)
		}
		# save current results
		cvar[i] <- crossprod(t, u)
		weights[,i] <- w
		loadings[,i] <- p
		scores[,i] <- t
		y.loadings[,i] <- q
		y.scores[,i] <- u
		inner[i] <- c
	}
	# calculate regression coefficients
	projection <- weights %*% solve(crossprod(loadings, weights))
	projection <- projection %*% (inner * diag(k))
	dimnames(projection) <- list(pnames, paste0("C", j))
	b <- tcrossprod(projection, y.loadings)
	if ( transpose ) {
		yhat <- t(t(b) %*% xt0)
	} else {
		yhat <- x0 %*% b
	}
	if ( is.numeric(y.scale) )
		yhat <- colsweep(yhat, y.scale, "*")
	if ( is.numeric(y.center) )
		yhat <- colsweep(yhat, y.center, "+")
	# return results
	ans <- list(coefficients=b, projection=projection, residuals=y0 - yhat,
		fitted.values=yhat, weights=weights, loadings=loadings, scores=scores,
		y.loadings=y.loadings, y.scores=y.scores, cvar=cvar)
	ans$transpose <- transpose
	ans$center <- if(is.null(center)) FALSE else center
	ans$scale <- if(is.null(scale)) FALSE else scale
	ans$y.center <- if(is.null(y.center)) FALSE else y.center
	ans$y.scale <- if(is.null(y.scale)) FALSE else y.scale
	ans$algorithm <- "nipals"
	class(ans) <- "pls"
	ans
}

# Kernel (#1 and #2)
pls_kernel <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, method = 1L, retscores = TRUE, verbose = NA, ...)
{
	k <- min(k, dim(x))
	# center and scale x and y + calculate covariance
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xy <- xt %*% y
		if ( method == 2L )
			xx <- tcrossprod(xt, xt)
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
		xy <- crossprod(x, y)
		if ( method == 2L )
			xx <- crossprod(x, x)
	}
	# prepare matrices
	j <- seq_len(k)
	projection <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	weights <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	y.loadings <- matrix(nrow=ncol(y), ncol=k,
		dimnames=list(colnames(y), paste0("C", j)))
	if ( retscores ) {
		scores <- matrix(nrow=N, ncol=k,
			dimnames=list(snames, paste0("C", j)))
		y.scores <- matrix(nrow=nrow(y), ncol=k,
			dimnames=list(rownames(y), paste0("C", j)))
		cvar <- setNames(numeric(k), paste0("C", j))
	}
	for ( i in j )
	{
		# calculate projection vectors
		if ( ncol(y) < P ) {
			yxxy <- crossprod(xy, xy)
			q <- eigen(yxxy, symmetric=TRUE)$vectors[,1L,drop=FALSE]
			w <- xy %*% q
		} else {
			xyyx <- tcrossprod(xy, xy)
			w <- eigen(xyyx, symmetric=TRUE)$vectors[,1L,drop=FALSE]
		}
		w <- w / sqrt(sum(w^2))
		r <- w
		if ( i > 1L ) {
			pk <- loadings[,1L:(i - 1L),drop=FALSE]
			rk <- projection[,1L:(i - 1L),drop=FALSE]
			r <- r - colSums(crossprod(w, pk) %*% t(rk))
		}
		# calculate loadings
		if ( method == 1L ) {
			if ( transpose ) {
				t <- t(crossprod(r, xt))
				tt <- sum(t^2)
				p <- (xt %*% t) / tt
			} else {
				t <- x %*% r
				tt <- sum(t^2)
				p <- crossprod(x, t) / tt
			}
			q <- crossprod(xy, r) / tt
		} else if ( method == 2L ) {
			if ( retscores ) {
				if ( transpose ) {
					t <- t(crossprod(r, xt))
				} else {
					t <- x %*% r
				}
			}
			tt <- drop(crossprod(r, xx %*% r))
			p <- crossprod(xx, r) / tt
			q <- crossprod(xy, r) / tt
		} else {
			stop("unrecognized kernel method: ", method)
		}
		# update covariance
		xy <- xy - tcrossprod(p, q) * tt
		# save current results
		weights[,i] <- w
		loadings[,i] <- p
		y.loadings[,i] <- q
		projection[,i] <- r
		if ( retscores ) {
			u <- y %*% q / sum(q^2)
			if ( i > 1L ) {
				tk <- scores[,1L:(i - 1L),drop=FALSE]
				u <- u - tk %*% (crossprod(tk, u) / colSums(tk^2))
			}
			cvar[i] <- crossprod(t, u)
			scores[,i] <- t
			y.scores[,i] <- u
		}
	}
	# calculate regression coefficients
	b <- tcrossprod(projection, y.loadings)
	if ( transpose ) {
		yhat <- t(t(b) %*% xt)
	} else {
		yhat <- x %*% b
	}
	if ( is.numeric(y.scale) )
		yhat <- colsweep(yhat, y.scale, "*")
	if ( is.numeric(y.center) )
		yhat <- colsweep(yhat, y.center, "+")
	# return results
	if ( retscores ) {
		ans <- list(coefficients=b, projection=projection, residuals=y - yhat,
			fitted.values=yhat, weights=weights, loadings=loadings, scores=scores,
			y.loadings=y.loadings, y.scores=y.scores, cvar=cvar)
	} else {
		ans <- list(coefficients=b, projection=projection, residuals=y - yhat,
			fitted.values=yhat, weights=weights, loadings=loadings,
			y.loadings=y.loadings)
	}
	ans$transpose <- transpose
	ans$center <- if(is.null(center)) FALSE else center
	ans$scale <- if(is.null(scale)) FALSE else scale
	ans$y.center <- if(is.null(y.center)) FALSE else y.center
	ans$y.scale <- if(is.null(y.scale)) FALSE else y.scale
	ans$algorithm <- "nipals"
	class(ans) <- "pls"
	ans
}

predict.pls <- function(object, newdata, k,
	type = c("response", "class"), ...)
{
	type <- match.arg(type)
	if ( missing(newdata) && missing(k) ) {
		if ( type == "class" ) {
			return(predict_class(fitted(object)))
		} else {
			return(fitted(object))
		}
	}
	if ( missing(newdata) )
		stop("'newdata' must be specified if 'k' is specified")
	if ( missing(k) )
		k <- ncol(object$loadings)
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	nm <- rownames(object$loadings)
	v <- if (object$transpose) rownames(newdata) else colnames(newdata)
	p <- if (object$transpose) nrow(newdata) else ncol(newdata)
	if ( !is.null(nm) ) {
		if ( !all(nm %in% v) )
			stop("'newdata' does not have named features ",
				"matching one of more of the original features")
		if ( object$transpose ) {
			newdata <- newdata[nm,,drop=FALSE]
		} else {
			newdata <- newdata[,nm,drop=FALSE]
		}
	} else {
		if ( p != nrow(object$loadings) )
			stop("'newdata' does not have the correct number of features")
	}
	# extract relevant components
	if ( k > ncol(object$loadings) )
		stop("'k' is larger than the number of components")
	projection <- object$projection[,1:k,drop=FALSE]
	y.loadings <- object$y.loadings[,1:k,drop=FALSE]
	# calculate regression coefficients
	b <- tcrossprod(projection, y.loadings)
	# predict new values
	if ( object$transpose ) {
		xt <- rowscale(newdata, center=object$center, scale=object$scale)
		pred <- t(t(b) %*% xt)
	} else {
		x <- colscale(newdata, center=object$center, scale=object$scale)
		pred <- x %*% b
	}
	if ( is.numeric(object$y.scale) )
		pred <- colsweep(pred, object$y.scale, "*")
	if ( is.numeric(object$y.center) )
		pred <- colsweep(pred, object$y.center, "+")
	if ( type == "class" )
		pred <- predict_class(pred)
	pred
}

print.pls <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
	cat(sprintf("Partial least squares (k=%d)\n", ncol(x$loadings)))
	if ( !is.null(x$cvar) ) {
		cat(sprintf("\nCovariances (1, .., k=%d):\n", length(x$cvar)))
	    print(x$cvar, ...)
	}
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
	# center and scale x and y
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
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
	j <- seq_len(k)
	weights <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	scores <- matrix(nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	ratio <- setNames(numeric(k), paste0("C", j))
	y0 <- y
	if ( transpose ) {
		w0 <- colsweep((xt %*% y), colSums(y^2), "/")
	} else {
		w0 <- colsweep(crossprod(x, y), colSums(y^2), "/")
	}
	tw <- prcomp(w0, center=FALSE, tol=tol)$x
	for ( i in j )
	{
		u <- y[,which.max(colSums(y)),drop=FALSE]
		du <- Inf
		iter <- 1
		# projection same as regular pls
		while ( iter <= niter && du > tol )
		{
			uu <- sum(u^2)
			if ( transpose ) {
				w <- xt %*% u / uu
				w <- w / sqrt(sum(w^2))
				t <- crossprod(xt, w) / sum(w^2)
			} else {
				w <- crossprod(x, u) / uu
				w <- w / sqrt(sum(w^2))
				t <- x %*% w / sum(w^2)
			}
			tt <- sum(t^2)
			q <- crossprod(y, t) / tt
			unew <- (y %*% q) / sum(q^2)
			du <- sqrt(sum((unew - u)^2))
			u <- unew
			iter <- iter + 1
		}
		if ( iter > niter && du > tol )
			warning("NIPALS did not converge in ",
				iter - 1, " iterations for component ", i)
		# orthogonalize projection
		if ( transpose ) {
			p <- xt %*% t / tt
			pnorm <- sqrt(sum(p^2))
			for ( l in seq_len(ncol(tw)) )
				p <- p - drop(crossprod(tw[,l], p) / sum(tw[,l]^2)) * tw[,l]
			wo <- p
			ro <- sqrt(sum(wo^2)) / pnorm
			wo <- wo / sqrt(sum(wo^2))
			to <- t(crossprod(wo, xt)) / sum(wo^2)
			po <- (xt %*% to) / sum(to^2)
			xt <- xt - tcrossprod(po, to)
		} else {
			p <- crossprod(x, t) / tt
			pnorm <- sqrt(sum(p^2))
			for ( l in seq_len(ncol(tw)) )
				p <- p - drop(crossprod(tw[,l], p) / sum(tw[,l]^2)) * tw[,l]
			wo <- p
			ro <- sqrt(sum(wo^2)) / pnorm
			wo <- wo / sqrt(sum(wo^2))
			to <- (x %*% wo) / sum(wo^2)
			po <- crossprod(x, to) / sum(to^2)
			x <- x - tcrossprod(to, po)
		}
		weights[,i] <- wo
		loadings[,i] <- po
		scores[,i] <- to
		ratio[i] <- ro
	}
	# return results
	x <- if (transpose) xt else x
	ans <- list(weights=weights, loadings=loadings,
		scores=scores, ratio=ratio, x=x)
	ans$transpose <- transpose
	ans$center <- if(is.null(center)) FALSE else center
	ans$scale <- if(is.null(scale)) FALSE else scale
	ans$y.center <- if(is.null(y.center)) FALSE else y.center
	ans$y.scale <- if(is.null(y.scale)) FALSE else y.scale
	ans$algorithm <- "nipals"
	class(ans) <- "opls"
	ans
}

predict.opls <- function(object, newdata, k, ...)
{
	if ( missing(newdata) && missing(k) )
		return(object$x)
	if ( missing(newdata) )
		stop("'newdata' must be specified if 'k' is specified")
	if ( missing(k) )
		k <- ncol(object$loadings)
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	nm <- rownames(object$loadings)
	v <- if (object$transpose) rownames(newdata) else colnames(newdata)
	p <- if (object$transpose) nrow(newdata) else ncol(newdata)
	if ( !is.null(nm) ) {
		if ( !all(nm %in% v) )
			stop("'newdata' does not have named features ",
				"matching one of more of the original features")
		if ( object$transpose ) {
			newdata <- newdata[nm,,drop=FALSE]
		} else {
			newdata <- newdata[,nm,drop=FALSE]
		}
	} else {
		if ( p != nrow(object$loadings) )
			stop("'newdata' does not have the correct number of features")
	}
	# extract relevant components
	if ( k > ncol(object$loadings) )
		stop("'k' is larger than the number of components")
	weights <- object$weights[,1:k,drop=FALSE]
	loadings <- object$loadings[,1:k,drop=FALSE]
	# predict new values
	if ( object$transpose ) {
		xt <- rowscale(newdata, center=object$center, scale=object$scale)
		for ( i in seq_len(k) ) {
			to <- t(t(weights[,i]) %*% xt) / sum(weights[,i]^2)
			xt <- xt - tcrossprod(loadings[,i], to)
		}
		xt
	} else {
		x <- colscale(newdata, center=object$center, scale=object$scale)
		for ( i in seq_len(k) ) {
			to <- (x %*% weights[,i]) / sum(weights[,i]^2)
			x <- x - tcrossprod(to, loadings[,i])
		}
		x
	}
}

print.opls <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
	cat(sprintf("Orthogonal partial least squares (k=%d)\n", ncol(x$loadings)))
	cat(sprintf("\nComponent ratios (1, .., k=%d):\n", length(x$ratio)))
    print(x$ratio, ...)
	invisible(x)
}

