
#### Partial least squares ####
## ----------------------------

# NIPALS
pls_nipals <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-5,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	x <- as_real_memory_matrix(x)
	k <- min(max(k), dim(x))
	# center and scale x and y
	if ( verbose && (center || scale.) )
		message("preparing x and y matrices")
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.,
		verbose=verbose, BPPARAM=BPPARAM, ...)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xt0 <- xt
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
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
	cvar <- set_names(numeric(k), paste0("C", j))
	inner <- numeric(k)
	y0 <- y
	for ( i in j )
	{
		if ( verbose )
			message("fitting PLS component ", i)
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
			matter_warn("NIPALS did not converge in ",
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
	if ( verbose )
		message("fitting regression coefficients")
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
		yhat <- colsweep_matrix(yhat, y.scale, "*")
	if ( is.numeric(y.center) )
		yhat <- colsweep_matrix(yhat, y.center, "+")
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

# SIMPLS
pls_simpls <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, method = 1L, retscores = TRUE,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(max(k), dim(x))
	# center and scale x and y + calculate covariance
	if ( verbose && (center || scale.) )
		message("preparing x and y matrices")
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.,
		verbose=verbose, BPPARAM=BPPARAM, ...)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xy <- xt %*% y
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
		xy <- crossprod(x, y)
	}
	# prepare matrices
	j <- seq_len(k)
	projection <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	y.loadings <- matrix(nrow=ncol(y), ncol=k,
		dimnames=list(colnames(y), paste0("C", j)))
	basis <- matrix(nrow=P, ncol=k)
	if ( retscores ) {
		scores <- matrix(nrow=N, ncol=k,
			dimnames=list(snames, paste0("C", j)))
		y.scores <- matrix(nrow=nrow(y), ncol=k,
			dimnames=list(rownames(y), paste0("C", j)))
		cvar <- set_names(numeric(k), paste0("C", j))
	}
	for ( i in j )
	{
		# show progress
		if ( verbose )
			message("fitting PLS component ", i)
		# calculate scores and loadings
		s <- svd(xy)
		r <- s$u[,1L,drop=FALSE]
		if ( transpose ) {
			t <- t(crossprod(r, xt))
			tt <- sum(t^2)
			p <- (xt %*% t) / tt
		} else {
			t <- x %*% r
			tt <- sum(t^2)
			p <- crossprod(x, t) / tt
		}
		q <- crossprod(y, t) / tt
		# update covariance
		v <- p
		if ( i > 1L ) {
			vk <- basis[,1L:(i - 1L),drop=FALSE]
			v <- v - vk %*% crossprod(vk, p)
		}
		v <- v / sqrt(sum(v^2))
		xy <- xy - v %*% crossprod(v, xy)
		# save current results
		loadings[,i] <- p
		y.loadings[,i] <- q
		projection[,i] <- r
		basis[,i] <- v
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
	if ( verbose )
		message("fitting regression coefficients")
	b <- tcrossprod(projection, y.loadings)
	if ( transpose ) {
		yhat <- t(t(b) %*% xt)
	} else {
		yhat <- x %*% b
	}
	if ( is.numeric(y.scale) )
		yhat <- colsweep_matrix(yhat, y.scale, "*")
	if ( is.numeric(y.center) )
		yhat <- colsweep_matrix(yhat, y.center, "+")
	# return results
	if ( retscores ) {
		ans <- list(coefficients=b, projection=projection, residuals=y - yhat,
			fitted.values=yhat, loadings=loadings, scores=scores,
			y.loadings=y.loadings, y.scores=y.scores, cvar=cvar)
	} else {
		ans <- list(coefficients=b, projection=projection, residuals=y - yhat,
			fitted.values=yhat, loadings=loadings,
			y.loadings=y.loadings)
	}
	ans$transpose <- transpose
	ans$center <- if(is.null(center)) FALSE else center
	ans$scale <- if(is.null(scale)) FALSE else scale
	ans$y.center <- if(is.null(y.center)) FALSE else y.center
	ans$y.scale <- if(is.null(y.scale)) FALSE else y.scale
	ans$algorithm <- "simpls"
	class(ans) <- "pls"
	ans
}

# Kernel (#1 and #2)
pls_kernel <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, method = 1L, retscores = TRUE,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(max(k), dim(x))
	# center and scale x and y + calculate covariance
	if ( verbose && (center || scale.) )
		message("preparing x and y matrices")
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.,
		verbose=verbose, BPPARAM=BPPARAM, ...)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
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
		x <- colscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
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
		cvar <- set_names(numeric(k), paste0("C", j))
	}
	for ( i in j )
	{
		# show progress
		if ( verbose )
			message("fitting PLS component ", i)
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
			matter_error("unrecognized kernel method: ", method)
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
	if ( verbose )
		message("fitting regression coefficients")
	b <- tcrossprod(projection, y.loadings)
	if ( transpose ) {
		yhat <- t(t(b) %*% xt)
	} else {
		yhat <- x %*% b
	}
	if ( is.numeric(y.scale) )
		yhat <- colsweep_matrix(yhat, y.scale, "*")
	if ( is.numeric(y.center) )
		yhat <- colsweep_matrix(yhat, y.center, "+")
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
	ans$algorithm <- paste0("kern", method)
	class(ans) <- "pls"
	ans
}

fitted.pls <- function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	if ( type == "class" ) {
		predict_class(object$fitted.values)
	} else {
		object$fitted.values
	}
}

predict.pls <- function(object, newdata, k,
	type = c("response", "class"), simplify = TRUE, ...)
{
	type <- match.arg(type)
	if ( missing(newdata) && missing(k) )
		return(fitted(object, type=type))
	if ( missing(newdata) )
		matter_error("'newdata' must be specified if 'k' is specified")
	if ( missing(k) )
		k <- ncol(object$loadings)
	if ( length(dim(newdata)) != 2L )
		matter_error("'newdata' must be a matrix or data frame")
	nm <- rownames(object$loadings)
	v <- if (object$transpose) rownames(newdata) else colnames(newdata)
	p <- if (object$transpose) nrow(newdata) else ncol(newdata)
	if ( !is.null(nm) ) {
		if ( !all(nm %in% v) )
			matter_error("'newdata' does not have named features ",
				"matching one of more of the original features")
		if ( object$transpose ) {
			newdata <- newdata[nm,,drop=FALSE]
		} else {
			newdata <- newdata[,nm,drop=FALSE]
		}
	} else {
		if ( p != nrow(object$loadings) )
			matter_error("'newdata' does not have the correct number of features")
	}
	if ( object$transpose ) {
		xt <- rowscale(newdata, center=object$center, scale=object$scale)
	} else {
		x <- colscale(newdata, center=object$center, scale=object$scale)
	}
	
	if ( any(k > ncol(object$loadings)) )
		matter_error("'k' is larger than the number of components")
	# predict for each k
	ans <- lapply(set_names(k, paste0("k=", k)),
		function(ki)
		{
			# extract relevant components
			projection <- object$projection[,1:ki,drop=FALSE]
			y.loadings <- object$y.loadings[,1:ki,drop=FALSE]
			# calculate regression coefficients
			b <- tcrossprod(projection, y.loadings)
			# predict new values
			if ( object$transpose ) {
				pred <- t(t(b) %*% xt)
			} else {
				pred <- x %*% b
			}
			if ( is.numeric(object$y.scale) )
				pred <- colsweep_matrix(pred, object$y.scale, "*")
			if ( is.numeric(object$y.center) )
				pred <- colsweep_matrix(pred, object$y.center, "+")
			if ( type == "class" )
				pred <- predict_class(pred)
			pred
		})
	if ( simplify ) {
		if ( length(ans) > 1L ) {
			if ( type == "class" ) {
				as.data.frame(ans, check.names=FALSE)
			} else {
				simplify2array(ans)
			}
		} else {
			ans[[1L]]
		}
	} else {
		ans
	}
}

print.pls <- function(x, ...)
{
	cat(sprintf("Partial least squares (k=%d)\n", ncol(x$loadings)))
	if ( !is.null(x$cvar) ) {
		cat(sprintf("\nCovariances (1, .., k=%d):\n", length(x$cvar)))
		preview_vector(x$cvar, ...)
	}
	if ( !is.null(coef(x)) ) {
		cat("\nCoefficients:\n")
		preview_matrix(t(coef(x)), ...)
	} else {
		cat("\nNo coefficients\n")
	}
	invisible(x)
}

vip <- function(object, type = c("projection", "weights"))
{
	type <- match.arg(type)
	w0 <- object[[type]]
	if ( is.null(w0) )
		matter_error("missing component: ", sQuote(type))
	P <- nrow(w0)
	scores <- object$scores
	y.loadings <- object$y.loadings
	if ( is.null(scores) )
		matter_error("missing component: 'scores'")
	if ( is.null(y.loadings) )
		matter_error("missing component: 'y.loadings'")
	w0 <- colsweep_matrix(w0, sqrt(colSums(w0^2)), "/")
	ssy <- colSums(scores^2) * colSums(y.loadings^2)
	vip <- sqrt(P * rowSums(ssy * w0^2) / sum(ssy))
	names(vip) <- rownames(w0)
	vip
}

#### Orthogonal partial least squares ####
## ---------------------------------------

# NIPALS
opls_nipals <- function(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-9, regression = TRUE,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	y_in <- y
	x <- as_real_memory_matrix(x)
	k <- min(max(k), dim(x))
	# center and scale x and y
	if ( verbose && (center || scale.) )
		message("preparing x and y matrices")
	if ( is.factor(y) || is.character(y) )
		y <- encode_dummy(y)
	y <- as.matrix(y)
	y <- colscale(y, center=center, scale=scale.,
		verbose=verbose, BPPARAM=BPPARAM, ...)
	y.center <- attr(y, "col-scaled:center")
	y.scale <- attr(y, "col-scaled:scale")
	if ( transpose ) {
		P <- nrow(x)
		N <- ncol(x)
		pnames <- rownames(x)
		snames <- colnames(x)
		xt <- rowscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(xt, "row-scaled:center")
		scale <- attr(xt, "row-scaled:scale")
		xt0 <- xt
	} else {
		P <- ncol(x)
		N <- nrow(x)
		pnames <- colnames(x)
		snames <- rownames(x)
		x <- colscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
		x0 <- x
	}
	# prepare matrices
	j <- seq_len(k)
	regressions <- vector("list", max(j))
	names(regressions) <- paste0("k=", j)
	weights <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	loadings <- matrix(nrow=P, ncol=k,
		dimnames=list(pnames, paste0("C", j)))
	scores <- matrix(nrow=N, ncol=k,
		dimnames=list(snames, paste0("C", j)))
	ratio <- set_names(numeric(k), paste0("C", j))
	y0 <- y
	if ( transpose ) {
		w0 <- colsweep_matrix((xt %*% y), colSums(y^2), "/")
	} else {
		w0 <- colsweep_matrix(crossprod(x, y), colSums(y^2), "/")
	}
	tw <- prcomp(w0, center=FALSE, tol=tol)$x
	for ( i in j )
	{
		# show progress
		if ( verbose )
			message("fitting OPLS component ", i)
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
			matter_warn("NIPALS did not converge in ",
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
			if ( regression ) {
				regressions[[i]] <- pls_nipals(xt, y=y_in, k=1L,
					center=FALSE, scale.=FALSE,
					transpose=TRUE, niter=niter, tol=tol,
					verbose=verbose, BPPARAM=BPPARAM, ...)
			}
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
			if ( regression ) {
				regressions[[i]] <- pls_nipals(x, y=y_in, k=1L,
					center=FALSE, scale.=FALSE,
					transpose=FALSE, niter=niter, tol=tol,
					verbose=verbose, BPPARAM=BPPARAM, ...)
			}
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
	ans$regressions <- regressions
	ans$algorithm <- "nipals"
	class(ans) <- "opls"
	ans
}

coef.opls <- function(object, ...)
{
	if ( is.null(object$regressions) )
		matter_error("missing component: 'regressions'")
	k <- length(object$regressions)
	coef(object$regressions[[k]], ...)
}

residuals.opls <- function(object, ...)
{
	if ( is.null(object$regressions) )
		matter_error("missing component: 'regressions'")
	k <- length(object$regressions)
	residuals(object$regressions[[k]], ...)
}

fitted.opls <- function(object, type = c("response", "class", "x"), ...)
{
	type <- match.arg(type)
	if ( type != "x" ) {
		if ( is.null(object$regressions) )
			matter_error("missing component: 'regressions'")
		k <- length(object$regressions)
		fitted(object$regressions[[k]], type=type, ...)
	} else {
		object$x
	}
}

predict.opls <- function(object, newdata, k,
	type = c("response", "class", "x"), simplify = TRUE, ...)
{
	type <- match.arg(type)
	if ( missing(newdata) && missing(k) )
		return(fitted(object, type=type))
	if ( type != "x" && is.null(object$regressions) )
		matter_error("missing component: 'regressions'")
	if ( missing(newdata) )
		matter_error("'newdata' must be specified if 'k' is specified")
	if ( missing(k) )
		k <- ncol(object$loadings)
	if ( length(dim(newdata)) != 2L )
		matter_error("'newdata' must be a matrix or data frame")
	nm <- rownames(object$loadings)
	v <- if (object$transpose) rownames(newdata) else colnames(newdata)
	p <- if (object$transpose) nrow(newdata) else ncol(newdata)
	if ( !is.null(nm) ) {
		if ( !all(nm %in% v) )
			matter_error("'newdata' does not have named features ",
				"matching one of more of the original features")
		if ( object$transpose ) {
			newdata <- newdata[nm,,drop=FALSE]
		} else {
			newdata <- newdata[,nm,drop=FALSE]
		}
	} else {
		if ( p != nrow(object$loadings) )
			matter_error("'newdata' does not have the correct number of features")
	}
	# extract relevant components
	if ( any(k > ncol(object$loadings)) )
		matter_error("'k' is larger than the number of components")
	ans <- lapply(set_names(k, paste0("k=", k)),
		function(ki)
		{
			weights <- object$weights[,1:ki,drop=FALSE]
			loadings <- object$loadings[,1:ki,drop=FALSE]
			# predict new values
			if ( object$transpose ) {
				xt <- rowscale(newdata, center=object$center, scale=object$scale)
				for ( i in seq_len(ki) ) {
					to <- t(t(weights[,i]) %*% xt) / sum(weights[,i]^2)
					xt <- xt - tcrossprod(loadings[,i], to)
				}
				if ( type != "x" ) {
					predict(object$regressions[[ki]], newdata=xt,
						type=type, simplify=TRUE, ...)
				} else {
					xt
				}
			} else {
				x <- colscale(newdata, center=object$center, scale=object$scale)
				for ( i in seq_len(ki) ) {
					to <- (x %*% weights[,i]) / sum(weights[,i]^2)
					x <- x - tcrossprod(to, loadings[,i])
				}
				if ( type != "x" ) {
					predict(object$regressions[[ki]], newdata=x,
						type=type, simplify=TRUE, ...)
				} else {
					x
				}
			}
		})
	if ( simplify ) {
		if ( length(ans) > 1L ) {
			simplify2array(ans)
		} else {
			ans[[1L]]
		}
	} else {
		ans
	}
}

print.opls <- function(x, print.x = FALSE, ...)
{
	cat(sprintf("Orthogonal partial least squares (k=%d)\n", ncol(x$loadings)))
	cat(sprintf("\nComponent ratios (1, .., k=%d):\n", length(x$ratio)))
	preview_vector(x$ratio, ...)
	if ( print.x && !is.null(x$x) ) {
		cat("\nProcessed variables:\n")
		preview_matrix(x$x, ...)
	}
	if ( !is.null(coef(x)) ) {
		cat("\nCoefficients:\n")
		preview_matrix(t(coef(x)), ...)
	} else {
		cat("\nNo coefficients\n")
	}
	invisible(x)
}
