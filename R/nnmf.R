
#### Nonnegative matrix factorization ####
## ---------------------------------------

# Berry at al (2007)
nnmf_als <- function(x, k = 3L, s = 1e-9, transpose = FALSE,
	niter = 100L, tol = 1e-5, verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(k, dim(x))
	init <- nndsvd(x, k=k, ...)
	w <- init$w
	h <- init$h
	dw <- dh <- Inf
	iter <- 1L
	while ( iter <= niter && (dw > tol || dh > tol) )
	{
		# update H
		a <- crossprod(w, w) + s * diag(k)
		b <- crossprod(w, x)
		hnew <- solve(a, b)
		hnew <- hnew * (hnew >= 0)
		dh <- sqrt(sum((hnew - h)^2))
		h <- hnew
		# update W
		a <- tcrossprod(h, h) + s * diag(k)
		b <- tcrossprod(h, x)
		wnew <- t(solve(a, b))
		wnew <- wnew * (wnew >= 0)
		dw <- sqrt(sum((wnew - w)^2))
		w <- wnew
		# show progress
		if ( verbose ) {
			message("iteration ", iter, ": ",
				"diff(W) = ", format.default(dw), ", ",
				"diff(H) = ", format.default(dh))
		}
		iter <- iter + 1L
	}
	j <- seq_len(k)
	if ( transpose ) {
		ans <- list(activation=w, x=t(h), iter=iter - 1L)
		dimnames(ans$activation) <- list(rownames(x), paste0("C", j))
		dimnames(ans$x) <- list(colnames(x), paste0("C", j))
	} else {
		ans <- list(activation=t(h), x=w, iter=iter - 1L)
		dimnames(ans$activation) <- list(colnames(x), paste0("C", j))
		dimnames(ans$x) <- list(rownames(x), paste0("C", j))
	}
	ans$transpose <- transpose
	class(ans) <- "nnmf"
	ans
}

# Lee and Seung (2000)
nnmf_mult <- function(x, k = 3L, s = 1e-9, cost = c("euclidean", "KL", "IS"),
	transpose = FALSE, niter = 100L, tol = 1e-5, verbose = NA, ...)
{
	cost <- match.arg(cost)
	if ( cost != "euclidean" )
		x <- as_real_memory_matrix(x)
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(k, dim(x))
	init <- nndsvd(x, k=k, ...)
	w <- init$w
	h <- init$h
	dw <- dh <- Inf
	iter <- 1L
	while ( iter <= niter && (dw > tol || dh > tol) )
	{
		# update H
		if ( cost == "euclidean" ) {
			hup <- crossprod(w, x) / (s + crossprod(w, w) %*% h)
		} else {
			wh <- w %*% h
			if ( cost == "KL" ) {
				hup <- rowsweep_matrix(crossprod(w, x / wh), s + colSums(w), "/")
			} else if ( cost == "IS" ) {
				hup <- crossprod(w, x / wh^2) / (s + crossprod(w, 1 / wh))
			} else {
				stop("unsupported cost: ", sQuote(cost))
			}
		}
		hnew <- h * hup
		dh <- sqrt(sum((hnew - h)^2))
		h <- hnew
		# update W
		if ( cost == "euclidean" ) {
			wup <- tcrossprod(x, h) / (s + w %*% tcrossprod(h, h))
		} else {
			wh <- w %*% h
			if ( cost == "KL" ) {
				wup <- colsweep_matrix(tcrossprod(x / wh, h), s + rowSums(h), "/")
			} else if ( cost == "IS" ) {
				wup <- tcrossprod(x / wh^2, h) / (s + tcrossprod(1 / wh, h))
			} else {
				stop("unsupported cost: ", sQuote(cost))
			}
		}
		wnew <- w * wup
		dw <- sqrt(sum((wnew - w)^2))
		w <- wnew
		# show progress
		if ( verbose ) {
			message("iteration ", iter, ": ",
				"diff(W) = ", format.default(dw), ", ",
				"diff(H) = ", format.default(dh))
		}
		iter <- iter + 1L
	}
	j <- seq_len(k)
	if ( transpose ) {
		ans <- list(activation=w, x=t(h), cost=cost, iter=iter - 1L)
		dimnames(ans$activation) <- list(rownames(x), paste0("C", j))
		dimnames(ans$x) <- list(colnames(x), paste0("C", j))
	} else {
		ans <- list(activation=t(h), x=w, cost=cost, iter=iter - 1L)
		dimnames(ans$activation) <- list(colnames(x), paste0("C", j))
		dimnames(ans$x) <- list(rownames(x), paste0("C", j))
	}
	ans$transpose <- transpose
	class(ans) <- "nnmf"
	ans
}

predict.nnmf <- function(object, newdata, ...)
{
	if ( missing(newdata) )
		return(object$x)
	if ( length(dim(newdata)) != 2L )
		stop("'newdata' must be a matrix or data frame")
	nm <- rownames(object$activation)
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
		if ( p != nrow(object$activation) )
			stop("'newdata' does not have the correct number of features")
	}
	if ( object$transpose ) {
		x <- t(pinv(object$activation) %*% newdata)
	} else {
		x <- newdata %*% pinv(t(object$activation))
	}
	x * (x >= 0)
}

print.nnmf <- function(x, print.x = FALSE, ...)
{
	d <- dim(x$activation)
	cat(sprintf("Activation (n x k) = (%d x %d):\n", d[1L], d[2L]))
	print(x$activation, ...)
	if ( print.x ) {
		cat("\nBasis variables:\n")
		print(x$x, ...)
	}
	invisible(x)
}

# Boutsidis and Gallopoulos (2008)
nndsvd <- function(x, k = 3L, ...)
{
	w <- matrix(0, nrow=nrow(x), ncol=k)
	h <- matrix(0, nrow=k, ncol=ncol(x))
	s <- irlba(x, nu=k, nv=k, fastpath=is.matrix(x), ...)
	w[,1L] <- sqrt(s$d[1L]) * abs(s$u[,1L])
	h[1L,] <- sqrt(s$d[1L]) * abs(s$v[,1L])
	if ( k == 1L )
		return(list(w=w, h=h))
	for ( i in 2L:k ) {
		u <- s$u[,i]
		v <- s$v[,i]
		up <- u * (u >= 0)
		un <- u * (u < 0)
		vp <- v * (v >= 0)
		vn <- v * (v < 0)
		upnorm = sqrt(sum(up^2))
		unnorm = sqrt(sum(un^2))
		vpnorm = sqrt(sum(vp^2))
		vnnorm = sqrt(sum(vn^2))
		mp <- upnorm * vpnorm
		mn <- unnorm * vnnorm
		if ( mp > mn ) {
			u <- up / upnorm
			v <- vp / vpnorm
			sigma <- mp
		} else {
			u <- un / unnorm
			v <- vn / vnnorm
			sigma <- mn
		}
		w[,i] = sqrt(s$d[i] * sigma) * abs(u)
		h[i,] = sqrt(s$d[i] * sigma) * abs(v)
	}
	list(w=w, h=h)
}
