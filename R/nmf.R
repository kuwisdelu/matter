
#### Nonnegative matrix factorization ####
## ---------------------------------------

# Lee and Seung (2000)
nmf_mu <- function(x, k = 3L, method = c("euclidean", "KL", "IS"),
	transpose = FALSE, niter = 100L, tol = 1e-5, verbose = NA, ...)
{
	method <- match.arg(method)
	if ( method != "euclidean" )
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
		if ( method == "euclidean" ) {
			hup <- crossprod(w, x) / (crossprod(w, w) %*% h)
		} else {
			wh <- w %*% h
			if ( method == "KL" ) {
				hup <- rowsweep_matrix(crossprod(w, x / wh), colSums(w), "/")
			} else if ( method == "IS" ) {
				hup <- crossprod(w, x / wh^2) / crossprod(w, 1 / wh)
			} else {
				stop("unsupported method: ", sQuote(method))
			}
		}
		hnew <- h * hup
		dh <- sqrt(sum((hnew - h)^2))
		h <- hnew
		# update W
		if ( method == "euclidean" ) {
			wup <- tcrossprod(x, h) / (w %*% tcrossprod(h, h))
		} else {
			wh <- w %*% h
			if ( method == "KL" ) {
				wup <- colsweep_matrix(tcrossprod(x / wh, h), rowSums(h), "/")
			} else if ( method == "IS" ) {
				wup <- tcrossprod(x / wh^2, h) / tcrossprod(1 / wh, h)
			} else {
				stop("unsupported method: ", sQuote(method))
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
		ans <- list(activation=w, x=t(h), method=method, iter=iter - 1L)
		dimnames(ans$activation) <- list(rownames(x), paste0("C", j))
		dimnames(ans$x) <- list(colnames(x), paste0("C", j))
	} else {
		ans <- list(activation=t(h), x=w, method=method, iter=iter - 1L)
		dimnames(ans$activation) <- list(colnames(x), paste0("C", j))
		dimnames(ans$x) <- list(rownames(x), paste0("C", j))
	}
	class(ans) <- "nmf"
	ans
}

print.nmf <- function(x, print.x = FALSE, ...)
{
	d <- dim(x$activation)
	cat(sprintf("Activation (n x k) = (%d x %d):\n", d[1], d[2]))
	print(x$activation, ...)
	if ( print.x && length(x$x) ) {
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
		up <- (u >= 0) * u
		un <- (u < 0) * u
		vp <- (v >= 0) * v
		vn <- (v < 0) * v
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
