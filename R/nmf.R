
#### Nonnegative matrix factorization ####
## ---------------------------------------

# Lee and Seung (2000)
nmf_mu <- function(x, n = 3L, niter = 100L,
	tol = 1e-5, verbose = TRUE, ...)
{
	init <- nndsvd(x, n=n, ...)
	w <- init$w
	h <- init$h
	dw <- dh <- Inf
	iter <- 1L
	while ( iter <= niter && (dw > tol || dh > tol) )
	{
		hup <- crossprod(w, x) / (crossprod(w, w) %*% h)
		hnew <- h * hup
		dh <- sqrt(sum((hnew - h)^2))
		h <- hnew
		wup <- tcrossprod(x, h) / (w %*% tcrossprod(h, h))
		wnew <- w * wup
		dw <- sqrt(sum((wnew - w)^2))
		w <- wnew
		if ( verbose ) {
			sdw <- format.default(dw)
			sdh <- format.default(dh)
			message("iteration ", iter, ": ",
				"diff(W) = ", sdw, ", ",
				"diff(H) = ", sdh)
		}
		iter <- iter + 1L
	}
	list(w=w, h=h, iter=iter)
}

# Boutsidis and Gallopoulos (2008)
nndsvd <- function(x, n = 3L, ...)
{
	w <- matrix(0, nrow=nrow(x), ncol=n)
	h <- matrix(0, nrow=n, ncol=ncol(x))
	s <- irlba(x, nu=n, nv=n, ...)
	w[,1L] <- sqrt(s$d[1L]) * abs(s$u[,1L])
	h[1L,] <- sqrt(s$d[1L]) * abs(s$v[,1L])
	if ( n == 1L )
		return(list(w=w, h=h))
	for ( i in 2L:n ) {
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

