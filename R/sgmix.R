
#### Spatial Gaussian mixture model ####
## --------------------------------------

sgmix <- function(x, y, vals, r = 1, k = 2,
	weights = c("gaussian", "bilateral", "adaptive"),
	metric = "maximum", p = 2, neighbors = NULL,
	annealing = TRUE, epsilon = 1e-3, niter = 10L,
	verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.matrix(x) && missing(y) && missing(vals) ) {
		dim <- dim(x)
		co <- as.matrix(expand.grid(
			x=seq_len(nrow(x)),
			y=seq_len(ncol(x))))
		x <- as.vector(x)
	} else {
		dim <- NULL
		if ( missing(y) && missing(vals) ) {
			co <- NULL
			if ( !is.list(weights) && !is.list(neighbors) )
				stop("both 'weights' and 'neighbors' must be ",
					"specified when x/y coordinates are not")
		} else {
			co <- cbind(x, y)
			x <- as.vector(vals)
		}
	}
	xmin <- min(x, na.rm=TRUE)
	xmax <- max(x, na.rm=TRUE)
	# find neighboring pixels
	if ( is.null(neighbors) ) {
		nb <- kdsearch(co, co, tol=r)
		d1 <- rowdist_at(co, ix=seq_len(nrow(co)), iy=nb, metric=metric, p=p)
		nb <- Map(function(i, d) i[d <= r], nb, d1)
	} else {
		nb <- rep_len(neighbors, length(x))
	}
	# find neighboring pixel weights
	if ( is.character(weights) ) {
		weights <- match.arg(weights)
		d2 <- rowdist_at(co, ix=seq_len(nrow(co)), iy=nb, metric="euclidean")
		a <- ((2 * r) + 1) / 4
		wts <- lapply(d2, function(d) exp(-d^2 / (2 * a^2)))
		if ( weights != "gaussian" ) {
			d3 <- Map(function(xi, i) abs(xi - x[i]), x, nb)
			if ( weights == "adaptive" ) {
				bs <- vapply(d3, function(d) (max(d) / 2)^2, numeric(1L))
			} else {
				bs <- rep_len(mad(x, na.rm=TRUE), length(x))
			}
			awts <- Map(function(d, b) exp(-d^2 / (2 * b^2)), d3, bs)
			awts <- lapply(awts, function(w) replace(w, is.na(w), 1))
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(x))
	}
	wts <- lapply(wts, function(w) w / sum(w, na.rm=TRUE))
	# initialize parameters (mu, sigma, alpha, beta)
	init <- kmeans(x, centers=k, ...)
	mu <- as.vector(init$centers)
	sigma <- as.vector(tapply(x, init$cluster, sd))
	alpha <- rep.int(1, k)
	beta <- 1
	# initialize p(x|mu,sigma)
	px <- matrix(0, nrow=length(x), ncol=k)
	for ( i in seq_len(k) )
		px[,i] <- (1 / sigma[i]) * exp(-(x - mu[i])^2 / (2 * sigma[i]^2))
	y <- px / rowSums(px)
	class <- predict_class(y)
	# expectation step
	stepE <- function(y, mu, sigma, alpha, beta, ...)
	{
		# compute posterior probability p(z|neighbors)
		ybar <- apply(y, 2L, convolve_at,
			index=nb, weights=wts, na.rm=TRUE)
		# update p(x|mu,sigma)
		px <- matrix(0, nrow=length(x), ncol=k)
		for ( i in seq_len(k) )
			px[,i] <- (1 / sigma[i]) * exp(-(x - mu[i])^2 / (2 * sigma[i]^2))
		px <- px / rowSums(px, na.rm=TRUE)
		# update prior probability
		priors <- t(alpha * t(ybar)^beta)
		priors <- priors / rowSums(priors, na.rm=TRUE)
		# update posterior probability p(z)
		y <- px * priors
		# compute log-likelihood
		loglik <- sum(log1p(rowSums(pmax(y, 0))), na.rm=TRUE)
		y <- y / rowSums(y, na.rm=TRUE)
		list(y=y, ybar=ybar, loglik=loglik)
	}
	# maximization step
	stepM <- function(eta, y, ybar, mu, sigma, alpha, beta, ...)
	{
		# initialize gradient
		gr <- list(
			mu=rep.int(1, k),
			sigma=rep.int(1, k),
			alpha=rep.int(1, k),
			beta=1)
		# compute gradient
		gr$mu <- rowSums(t(y) * (mu - rep(x, each=k)) / sigma^2, na.rm=TRUE)
		c1 <- 1 / sigma
		c2 <- (mu - rep(x, each=k))^2 / sigma^3
		gr$sigma <- rowSums(t(y) * (c1 - c2), na.rm=TRUE)
		c1 <- -rowSums(2 * t(y) / alpha, na.rm=TRUE)
		c2 <- colSums(y * ybar^beta, na.rm=TRUE)
		c3 <- rowSums(alpha^2 * t(ybar^beta), na.rm=TRUE)
		gr$alpha <- c1 + 2 * alpha * sum(c2 / c3)
		c1 <- alpha^2 * t(ybar^beta)
		c2 <- c1 * t(log1p(ybar))
		c3 <- colSums(c2, na.rm=TRUE) / colSums(c1, na.rm=TRUE)
		gr$beta <- sum(y * (-log1p(ybar) + c3), na.rm=TRUE)
		# find step size limits
		limits <- range(c(0, abs(sigma / gr$sigma),
			abs(alpha / gr$alpha), abs(beta / gr$beta)), na.rm=TRUE)
		# update parameters
		list(
			mu=mu - eta * gr$mu,
			sigma=sigma - eta * gr$sigma,
			alpha=alpha - eta * gr$alpha,
			beta=beta - eta * gr$beta,
			limits=limits)
	}
	# iterate
	tt <- 1
	for ( iter in seq_len(niter) )
	{
		# update probability from expectation step
		E <- stepE(y, mu=mu, sigma=sigma, alpha=alpha, beta=beta)
		ybar <- E$ybar
		y <- E$y
		loglik <- E$loglik
		if ( all(class == predict_class(y)) ) {
			if ( verbose )
				message("no more class assignment changes")
			break
		}
		class <- predict_class(y)
		# set up objective function
		fn <- function(eta, ...) {
			m <- stepM(eta, ...)
			e <- stepE(y, mu=m$mu, sigma=m$sigma, alpha=m$alpha, beta=m$beta)
			if ( is.finite(e$loglik) ) {
				-e$loglik
			} else {
				.Machine$double.xmax
			}
		}
		# find optimal gradient descent step size
		eta <- stepM(0, y=y, ybar=ybar,
			mu=mu, sigma=sigma, alpha=alpha, beta=beta)
		G <- optimize(fn, eta$limits, y=y, ybar=ybar,
			mu=mu, sigma=sigma, alpha=alpha, beta=beta)
		if ( verbose )
			message("log Lik = ",
				format.default(-G$objective), " on iteration ", iter,
				" (step size = ", format.default(G$minimum), ")")
		if ( annealing || loglik > -G$objective )
		{
			# simulate annealing
			i <- sample(k)
			sa_mu <- mu
			sa_mu[i] <- rnorm(1L, mean=mu[i], sd=tt * sigma[i])
			S <- stepE(y, mu=sa_mu, sigma=sigma, alpha=alpha, beta=beta)
			tt <- tt - (1 / niter)
			if ( S$loglik > -G$objective && S$loglik > loglik )
			{
				if ( verbose )
					message("log Lik = ",
						format.default(S$loglik), " on iteration ", iter,
						" (simulated annealing)")
				mu <- sa_mu
				next
			}
			if ( loglik > -G$objective ) {
				if ( verbose )
					message("log Lik decreased; reverting to previous model")
				break
			}
		}
		# update parameters from maximization step
		M <- stepM(G$minimum, y=y, ybar=ybar,
			mu=mu, sigma=sigma, alpha=alpha, beta=beta)
		if ( any(c(M$sigma, M$alpha, M$beta) <= 0) || 
			any(M$mu < xmin) || any(M$mu > xmax) )
		{
			if ( verbose )
				message("constraining parameters; reverting to previous model")
			break
		}
		mu <- M$mu
		sigma <- M$sigma
		alpha <- M$alpha
		beta <- M$beta
		if ( abs(-loglik - G$objective) < epsilon )
			break
	}
	# re-order based on segment means
	ord <- order(mu)
	y <- y[,ord,drop=FALSE]
	mu <- mu[ord]
	sigma <- sigma[ord]
	alpha <- alpha[ord]
	# estimate final parameters and probabilities
	E <- stepE(y, mu=mu, sigma=sigma, alpha=alpha, beta=beta)
	y <- E$y
	colnames(y) <- seq_len(k)
	names(mu) <- seq_len(k)
	names(sigma) <- seq_len(k)
	names(alpha) <- seq_len(k)
	class <- predict_class(y)
	if ( !is.null(dim) ) {
		class <- as.integer(class)
		dim(class) <- dim
	}
	ans <- list(class=class, probability=y,
		mu=mu, sigma=sigma, alpha=alpha, beta=beta)
	ans$logLik <- loglik
	class(ans) <- "sgmix"
	ans
}

sgmixn <- function(x, y, vals, r = 1, k = 2, group = NULL,
	weights = c("gaussian", "bilateral", "adaptive"),
	metric = "maximum", p = 2, neighbors = NULL,
	verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.matrix(x) && missing(y) && missing(vals) ) {
		dim <- dim(x)
		co <- as.matrix(expand.grid(
			x=seq_len(nrow(x)),
			y=seq_len(ncol(x))))
		x <- as.vector(x)
	} else {
		dim <- NULL
		if ( missing(y) && missing(vals) ) {
			co <- NULL
			if ( !is.list(weights) && !is.list(neighbors) )
				stop("both 'weights' and 'neighbors' must be ",
					"specified when x/y coordinates are not")
		} else {
			co <- cbind(x, y)
			x <- as.vector(vals)
		}
	}
	if ( is.null(group) )
		group <- rep_len(1, length(x))
	# find neighboring pixels
	if ( is.null(neighbors) ) {
		nb <- kdsearch(co, co, tol=r)
		ds <- rowdist_at(co, ix=seq_len(nrow(co)), iy=nb, metric=metric, p=p)
		nb <- lapply(seq_along(x),
			function(i) {
				d_ok <- ds[[i]] <= r
				g_ok <- group[nb[[i]]] %in% group[i]
				nb[[i]][d_ok & g_ok]
			})
	} else {
		nb <- rep_len(neighbors, length(x))
	}
	# fit model for each group
	if ( is.factor(group) ) {
		gs <- levels(group)
	} else {
		gs <- sort(unique(group))
	}
	ans <- lapply(gs, function(g)
		{
			if ( verbose )
				message("fitting model for group ", g)
			i <- which(group %in% g)
			nbi <- lapply(nb[i], bsearch, table=i)
			if ( is.character(weights) ) {
				sgmix(co[i,1L], co[i,2L], x[i], r=r, k=k,
					weights=weights, neighbors=nbi, verbose=verbose, ...)
			} else {
				sgmix(x[i], r=r, k=k,
					weights=weights[i], neighbors=nbi, verbose=verbose, ...)
			}
		})
	# combine and return models
	mu <- unlist(lapply(ans, `[[`, "mu"))
	sigma <- unlist(lapply(ans, `[[`, "sigma"))
	alpha <- unlist(lapply(ans, `[[`, "alpha"))
	beta <- unlist(lapply(ans, `[[`, "beta"))
	names(mu) <- seq_along(mu)
	names(sigma) <- names(mu)
	names(alpha) <- names(mu)
	names(beta) <- gs
	y <- matrix(0, nrow=length(x), ncol=length(mu))
	for ( i in seq_along(gs) ) {
		ir <- which(group %in% gs[i])
		ic <- seq_len(k) + (i - 1L) * k
		y[ir,ic] <- ans[[i]]$probability
	}
	colnames(y) <- names(mu)
	class <- predict_class(y)
	if ( !is.null(dim) ) {
		class <- as.integer(class)
		dim(class) <- dim
	}
	loglik <- sum(unlist(lapply(ans, `[[`, "logLik")))
	ans <- list(class=class, probability=y,
		mu=mu, sigma=sigma, alpha=alpha, beta=beta)
	ans$group <- rep(gs, each=k)
	ans$loglik <- loglik
	class(ans) <- "sgmix"
	ans
}


print.sgmix <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
	cat(sprintf("Spatial Gaussian mixture model (k=%d)\n",
		length(x$mu)))
	cat("\nParameter estimates:\n")
	if ( is.null(x$group) ) {
		print(rbind(mu=x$mu, sigma=x$sigma), ...)
	} else {
		ans <- format(rbind(mu=x$mu, sigma=x$sigma), digits=digits, ...)
		ans <- rbind(ans, group=as.character(x$group))
		print(ans, quote=FALSE, right=TRUE)
	}
	invisible(x)
}

fitted.sgmix <- function(object,
	type = c("probability", "class", "mean"), ...)
{
	type <- match.arg(type)
	if ( type == "probability" ) {
		object$probability
	} else if ( type == "class" ) {
		object$class
	} else {
		x <- object$mu[as.integer(object$class)]
		if ( !is.null(dim(object$class)) ) {
			matrix(x, nrow=nrow(object$class), ncol=ncol(object$class))
		} else {
			x
		}
	}
}

logLik.sgmix <- function(object, ...)
{
	nobs <- length(object$class)
	p <- 3L * length(object$mu) + 1L
	structure(object$logLik, df=nobs - p, nobs=nobs,
		class="logLik")
}
