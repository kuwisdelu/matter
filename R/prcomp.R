
#### Principal components for matter matrices ####
## -----------------------------------------------

setMethod("prcomp", "matter_mat",
	function(x, k = 3L, retx = TRUE, center = TRUE, scale. = FALSE, ...)
{
	prcomp_lanczos(x, k=k, retx=retx, center=center, scale.=scale., ...)
})

setMethod("prcomp", "sparse_mat",
	function(x, k = 3L, retx = TRUE, center = TRUE, scale. = FALSE, ...)
{
	prcomp_lanczos(x, k=k, retx=retx, center=center, scale.=scale., ...)
})

prcomp_lanczos <- function(x, k = 3L, retx = TRUE,
	center = TRUE, scale. = FALSE, transpose = FALSE,
	verbose = NA, BPPARAM = bpparam(), ...)
{
	if ( "n" %in% ...names() ) {
		matter_warn("'n' is deprecated; use 'k' instead")
		k <- list(...)$n
	}
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	k <- min(max(k), dim(x))
	# center and scale x
	if ( !isFALSE(center) || !isFALSE(scale.) ) {
		msg <- character()
		if ( !isFALSE(center) )
			msg <- c(msg, "centering")
		if ( !isFALSE(scale.) )
			msg <- c(msg, "scaling")
		msg <- paste0(msg, collapse=" and ")
		matter_log(msg, " data matrix", verbose=verbose)
	}
	if ( transpose ) {
		x <- rowscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(x, "row-scaled:center")
		scale <- attr(x, "row-scaled:scale")
	} else {
		x <- colscale(x, center=center, scale=scale.,
			verbose=verbose, BPPARAM=BPPARAM, ...)
		center <- attr(x, "col-scaled:center")
		scale <- attr(x, "col-scaled:scale")
	}
	matter_log("fitting principal components via IRLBA", verbose=verbose)
	# calculate PCA via SVD
	j <- seq_len(k)
	s <- irlba(x, nu=k, nv=k, fastpath=is.matrix(x), verbose=verbose)
	if ( transpose ) {
		ans <- list(sdev = s$d / sqrt(max(1, ncol(x) - 1)))
		names(ans$sdev) <- paste0("PC", j)
		ans$rotation <- s$u
		dimnames(ans$rotation) <- list(rownames(x), paste0("PC", j))
	} else {
		ans <- list(sdev = s$d / sqrt(max(1, nrow(x) - 1)))
		names(ans$sdev) <- paste0("PC", j)
		ans$rotation <- s$v
		dimnames(ans$rotation) <- list(colnames(x), paste0("PC", j))
	}
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
	if ( retx ) {
		if ( transpose ) {
			ans$x <- s$v %*% diag(s$d)
			dimnames(ans$x) <- list(colnames(x), paste0("PC", j))
		} else {
			ans$x <- s$u %*% diag(s$d)
			dimnames(ans$x) <- list(rownames(x), paste0("PC", j))
		}
	}
	class(ans) <- c("prcomp_lanczos", "prcomp")
	ans
}

print.prcomp_lanczos <- function(x, print.x = FALSE, ...)
{
	cat(sprintf("Principal components (k=%d)\n", ncol(x$x)))
	if ( !is.null(x$sdev) ) {
		cat(sprintf("\nStandard deviations (1, .., k=%d):\n", length(x$sdev)))
		preview_vector(x$sdev, ...)
	}
	d <- dim(x$rotation)
	cat(sprintf("\nRotation (n x k) = (%d x %d):\n", d[1L], d[2L]))
	preview_matrix(x$rotation, ...)
	if ( print.x && !is.null(x$x) ) {
		cat("\nRotated variables:\n")
		preview_matrix(x$x, ...)
	}
	invisible(x)
}
