
#### Principal components for matter matrices ####
## -----------------------------------------------

setMethod("prcomp", "matter_mat",
	function(x, n = 3, retx = TRUE, center = TRUE, scale. = FALSE, ...) {
		prcomp_int(x, n=n, retx=retx, center=center, scale.=scale., ...)
})

# based on code for prcomp_irlba from package:irlba

prcomp_int <- function(x, n, retx, center, scale., ...) {
    if ( "tol" %in% names(match.call(expand.dots=FALSE)$...) )
        warning("The 'tol' truncation argument from 'prcomp' is not supported\n",
        	"  for class 'matter_mat'. If specified, 'tol' is passed to 'irlba'\n",
        	"  to control that algorithm's convergence tolerance.")
    x <- colscale(x, center=center, scale=scale.)
    sv <- irlba(x, nu=n, nv=n, fastpath=FALSE, ...)
    ans <- list(sdev = sv$d/sqrt(max(1, nrow(x) - 1)), rotation = sv$v)
    colnames(ans$rotation) <- paste0("PC", seq(1, ncol(ans$rotation)))
    if ( !is.null(attr(x, "col-scaled:center")) ) {
    	ans$center <- attr(x, "scaled:center")
    } else {
    	ans$center <- FALSE
    }
    if ( !is.null(attr(x, "col-scaled:scale")) ) {
    	ans$scale <- attr(x, "scaled:scale")
    } else {
    	ans$scale <- FALSE
    }
    if ( retx ) {
        ans <- c(ans, list(x = sv$d * sv$u))
        colnames(ans$x) <- paste("PC", seq(1, ncol(ans$rotation)), sep = "")
    }
    class(ans) <- "prcomp"
    ans
}

