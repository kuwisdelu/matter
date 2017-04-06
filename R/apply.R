
#### Apply functions over matter matrices ####
## -------------------------------------------

setMethod("apply", "matter_mat",
	function(X, MARGIN, FUN, ...) {
		apply_matter(X, MARGIN, FUN, ...)
})

# based on code from package:base and package:biganalytics

apply_matter <- function(X, MARGIN, FUN, ...)
{
	if ( length(MARGIN) > 1 ) 
		stop("dim(MARGIN) > 1 not supported for 'matter' objects")
	FUN <- match.fun(FUN)
	dn.ans <- dimnames(X)[MARGIN]
	if ( MARGIN == 1 ) {
		d2 <- nrow(X)
		ans <- vector("list", nrow(X))
		for ( i in 1:d2 ) {
			tmp <- FUN(X[i,], ...)
			if (!is.null(tmp))
				ans[[i]] <- tmp
		}
	} else if ( MARGIN == 2 ) {
		d2 <- ncol(X)
		ans <- vector("list", ncol(X))
		for (i in 1:d2) {
			tmp <- FUN(X[,i], ...)
			if (!is.null(tmp))
				ans[[i]] <- tmp
		}
	} else {
		stop("only MARGIN = 1 or 2 supported for 'matter' objects")
	}
	ans.list <- is.recursive(ans[[1]])
	l.ans <- length(ans[[1]])
	ans.names <- names(ans[[1]])
	if ( !ans.list )
		ans.list <- any(unlist(lapply(ans, length)) != l.ans)
	if ( !ans.list && length(ans.names) ) {
		all.same <- sapply(ans, function(x) identical(names(x), ans.names))
		if (!all(all.same))
			ans.names <- NULL
	}
	if ( ans.list ) {
		len.a <- d2
	} else {
		len.a <- length(ans <- unlist(ans, recursive = FALSE))
	}
	if ( len.a == d2 ) {
		if ( length(dn.ans[[1]]) )
			names(ans) <- dn.ans[[1]]
		return(ans)
	}
	if ( len.a > 0 && len.a %% d2 == 0 ) {
		if ( is.null(dn.ans) )
			dn.ans <- vector(mode = "list", length(d2))
		dn.ans <- c(list(ans.names), dn.ans)
		return(array(ans, c(len.a%/%d2, d2), if (!all(sapply(dn.ans, 
			is.null))) dn.ans))
	}
	return(ans)
}


