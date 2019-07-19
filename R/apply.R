
#### Apply functions over matter matrices ####
## -------------------------------------------

setMethod("apply", "matter_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		apply_matter(X, MARGIN, FUN, ..., BPPARAM=BPPARAM)
})

setMethod("apply", "sparse_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		apply_matter(X, MARGIN, FUN, ..., BPPARAM=BPPARAM)
})

setMethod("apply", "virtual_mat",
	function(X, MARGIN, FUN, ..., BPPARAM = bpparam()) {
		apply_matter(X, MARGIN, FUN, ..., BPPARAM=BPPARAM)
})

#### List-Apply functions over matter lists ####
## -------------------------------------------

setMethod("lapply", "matter_list",
	function(X, FUN, ..., BPPARAM = bpparam())
	{
		mapfun_matter(X, FUN, ..., SIMPLIFY=FALSE,
			USE.NAMES=TRUE, BPPARAM=BPPARAM)
	}
)

setMethod("sapply", "matter_list",
	function(X, FUN, ..., BPPARAM = bpparam(),
		simplify = TRUE, USE.NAMES = TRUE)
	{
		mapfun_matter(X, FUN, ..., SIMPLIFY=simplify,
			USE.NAMES=USE.NAMES, BPPARAM=BPPARAM)
	}
)

# lapply and sapply

mapfun_matter <- function(X, FUN, ..., SIMPLIFY, USE.NAMES, BPPARAM)
{
	FUN <- match.fun(FUN)
	len <- length(X)
	ans <- bplapply(1:len, function(i) FUN(X[[i]], ...), BPPARAM=BPPARAM)
	if ( USE.NAMES && !is.null(names(X)) )
		names(ans) <- names(X)
	if ( USE.NAMES && is.character(X) && is.null(names(ans)) )
		names(ans) <- X
	if ( !isFALSE(SIMPLIFY) && length(ans) ) {
		simplify2array(ans, higher = (SIMPLIFY == "array"))
	} else {
		ans
	}
}

# apply based on base::apply and biganalytics::apply

apply_matter <- function(X, MARGIN, FUN, ..., BPPARAM)
{
	if ( length(MARGIN) > 1 ) 
		stop("dim(MARGIN) > 1 not supported for 'matter' objects")
	FUN <- match.fun(FUN)
	dn.ans <- dimnames(X)[MARGIN]
	if ( MARGIN == 1 ) {
		d2 <- nrow(X)
		ans <- bplapply(1:d2, function(i) FUN(X[i,], ...), BPPARAM=BPPARAM)
	} else if ( MARGIN == 2 ) {
		d2 <- ncol(X)
		ans <- bplapply(1:d2, function(i) FUN(X[,i], ...), BPPARAM=BPPARAM)
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


