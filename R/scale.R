
#### Scaling for matter matrices ####
## ----------------------------------

setMethod("scale", "matter_mat", function(x, center=TRUE, scale=TRUE)
{
	if ( datamode(x) != "numeric" )
		datamode(x) <- "numeric"
	by_which_dim <- switch(class(x),
		matter_matc = "by_each_group",
		matter_matr = "by_group")
	if ( is.logical(center) ) {
		if ( center ) {
			center <- colMeans(x, na.rm=TRUE)
			x <- register_op(x, NULL, center, "-", by_which_dim)
		}
	} else if ( is.numeric(center) && length(center) == ncol(x) ) {
		x <- register_op(x, NULL, center, "-", by_which_dim)
	} else {
		stop("length of 'center' must equal the number of columns of 'x'")
	}
	if ( is.logical(scale) ) {
		if ( scale ) {
			scale <- sqrt(colSums(x^2, na.rm=TRUE) / max(1, nrow(x) - 1L))
			x <- register_op(x, NULL, scale, "/", by_which_dim)
		}
	} else if ( is.numeric(scale) && length(scale) == ncol(x) ) {
		x <- register_op(x, NULL, scale, "/", by_which_dim)
	} else if ( !is.null(scale) ) {
		stop("length of 'center' must equal the number of columns of 'x'")
	}
	if ( is.numeric(center) )
		attr(x, "scaled:center") <- center
	if ( is.numeric(scale) )
		attr(x, "scaled:scale") <- scale
	x
})

