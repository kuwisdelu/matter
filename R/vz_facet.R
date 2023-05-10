
#### Define faceting for 'vizi' ####
## ---------------------------------

setClass("vz_facet",
	slots = c(
		encoding = "list_OR_NULL",
		levels = "list_OR_NULL",
		subscripts = "list_OR_NULL",
		dim = "numeric",
		grid = "logical",
		byrow = "logical",
		free = "character_OR_NULL",
		labels = "character_OR_NULL",
		drop = "logical"))

setClassUnion("facet_OR_NULL", c("vz_facet", "NULL"))

facet <- function(plot, facets = NULL, nrow = NA, ncol = NA,
		grid = FALSE, byrow = !grid, free = NULL,
		labels = NULL, drop = TRUE)
{
	if ( !inherits(plot, "vz_graphic") )
		stop("'plot' must inherit from 'vz_graphic'")
	if ( is(facets, "formula") ) {
		e <- environment(facets)
		grid <- length(facets) == 3L
		f <- parse_formula(facets)
		if ( grid ) {
			facets <- c(f$lhs, f$rhs)
		} else {
			facets <- f$rhs
		}
		attr(facets, "env") <- e
	} else {
		if ( is.character(facets) && is.null(names(facets)) )
			names(facets) <- facets
		if ( isTRUE(grid) && length(facets) != 2L )
			stop("facets must be length-2 when grid = TRUE")
		facets <- as.list(facets)
		attr(facets, "env") <- parent.frame(1)
	}
	dim <- as.integer(c(nrow, ncol))
	plot@facet <- new("vz_facet", facets=facets, levels=NULL,
		subscripts=NULL, dim=dim, grid=grid, byrow=byrow,
		free=free, labels=labels, drop=drop)
	plot
}
