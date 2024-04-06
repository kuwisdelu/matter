
#### Multiple instance learning ####
## ---------------------------------

mi_learn <- function(fn, x, y, group, pos = 1L,
	threshold = 0.01, verbose = NA, ...)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	y <- as.factor(y)
	if ( nlevels(y) != 2L )
		stop("y must have exactly 2 levels")
	if ( is.integer(pos) )
		pos <- levels(y)[pos]
	neg <- setdiff(levels(y), pos)
	if ( verbose )
		message("using ", sQuote(pos), " as positive class")
	group <- as.factor(group)
	if ( length(y) != length(group) ) {
		if ( length(y) != nlevels(group) ) {
			stop("length of y [", length(y), "] does not match ",
				"length of group [", length(group), "] ",
				"or its levels [", nlevels(group), "]")
		} else {
			yg <- y
			y <- y[as.integer(group)]
		}
	} else {
		y <- rep_len(y, length(group))
		yg <- lapply(levels(group),
			function(g) unique(y[!is.na(y) & group %in% g]))
		if ( any(lengths(yg) != 1L) ) {
			stop("labels must be homogenous within each group")
		} else {
			yg <- unlist(yg)
		}
	}
	iter <- 1
	uprop <- 1
	# multiple instance learning
	while ( uprop > threshold )
	{
		if ( verbose )
			message("multiple instance iteration ", iter)
		model <- fn(x, y, ...)
		yi <- fitted(model, "class")
		py <- fitted(model)
		if ( is.matrix(py) )
			py <- py[,1L,drop=TRUE]
		for ( j in seq_along(yg) )
		{
			g <- levels(group)[j]
			# set all negative bag labels to neg
			if ( all(yg %in% neg) )
			{
				yi[group %in% g] <- neg
				next
			}
			# update positive bag labels
			yj <- yi[group %in% g]
			if ( all(yj %in% neg) ) {
				imax <- which.max(py[group %in% g])
				yj[imax] <- pos
			}
			yi[group %in% g] <- yj
		}
		iter <- iter + 1
		utot <- sum(y != yi, na.rm=TRUE)
		uprop <- utot / sum(!is.na(y))
		if ( verbose )
			message(utot, " labels updated (",
				round(100 * uprop, digits=2L), "%)")
		y <- yi
	}
	fn(x, y, ...)
}

