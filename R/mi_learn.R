
#### Multiple instance learning ####
## ---------------------------------

mi_learn <- function(fn, x, y, bags, pos = 1L, ...,
	score = fitted, threshold = 0.01, verbose = NA)
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	y <- as.factor(y)
	if ( nlevels(y) != 2L )
		stop("y must have exactly 2 levels")
	if ( is.integer(pos) )
		pos <- levels(y)[pos]
	neg <- setdiff(levels(y), pos)
	ipos <- which(levels(y) %in% pos)
	if ( verbose )
		message("# using ", sQuote(pos), " as positive class")
	bags <- droplevels(as.factor(bags))
	if ( length(y) != length(bags) ) {
		if ( length(y) != nlevels(bags) ) {
			stop("length of y [", length(y), "] does not match ",
				"length of bags [", length(bags), "] ",
				"or its levels [", nlevels(bags), "]")
		} else {
			y_bags <- y
			y <- y[as.integer(bags)]
		}
	} else {
		y_bags <- lapply(levels(bags),
			function(bag) unique(y[!is.na(y) & bags %in% bag]))
		if ( any(lengths(y_bags) > 1L) ) {
			stop("labels must be homogenous within each bag")
		} else {
			y_bags <- unlist(y_bags)
		}
	}
	iter <- 1
	uprop <- 1
	# multiple instance learning
	while ( uprop > threshold )
	{
		if ( verbose )
			message("# multiple instance iteration ", iter)
		model <- fn(x, y, ...)
		py <- score(model)
		if ( is.matrix(py) ) {
			yi <- predict_class(py)
		} else {
			yi <- factor(ifelse(py > 0.5, pos, neg))
		}
		if ( is.matrix(py) )
			py <- py[,ipos,drop=TRUE]
		for ( j in seq_along(y_bags) )
		{
			bag <- which(bags %in% levels(bags)[j])
			# set all negative bag labels to neg
			if ( y_bags[j] == neg )
			{
				yi[bag] <- neg
				next
			}
			# update positive bag labels
			yj <- yi[bag]
			if ( all(yj %in% neg) )
			{
				# find instance with highest pos score
				imax <- which.max(py[bag])
				yj[imax] <- pos
			}
			yi[bag] <- yj
		}
		iter <- iter + 1
		utot <- sum(y != yi, na.rm=TRUE)
		uprop <- utot / sum(!is.na(y))
		if ( verbose )
			message("# ", utot, " labels updated (",
				round(100 * uprop, digits=2L), "%)")
		y <- yi
	}
	if ( verbose )
		message("# finalizing model")
	fn(x, y, ...)
}

