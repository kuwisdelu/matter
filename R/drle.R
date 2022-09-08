
#### Define drle class ####
## -------------------------

setClassUnion("numeric_OR_logical", c("numeric", "logical"))

setClass("drle",
	slots = c(
		values = "numeric_OR_logical",
		deltas = "numeric",
		lengths = "numeric"),
	validity = function(object) {
		errors <- NULL
		lens <- c(length(object@values), length(object@lengths), length(object@deltas))
		if ( length(unique(lens)) != 1 )
			errors <- c(errors, "'values', 'deltas', and 'lengths' must have the same length")
		if ( any(object@lengths < 1) )
			errors <- c(errors, "all 'lengths' must be positive")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("drle_fct",
	slots = c(levels = "character"),
	contains = "drle")

drle <- function(x, cr_threshold = 0)
{
	if ( is.drle(x) )
		return(x)
	if ( !is.numeric(x) && !is.logical(x) )
		x <- as.factor(x)
	y <- .Call("C_encodeDRLE", x, cr_threshold, PACKAGE="matter")
	if ( is.factor(x) && is.drle(y) )
		y <- new("drle_fct", y, levels=levels(x))
	if ( validObject(y) )
		y
}

setMethod("describe_for_display", "drle", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("compressed ", typeof(x@values), " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("describe_for_display", "drle_fct", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("compressed factor")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "drle", function(x) preview_vector(x))

setMethod("show", "drle", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	n <- getOption("matter.show.head.n")
	if ( getOption("matter.show.head") )
		try(preview_for_display(object), silent=TRUE)
	cat("with", length(object@values), "delta-encoded runs\n")
})

setMethod("show", "drle_fct", function(object) {
	callNextMethod()
	cat("Levels(", nlevels(object), "): ", sep="")
	cat(paste_head(object@levels), "\n")
})

is.drle <- function(x) is(x, "drle")

get_drle_elts <- function(x, i = NULL) {
	.Call("C_decodeDRLE", x, i, PACKAGE="matter")
}

subset_drle <- function(x, i = NULL) {
	.Call("C_recodeDRLE", x, i, PACKAGE="matter")
}

setAs("drle", "list", function(from)
	list(values=from@values,
		deltas=from@deltas,
		lengths=from@lengths))

setAs("drle", "vector", function(from) from[])

setAs("drle", "integer", function(from) as.integer(from[]))

setAs("drle", "numeric", function(from) as.numeric(from[]))

setAs("drle_fct", "factor", function(from) from[])

setMethod("as.vector", "drle", function(x) as(x, "vector"))

setMethod("as.integer", "drle", function(x) as(x, "integer"))

setMethod("as.numeric", "drle", function(x) as(x, "numeric"))

setMethod("as.factor", "drle_fct", function(x) as(x, "factor"))

setMethod("as.list", "drle", function(x) as(x, "list"))

setMethod("as.data.frame", "drle", function(x) as.data.frame(as.list(x)))

setMethod("[",
	c(x = "drle", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop)
	{
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		i <- as_subscripts(i, x)
		if ( is_nil(drop) ) {
			subset_drle(x, i)
		} else {
			get_drle_elts(x, i)
		}
	})

setMethod("[",
	c(x = "drle_fct", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop)
	{
		y <- callNextMethod()
		if ( is.numeric(y) )
			y <- factor(levels(x)[y], levels=levels(x))
		y
	})

setMethod("length", "drle", function(x) sum(x@lengths))

setMethod("levels", "drle_fct", function(x) x@levels)

setReplaceMethod("levels", "drle_fct",
	function(x, value) {
		x@levels <- value
		x
	})

setMethod("combine", c("drle", "drle"), function(x, y, ...) {
	n <- length(x@values)
	nextval <- x@values[n] + x@deltas[n] * x@lengths[n]
	if ( nextval == y@values[1] )
	{
		if ( x@deltas[n] == y@deltas[1] || y@lengths[1] == 1 )
		{
			x@lengths[n] <- x@lengths[n] + y@lengths[1]
			y@values <- y@values[-1]
			y@lengths <- y@lengths[-1]
			y@deltas <- y@deltas[-1]
		} else if ( y@lengths[1] == 2 && x@lengths[n] > 2 )
		{
			x@lengths[n] <- x@lengths[n] + 1L
			y@values[1] <- y@values[1] + y@deltas[1]
			y@lengths[1] <- 1L
			y@deltas[1] <- 0L
		}
	}
	new(class(x),
		values=c(x@values, y@values),
		lengths=c(x@lengths, y@lengths),
		deltas=c(x@deltas, y@deltas))
})

setMethod("combine", c("drle_fct", "drle_fct"), function(x, y, ...) {
	if ( isTRUE(all.equal(levels(x), levels(y))) ) {
		y <- combine(as(x, "drle"), as(y, "drle"))
		y <- new(class(x), y, levels=levels(x))
	} else {
		y <- c(as.factor(x), as.factor(y))
		y <- drle(y)
	}
	if ( validObject(y) )
		y
})

setMethod("combine", c("drle", "numeric"),
	function(x, y, ...) combine(x[], y))

setMethod("combine", c("numeric", "drle"),
	function(x, y, ...) combine(x, y[]))

setMethod("c", "drle", function(x, ...)
{
	if ( ...length() > 0 )
		x <- do.call(combine, list(x, ...))
	if ( validObject(x) )
		x
})

