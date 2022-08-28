
#### Define drle class ####
## -------------------------

setClassUnion("integer_OR_numeric", c("integer", "numeric"))

setClass("drle",
	slots = c(
		values = "numeric",
		deltas = "numeric",
		lengths = "numeric"),
	prototype = prototype(
		values = integer(),
		deltas = integer(),
		lengths= integer()),
	validity = function(object) {
		errors <- NULL
		if ( typeof(object@values) != typeof(object@deltas) )
			errors <- c(errors, "'values' and 'deltas' must be the same data type")
		lens <- c(length(object@values), length(object@lengths), length(object@deltas))
		if ( length(unique(lens)) != 1 )
			errors <- c(errors, "'values', 'deltas', and 'lengths' must have the same length")
		if ( any(object@lengths < 1) )
			errors <- c(errors, "all 'lengths' must be positive")
		if ( is.null(errors) ) TRUE else errors
	})

drle <- function(x, cr_threshold = 0)
{
	if ( is.drle(x) )
		return(x)
	if ( !is.numeric(x) )
		stop("'x' must be a numeric vector")
	nruns <- .Call("C_numRuns", x, TRUE, PACKAGE="matter")
	comp_size <- 984 + (nruns * (2 * sizeof(typeof(x)) + sizeof("integer")))
	uncomp_size <- 48 + (length(x) * sizeof(typeof(x)))
	if ( uncomp_size / comp_size > cr_threshold ) {
		out <- .Call("C_encodeDRLE", x, PACKAGE="matter")
	} else {
		out <- x
	}
	if ( validObject(out) )
		out
}

setMethod("describe_for_display", "drle", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("compressed ", typeof(x@values), " vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("show", "drle", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	n <- getOption("matter.show.head.n")
	runs <- data.frame(
		values=object@values,
		deltas=object@deltas,
		lengths=object@lengths)
	print(head(runs, n=n))
	if ( nrow(runs) > n )
		cat("... and", nrow(runs) - n, "more runs\n")
})

is.drle <- function(x) is(x, "drle")

get_drle_elts <- function(x, i = NULL) {
	.Call("C_decodeDRLE", x, i, PACKAGE="matter")
}

subset_drle <- function(x, i = NULL) {
	.Call("C_recodeDRLE", x, i, PACKAGE="matter")
}

setAs("drle", "list", function(from)
	list(values=from@values, lengths=from@lengths, deltas=from@deltas))

setAs("drle", "vector", function(from) from[])

setMethod("as.list", "drle", function(x) as(x, "list"))

setMethod("as.vector", "drle", function(x) as(x, "vector"))

setMethod("[",
	c(x = "drle", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop)
	{
		if ( ...length() > 0 )
			stop("incorrect number of dimensions")
		if ( missing(i) )
			i <- NULL
		if ( is_nil(drop) ) {
			subset_drle(x, i)
		} else {
			get_drle_elts(x, i)
		}
	})

setMethod("length", "drle", function(x) sum(x@lengths))

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

setMethod("combine", c("drle", "numeric"),
	function(x, y, ...) combine(x, drle(y)))

setMethod("combine", c("numeric", "drle"),
	function(x, y, ...) combine(drle(x), y))

setMethod("c", "drle", function(x, ...)
{
	dots <- list(...)
	if ( length(dots) > 0 ) {
		y <- do.call(combine, list(x, ...))
	} else {
		y <- x
	}
	if ( validObject(y) )
		y
})

