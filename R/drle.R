
#### Define drle class ####
## -------------------------

setClassUnion("integer_OR_numeric", c("integer", "numeric"))

setClass("drle",
	slots = c(
		values = "integer_OR_numeric",
		lengths = "integer",
		deltas = "integer_OR_numeric"),
	prototype = prototype(
		values = integer(),
		lengths= integer(),
		deltas = integer()),
	validity = function(object) {
		errors <- NULL
		if ( typeof(object@values) != typeof(object@deltas) )
			errors <- c(errors, "'values' and 'deltas' must be of the same type")
		lens <- c(length(object@values), length(object@lengths), length(object@deltas))
		if ( length(unique(lens)) != 1 )
			errors <- c(errors, "'values', 'lengths', and 'deltas' must have the same length")
		if ( any(object@lengths < 1) )
			errors <- c(errors, "'lengths' must be positive")
		if ( is.null(errors) ) TRUE else errors
	})

drle <- function(x, cr_threshold = 0)
{
	if ( is.drle(x) )
		return(x)
	if ( !(is.integer(x) || is.numeric(x)) )
		stop("'x' must be an 'integer' or 'numeric' vector")
	nruns <- .Call("C_countRuns", x, PACKAGE="matter")
	comp_size <- nruns * (sizeof("integer") + 2 * sizeof(typeof(x)))
	uncomp_size <- length(x) * sizeof(typeof(x))
	if ( uncomp_size / comp_size > cr_threshold ) {
		out <- .Call("C_createDRLE", x, nruns, PACKAGE="matter")
	} else {
		out <- x
	}
	if ( validObject(out) )
		out
}

is.drle <- function(x) is(x, "drle")

getDRLE <- function(x) {
	.Call("C_getDRLE", x, PACKAGE="matter")
}

getDRLEElements <- function(x, i) {
	if ( is.logical(i) )
		i <- logical2index(x, i)
	i <- as.integer(i - 1)
	if ( length(i) > 1 && is.unsorted(i) ) {
		o <- order(i)
		y <- .Call("C_getDRLEElements", x, i[o], PACKAGE="matter")
		y[o] <- y
	} else {
		y <- .Call("C_getDRLEElements", x, i, PACKAGE="matter")
	}
	y
}

setAs("drle", "list", function(from)
	list(values=from@values, lengths=from@lengths, deltas=from@deltas))

setAs("drle", "vector", function(from) from[])

setMethod("as.list", "drle", function(x) as(x, "list"))

setMethod("as.vector", "drle", function(x) as(x, "vector"))

setMethod("[",
	c(x = "drle", i = "missing", j = "missing", drop = "missing"),
	function(x, ...) getDRLE(x))

setMethod("[",
	c(x = "drle", i = "ANY", j = "missing", drop = "missing"),
	function(x, i, ...) getDRLEElements(x, i))

setMethod("length", "drle", function(x) sum(x@lengths))

setMethod("combine", c("drle", "drle"), function(x, y, ...) {
	nruns <- length(x@values)
	nextval <- x@values[nruns] + x@deltas[nruns] * x@lengths[nruns]
	if ( nextval == y@values[1] ) {
		if ( x@deltas[nruns] == y@deltas[1] || y@lengths[1] == 1 ) {
			x@lengths[nruns] <- x@lengths[nruns] + y@lengths[1]
			y@values <- y@values[-1]
			y@lengths <- y@lengths[-1]
			y@deltas <- y@deltas[-1]
		} else if ( y@lengths[1] == 2 ) {
			x@lengths[nruns] <- x@lengths[nruns] + 1L
			y@values[1] <- y@values[1] + y@deltas[1]
			y@lengths[1] <- 1L
			y@deltas[1] <- vector(1, mode=typeof(y@deltas))
		}
	}
	new("drle",
		values=c(x@values, y@values),
		lengths=c(x@lengths, y@lengths),
		deltas=c(x@deltas, y@deltas))
})

setMethod("combine", c("drle", "numeric"),
	function(x, y, ...) combine(x[], y))

setMethod("combine", c("numeric", "drle"),
	function(x, y, ...) combine(x, y[]))

setMethod("c", "drle", function(x, ..., recursive=FALSE)
{
	dots <- list(...)
	if ( length(dots) == 0 ) {
		x
	} else if ( length(dots) == 1 ) {
		combine(x, dots[[1]])
	} else {
		do.call(combine, list(x, ...))
	}
})

setMethod("show", "drle", function(object) {
	print(data.frame(
		values=object@values,
		lengths=object@lengths,
		deltas=object@deltas))
})

