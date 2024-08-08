
#### Define drle class ####
## -------------------------

setClassUnion("numeric_OR_logical", c("numeric", "logical"))

setClass("drle",
	slots = c(
		values = "numeric_OR_logical",
		lengths = "numeric",
		deltas = "numeric"),
	validity = function(object) {
		errors <- NULL
		if ( length(object@deltas) > 0L ) {
			lens <- c(length(object@values), length(object@lengths), length(object@deltas))
		} else {
			lens <- c(length(object@values), length(object@lengths))
		}
		if ( n_unique(lens) != 1L )
			errors <- c(errors, "'values', 'deltas', and 'lengths' must have the same length")
		if ( any(object@lengths < 1) )
			errors <- c(errors, "all 'lengths' must be positive")
		if ( is.null(errors) ) TRUE else errors
	})

setClass("drle_fct",
	slots = c(levels = "character"),
	contains = "drle")

drle <- function(x, type = "drle", cr_threshold = 0)
{
	if ( is.drle(x) )
		return(x)
	if ( !is.numeric(x) && !is.logical(x) )
		x <- as.factor(x)
	if ( !is.numeric(x) ) {
		type <- as_run_type("rle")
	} else {
		type <- as_run_type(type)
	}
	y <- .Call(C_encodeDRLE, x, type, cr_threshold, PACKAGE="matter")
	if ( is.drle(y) && is.factor(x) )
		y <- new("drle_fct", y, levels=levels(x))
	if ( is.drle(y) && all(y@deltas == 0) )
		y@deltas <- vector(typeof(y@deltas), length=0L)
	if ( validObject(y) )
		y
}

setAs("drle", "list", function(from)
	list(values=from@values,
		lengths=from@lengths,
		deltas=from@deltas))

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
	if ( length(object@deltas) > 0L ) {
		cat("with", length(object@values), "delta-encoded runs\n")
	} else {
		cat("with", length(object@values), "runs\n")
	}
})

setMethod("show", "drle_fct", function(object) {
	callNextMethod()
	cat("Levels(", nlevels(object), "): ", sep="")
	cat(paste_head(object@levels), "\n")
})

setMethod("vm_used", "drle", function(x) size_bytes(0))

setMethod("shm_used", "drle", function(x) size_bytes(0))

setMethod("type", "drle", function(x) typeof(x@values))

is.drle <- function(x) is(x, "drle")

get_drle_elts <- function(x, i = NULL) {
	.Call(C_decodeDRLE, x, i, PACKAGE="matter")
}

subset_drle <- function(x, i = NULL) {
	.Call(C_recodeDRLE, x, i, PACKAGE="matter")
}

setMethod("[",
	c(x = "drle", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop = TRUE)
	{
		if ( ...length() > 0 )
			matter_error("incorrect number of dimensions")
		i <- as_subscripts(i, x)
		if ( is_null_or_na(drop) ) {
			subset_drle(x, i)
		} else {
			get_drle_elts(x, i)
		}
	})

setMethod("[",
	c(x = "drle_fct", i = "ANY", j = "ANY", drop = "ANY"),
	function(x, i, ..., drop = TRUE)
	{
		if ( ...length() > 0 )
			matter_error("incorrect number of dimensions")
		i <- as_subscripts(i, x)
		if ( is_null_or_na(drop) ) {
			y <- subset_drle(x, i)
		} else {
			y <- get_drle_elts(x, i)
		}
		if ( is.drle(y) ) {
			y <- new(class(x), y, levels=levels(x))
		} else {
			y <- factor(levels(x)[y], levels=levels(x))
		}
		y
	})

setMethod("length", "drle", function(x) sum(x@lengths))

setMethod("levels", "drle_fct", function(x) x@levels)

setReplaceMethod("levels", "drle_fct",
	function(x, value) {
		x@levels <- as.character(value)
		x
	})

setMethod("droplevels", "drle_fct", function(x) {
	i <- which(seq_along(x@levels) %in% x@values)
	x@values <- bsearch(x@values, i)
	x@levels <- x@levels[i]
	if ( validObject(x) )
		x
})

setMethod("combine", c("drle", "drle"), function(x, y, ...) {
	n <- length(x@values)
	if ( length(x@deltas) == 0L )
		x@deltas <- vector(typeof(x@deltas), length=length(x@values))
	if ( length(y@deltas) == 0L )
		y@deltas <- vector(typeof(y@deltas), length=length(y@values))
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
	ans <- new(class(x),
		values=c(x@values, y@values),
		lengths=c(x@lengths, y@lengths),
		deltas=c(x@deltas, y@deltas))
	if ( is.drle(ans) && all(ans@deltas == 0) )
		ans@deltas <- vector(typeof(ans@deltas), length=0L)
	if ( validObject(ans) )
		ans
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
	function(x, y, ...) c(x[], y))

setMethod("combine", c("numeric", "drle"),
	function(x, y, ...) c(x, y[]))

setMethod("c", "drle", function(x, ...)
{
	if ( ...length() > 0 )
		x <- do.call(combine, list(x, ...))
	if ( validObject(x) )
		x
})

