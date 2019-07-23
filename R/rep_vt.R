
#### Define rep_vt class ####
## -------------------------

setClass("rep_vt",
	slots = c(
		data = "vector",
		length = "numeric"),
	prototype = prototype(
		data = integer(),
		length= 0))

rep_vt <- function(x, times, length.out = length(x) * times)
{
	attributes(x) <- NULL
	out <- new("rep_vt", data=x, length=length.out)
	if ( validObject(out) )
		out
}

setMethod("describe_for_display", "rep_vt", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("repeated vector")
	paste0(desc1, " :: ", desc2)
})

setMethod("show", "rep_vt", function(object) {
	cat(describe_for_display(object), "\n", sep="")
	print(list(data=object@data, length=object@length))
})

setAs("rep_vt", "list", function(from) as.list(from[]))

setAs("rep_vt", "vector", function(from) from[])

setMethod("as.list", "rep_vt", function(x) as(x, "list"))

setMethod("as.vector", "rep_vt", function(x) as(x, "vector"))

setMethod("[",
	c(x = "rep_vt", i = "missing", j = "missing", drop = "missing"),
	function(x, ...) rep_len(x@data, x@length))

setMethod("[",
	c(x = "rep_vt", i = "ANY", j = "missing", drop = "missing"),
	function(x, i, ...) {
		if ( 0L < i && i <= x@length ) {
			x@data[((i - 1L) %% length(x@data)) + 1L]
		} else {
			stop("subscript out of bounds")
		}
	})

setMethod("[[",
	c(x = "rep_vt", i = "ANY"),
	function(x, i, ...) {
		if ( 0L < i && i <= x@length ) {
			x@data[[((i - 1L) %% length(x@data)) + 1L]]
		} else {
			stop("subscript out of bounds")
		}
	})

setMethod("length", "rep_vt", function(x) x@length)

