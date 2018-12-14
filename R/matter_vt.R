
#### Define matter<virtual> class for virtual matter objects ####
## --------------------------------------------------------------

setClass("matter_vt",
	slot = c(data = "list"),
	contains = c("matter", "VIRTUAL"),
	prototype = prototype(
		data = list(),
		datamode = make_datamode("virtual", type="R"),
		filemode = make_filemode(),
		dim = 0L,
		dimnames = NULL),
	validity = function(object) {
		errors <- NULL
		if ( !"virtual" %in% object@datamode )
			errors <- c(errors, "'datamode' must include 'virtual'")
		if ( is.null(errors) ) TRUE else errors
	})

setMethod("show", "matter_vt", function(object) {
	object.memory <- object.size(object)
	class(object.memory) <- "num_bytes"
	cat("    ", format(object.memory, units="auto"), " real memory\n", sep="")
	cat("    ", format(vm_used(object), units="auto"), " virtual memory\n", sep="")
})

setReplaceMethod("datamode", "matter_vt", function(x, value) {
	x@data <- lapply(x@data, function(a) {
		if ( is.matter(a) )
			datamode(a) <- value
		a
	})
	x
})

setReplaceMethod("paths", "matter_vt", function(x, value) {
	x@data <- lapply(x@data, function(a) {
		if ( is.matter(a) )
			paths(a) <- value
		a
	})
	callNextMethod(x, value)
})

setReplaceMethod("filemode", "matter_vt", function(x, value) {
	x@data <- lapply(x@data, function(a) {
		if ( is.matter(a) )
			filemode(a) <- value
		a
	})
	callNextMethod(x, value)
})

setReplaceMethod("readonly", "matter_vt", function(x, value) {
	x@data <- lapply(x@data, function(a) {
		if ( is.matter(a) )
			readonly(a) <- value
		a
	})
	callNextMethod(x, value)
})

setReplaceMethod("chunksize", "matter_vt", function(x, value) {
	x@data <- lapply(x@data, function(a) {
		if ( is.matter(a) )
			chunksize(a) <- value
		a
	})
	callNextMethod(x, value)
})
