
# coerce between matter subclasses and to base R types

as.native <- function(x, ALTREP = getOption("matter.coerce.altrep"))
{
	if ( isTRUE(ALTREP) ) {
		as.altrep(x)
	} else {
		x[]
	}
}

as.nativelist <- function(x, ALTREP = getOption("matter.coerce.altrep.list"))
{
	if ( isTRUE(ALTREP) ) {
		as.altrep(x)
	} else {
		x[]
	}
}

#### matter_vec ####

setAs("matter_vec", "vector", function(from) as.native(from))

setMethod("as.vector", "matter_vec",
	function(x, mode = "any") {
		switch(tolower(mode),
			any=as.native(x),
			raw=as.raw(x),
			logical=as.logical(x),
			integer=as.integer(x),
			double=as.double(x),
			numeric=as.numeric(x),
			altrep=as.native(x, ALTREP=TRUE),
			stop("unsupported vector mode: '", mode, "'"))
	})

setMethod("as.raw", "matter_vec",
	function(x)
	{
		datamode(x) <- "raw"
		names(x) <- NULL
		as.native(x)
	})

setMethod("as.logical", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "logical"
		names(x) <- NULL
		as.native(x, ...)
	})

setMethod("as.integer", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "integer"
		names(x) <- NULL
		as.native(x, ...)
	})

setMethod("as.numeric", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "numeric"
		names(x) <- NULL
		as.native(x, ...)
	})

setMethod("as.character", "matter_vec",
	function(x, ...)
	{
		names(x) <- NULL
		as.character(as.native(x, ...))
	})

setMethod("as.matrix", "matter_vec",
	function(x, ...) as.matrix(as(x, "matter_mat"), ...))

setMethod("as.array", "matter_vec",
	function(x, ...) as.array(as(x, "matter_arr"), ...))

#### matter_mat ####

setAs("matter_mat", "matrix", function(from) as.native(from))

setMethod("as.matrix", "matter_mat", function(x, ...) as.native(x, ...))

setMethod("as.array", "matter_arr", function(x, ...) as.native(as(x, "matter_arr"), ...))

setMethod("as.vector", "matter_mat", function(x, mode = "any") as.vector(as(x, "matter_vec"), mode=mode))

setMethod("as.raw", "matter_mat", function(x) as.raw(as(x, "matter_vec")))

setMethod("as.logical", "matter_mat", function(x, ...) as.logical(as(x, "matter_vec"), ...))

setMethod("as.integer", "matter_mat", function(x, ...) as.integer(as(x, "matter_vec"), ...))

setMethod("as.numeric", "matter_mat", function(x, ...) as.numeric(as(x, "matter_vec"), ...))

#### matter_arr ####

setAs("matter_arr", "array", function(from) as.native(from))

setMethod("as.array", "matter_arr", function(x, ...) as.native(x, ...))

setMethod("as.matrix", "matter_arr", function(x, ...) as.native(x, ...))

setMethod("as.vector", "matter_arr", function(x, mode = "any") as.vector(as(x, "matter_vec"), mode=mode))

setMethod("as.raw", "matter_arr", function(x) as.raw(as(x, "matter_vec")))

setMethod("as.logical", "matter_arr", function(x, ...) as.logical(as(x, "matter_vec"), ...))

setMethod("as.integer", "matter_arr", function(x, ...) as.integer(as(x, "matter_vec"), ...))

setMethod("as.numeric", "matter_arr", function(x, ...) as.numeric(as(x, "matter_vec"), ...))

#### matter_str ####

setAs("matter_str", "character", function(from) as.native(from))

setMethod("as.character", "matter_str", function(x, ...) as.native(x, ...))

#### matter_fct ####

setAs("matter_fct", "factor", function(from) as.native(from))

setMethod("as.factor", "matter_fct", function(x) as.native(x))

#### matter_list ####

setAs("matter_list", "list", function(from) as.nativelist(from))

setMethod("as.list", "matter_list", function(x, ...) as.nativelist(x, ...))

