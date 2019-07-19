
# coerce between matter subclasses and to base R types

as_native <- function(x, ALTREP = getOption("matter.coerce.altrep"))
{
	if ( ALTREP ) {
		as.altrep(x)
	} else {
		x[]
	}
}

#### matter_vec ####

setAs("matter_vec", "vector", function(from) as_native(from))

setMethod("as.vector", "matter_vec",
	function(x, mode = "any") {
		switch(mode,
			any=as_native(x),
			raw=as.raw(x),
			logical=as.logical(x),
			integer=as.integer(x),
			double=as.double(x),
			numeric=as.numeric(x),
			altrep=as_native(x, ALTREP=TRUE),
			ALTREP=as_native(x, ALTREP=TRUE),
			stop("unsupported vector mode: '", mode, "'"))
	})

setMethod("as.raw", "matter_vec",
	function(x)
	{
		datamode(x) <- "raw"
		names(x) <- NULL
		as_native(x)
	})

setMethod("as.logical", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "logical"
		names(x) <- NULL
		as_native(x, ...)
	})

setMethod("as.integer", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "integer"
		names(x) <- NULL
		as_native(x, ...)
	})

setMethod("as.double", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "numeric"
		names(x) <- NULL
		as_native(x, ...)
	})

setMethod("as.numeric", "matter_vec",
	function(x, ...)
	{
		datamode(x) <- "numeric"
		names(x) <- NULL
		as_native(x, ...)
	})

setMethod("as.character", "matter_vec",
	function(x, ...)
	{
		names(x) <- NULL
		as.character(as_native(x, ...))
	})

setMethod("as.matrix", "matter_vec",
	function(x, ...) as.matrix(as(x, "matter_mat"), ...))

setMethod("as.array", "matter_vec",
	function(x, ...) as.array(as(x, "matter_arr"), ...))

setAs("matter_vec", "matter_mat", function(from) {
	new("matter_matc",
		data=from@data,
		datamode=from@datamode,
		paths=from@paths,
		filemode=from@filemode,
		length=from@length,
		dim=c(as.integer(from@length), 1L),
		names=NULL,
		dimnames=if ( !is.null(from@names) ) list(NULL, from@names) else NULL,
		ops=from@ops)
})

setAs("matter_vec", "matter_arr", function(from) {
	new("matter_arr",
		data=from@data,
		datamode=from@datamode,
		paths=from@paths,
		filemode=from@filemode,
		length=from@length,
		dim=as.integer(from@length),
		names=NULL,
		dimnames=if ( !is.null(from@names) ) list(from@names) else NULL,
		ops=from@ops)
})

setAs("matter_vec", "matter_list", function(from) {
	if ( !is.null(from@ops) )
		warning("dropping delayed operations")
	new("matter_list",
		data=from@data,
		datamode=from@datamode,
		paths=from@paths,
		filemode=from@filemode,
		length=1,
		dim=as.integer(from@length),
		names=NULL,
		dimnames=if ( !is.null(from@names) ) list(from@names) else NULL,
		ops=NULL)
})

#### matter_mat ####

setAs("matter_mat", "matrix", function(from) as_native(from))

setMethod("as.matrix", "matter_mat", function(x, ...) as_native(x, ...))

setMethod("as.array", "matter_arr", function(x, ...) as_native(as(x, "matter_arr"), ...))

setMethod("as.vector", "matter_mat", function(x, mode = "any") as.vector(as(x, "matter_vec"), mode=mode))

setMethod("as.raw", "matter_mat", function(x) as.raw(as(x, "matter_vec")))

setMethod("as.logical", "matter_mat", function(x, ...) as.logical(as(x, "matter_vec"), ...))

setMethod("as.integer", "matter_mat", function(x, ...) as.integer(as(x, "matter_vec"), ...))

setMethod("as.numeric", "matter_mat", function(x, ...) as.numeric(as(x, "matter_vec"), ...))

setMethod("as.double", "matter_mat", function(x, ...) as.double(as(x, "matter_vec"), ...))

setAs("matter_mat", "matter_arr", function(from) {
	to <- as(as(from, "matter_vec"), "matter_arr")
	dim(to) <- dim(from)
	to
})

setAs("matter_mat", "matter_vec", function(from) {
	if ( !is.null(from@ops) )
		warning("dropping delayed operations")
	new("matter_vec",
		data=drop_groups_from_atoms(from@data),
		datamode=from@datamode,
		paths=from@paths,
		filemode=from@filemode,
		length=from@length,
		dim=NULL,
		names=NULL,
		dimnames=NULL,
		ops=NULL)
})

setAs("matter_mat", "matter_list", function(from) {
	if ( !is.null(from@ops) )
		warning("dropping delayed operations")
	new("matter_list",
		data=from@data,
		datamode=rep(from@datamode, length(from@data)),
		paths=from@paths,
		filemode=from@filemode,
		length=length(from@data),
		dim=rep(as.integer(prod(from@dim) / length(from@data)),
			length(from@data)),
		names=NULL,
		dimnames=NULL,
		ops=NULL)
})

#### matter_arr ####

setAs("matter_arr", "array", function(from) as_native(from))

setMethod("as.array", "matter_arr", function(x, ...) as_native(x, ...))

setMethod("as.matrix", "matter_arr", function(x, ...) as_native(x, ...))

setMethod("as.vector", "matter_arr", function(x, mode = "any") as.vector(as(x, "matter_vec"), mode=mode))

setMethod("as.raw", "matter_arr", function(x) as.raw(as(x, "matter_vec")))

setMethod("as.logical", "matter_arr", function(x, ...) as.logical(as(x, "matter_vec"), ...))

setMethod("as.integer", "matter_arr", function(x, ...) as.integer(as(x, "matter_vec"), ...))

setMethod("as.numeric", "matter_arr", function(x, ...) as.numeric(as(x, "matter_vec"), ...))

setMethod("as.double", "matter_arr", function(x, ...) as.double(as(x, "matter_vec"), ...))

setAs("matter_arr", "matter_vec", function(from) {
	new("matter_vec",
		data=from@data,
		datamode=from@datamode,
		paths=from@paths,
		filemode=from@filemode,
		length=from@length,
		dim=NULL,
		names=NULL,
		dimnames=NULL,
		ops=from@ops)
})

#### matter_str ####

setAs("matter_str", "character", function(from) as_native(from))

setMethod("as.character", "matter_str", function(x, ...) as_native(x, ...))

#### matter_fc ####

setAs("matter_fc", "factor", function(from) as_native(from))

setMethod("as.factor", "matter_fc", function(x) as_native(x))

#### matter_list ####

setAs("matter_list", "matter_vec", function(from) {
	new("matter_vec",
		data=drop_groups_from_atoms(from@data),
		datamode=widest_datamode(from@datamode),
		paths=from@paths,
		filemode=from@filemode,
		length=as.numeric(sum(from@dim)),
		dim=NULL,
		names=NULL,
		dimnames=NULL,
		ops=NULL)
})

setAs("matter_list", "matter_matc", function(from) {
	if ( length(unique(from@dim)) != 1L )
		stop("all elements must be the same length")
	new("matter_matc",
		data=from@data,
		datamode=widest_datamode(from@datamode),
		paths=from@paths,
		filemode=from@filemode,
		length=as.numeric(sum(from@dim)),
		dim=c(from@dim[1L], from@length),
		names=NULL,
		dimnames=if ( !is.null(from@names) ) list(NULL, from@names) else NULL,
		ops=NULL)
})

setAs("matter_list", "matter_matr", function(from) {
	if ( length(unique(from@dim)) != 1L )
		stop("all elements must be the same length")
	new("matter_matr",
		data=from@data,
		datamode=widest_datamode(from@datamode),
		paths=from@paths,
		filemode=from@filemode,
		length=as.numeric(sum(from@dim)),
		dim=c(from@length, from@dim[1L]),
		names=NULL,
		dimnames=if ( !is.null(from@names) ) list(from@names, NULL) else NULL,
		ops=NULL)
})


