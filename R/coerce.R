
# coerce between matter subclasses

# matter_vec

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

# matter_mat

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

# matter_arr

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

# matter_list

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


