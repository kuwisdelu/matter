
#### Manage shared resources ####
## ------------------------------

create_file_resource <- function(name)
{
	path <- normalizePath(name, mustWork=FALSE)
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources )
		matter_error("shared resource named ", sQuote(name), "already exists")
	if ( file.exists(path) )
		matter_error("file ", sQuote(path), " already exists")
	if ( file.create(path) ) {
		path <- normalizePath(path, mustWork=TRUE)
		handle <- new.env(parent=emptyenv())
		handle[["name"]] <- name
		handle[["path"]] <- path
		lockEnvironment(handle, TRUE)
		assign(name, Sys.getpid(), envir=matter_shared_resource_pool())
	} else {
		matter_error("failed to create file: ", sQuote(path))
	}
	reg.finalizer(handle, finalize_shared_resource, onexit=TRUE)
	structure(name, ref=handle, class=c("shared_file", "shared_resource"))
}

remove_file_resource <- function(handle)
{
	name <- handle[["name"]]
	path <- handle[["path"]]
	path <- normalizePath(handle[["path"]], mustWork=FALSE)
	status <- FALSE
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources && file.exists(path) )
	{
		owner <- matter_shared_resource_pool()[[name]]
		if ( owner == Sys.getpid() ) {
			add_shared_file_freed(sizeof_file_resource(name))
			status <- file.remove(path)
			rm(list=name, envir=matter_shared_resource_pool())
		}
	}
	status
}

sizeof_file_resource <- function(name)
{
	if ( missing(name) ) {
		name <- matter_shared_resource_list()
		name <- name[!is_shared_memory_pattern(name)]
	}
	if ( length(name) ) {
		size <- file.size(normalizePath(name, mustWork=FALSE))
		size_bytes(set_names(size, name))
	} else {
		numeric(0L)
	}
}

create_memory_resource <- function(name)
{
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources )
		matter_error("shared resource named ", sQuote(name), "already exists")
	status <- .Call(C_createSharedMemory, as.character(name), PACKAGE="matter")
	if ( status ) {
		handle <- new.env(parent=emptyenv())
		handle[["name"]] <- name
		lockEnvironment(handle, TRUE)
		assign(name, Sys.getpid(), envir=matter_shared_resource_pool())
	} else {
		matter_error("failed to create shared memory object: ", sQuote(name))
	}
	reg.finalizer(handle, finalize_shared_resource, onexit=TRUE)
	structure(name, ref=handle, class=c("shared_memory", "shared_resource"))
}

remove_memory_resource <- function(handle)
{
	name <- handle[["name"]]
	status <- FALSE
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources )
	{
		owner <- matter_shared_resource_pool()[[name]]
		if ( owner == Sys.getpid() ) {
			add_shared_memory_freed(sizeof_memory_resource(name))
			status <- .Call(C_removeSharedMemory, name, PACKAGE="matter")
			rm(list=name, envir=matter_shared_resource_pool())
		}
	}
	status
}

sizeof_memory_resource <- function(name)
{
	if ( missing(name) ) {
		name <- matter_shared_resource_list()
		name <- name[is_shared_memory_pattern(name)]
	}
	if ( length(name) ) {
		size <- .Call(C_sizeofSharedMemory, as.character(name), PACKAGE="matter")
		size_bytes(set_names(size, name))
	} else {
		numeric(0L)
	}
}

is_shared_memory_pattern <- function(name) {
	substr(as.character(name), 1L, 1L) == ":"
}

is_shared_memory_object <- function(name) {
	.Call(C_detectSharedMemory, as.character(name), PACKAGE="matter")
}

create_shared_resource <- function(name)
{
	if ( is_shared_memory_pattern(name) ) {
		create_memory_resource(name)
	} else {
		create_file_resource(name)
	}
}

remove_shared_resource <- function(handle, gc = FALSE)
{
	if ( is_shared_memory_pattern(handle[["name"]]) ) {
		remove_memory_resource(handle)
	} else {
		if ( !gc || getOption("matter.temp.gc") )
			remove_file_resource(handle)
	}
}

sizeof_shared_resource <- function(name)
{
	size_bytes(vapply(name, function(x) {
		if ( is_shared_memory_pattern(x) ) {
			sizeof_memory_resource(x)
		} else {
			sizeof_file_resource(x)
		}
	}, numeric(1L)))
}

finalize_shared_resource <- function(handle)
{
	remove_shared_resource(handle, TRUE)
}

shared_resources <- list2env(list(
	.shared_file_freed=size_bytes(0)),
	.shared_memory_freed=size_bytes(0))

get_shared_file_freed <- function() {
	shared_resources[[".shared_file_freed"]]
}

set_shared_file_freed <- function(value) {
	shared_resources[[".shared_file_freed"]] <- size_bytes(unname(value))
}

add_shared_file_freed <- function(value) {
	current <- get_shared_file_freed()
	set_shared_file_freed(current + value)
}

get_shared_memory_freed <- function() {
	shared_resources[[".shared_memory_freed"]]
}

set_shared_memory_freed <- function(value) {
	shared_resources[[".shared_memory_freed"]] <- size_bytes(unname(value))
}

add_shared_memory_freed <- function(value) {
	current <- get_shared_memory_freed()
	set_shared_memory_freed(current + value)
}

matter_shared_resource_pool <- function() shared_resources

matter_shared_resource_list <- function(full = FALSE)
{
	resources <- ls(shared_resources)
	if ( full ) {
		sizes <- size_bytes(sizeof_shared_resource(resources))
		resources <- data.frame(name=resources, size=sizes, row.names=NULL)
	}
	resources
}

matter_shared_resource <- function(create = NULL, remove = NULL)
{
	if ( !is.null(create) && !is.null(remove) ) {
		matter_error("must specify only one of 'create' or 'remove'")
	} else if ( !is.null(create) ) {
		name <- as.character(create)
		if ( !is.character(name) || length(name) != 1L )
			stop("resource to be created must be a single string")
		ans <- create_shared_resource(name)
	} else if ( !is.null(remove) ) {
		name <- as.character(remove)
		if ( !is.character(name) || length(name) != 1L )
			stop("resource to be removed must be a single string")
		ans <- remove_shared_resource(name)
	} else {
		matter_error("must specify one of 'create' or 'remove'")
	}
	ans
}
