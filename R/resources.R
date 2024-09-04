
#### Initialize matter resources ####
## ----------------------------------

matter_resources <- list2env(list(
	.logger=simple_logger(NULL),
	.shared_file_max_used=size_bytes(0),
	.shared_memory_max_used=size_bytes(0),
	.Random.seed=NULL))

matter_logger <- function() matter_resources[[".logger"]]

matter_log <- function(..., verbose = FALSE) {
	matter_logger()$log(..., signal=verbose)
}

matter_warn <- function(...) {
	call <- sys.call(-1L)
	matter_logger()$warning(..., call=call)
}

matter_error <- function(...) {
	call <- sys.call(-1L)
	matter_logger()$stop(..., call=call)
}

#### Manage shared resources ####
## ------------------------------

create_file_resource <- function(name)
{
	path <- fix_path(name, mustWork=FALSE)
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources )
		matter_error("shared resource named ", sQuote(name), "already exists")
	if ( file.exists(path) )
		matter_error("file ", sQuote(path), " already exists")
	if ( file.create(path) ) {
		path <- fix_path(path, mustWork=TRUE)
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
	path <- fix_path(handle[["path"]], mustWork=FALSE)
	status <- FALSE
	known_resources <- matter_shared_resource_list()
	if ( name %in% known_resources && file.exists(path) )
	{
		owner <- matter_shared_resource_pool()[[name]]
		if ( owner == Sys.getpid() ) {
			update_shared_file_max()
			status <- file.remove(path)
			rm(list=name, envir=matter_shared_resource_pool())
		}
	}
	status
}

sizeof_file_resource <- function(name, owned = FALSE)
{
	if ( missing(name) ) {
		name <- matter_shared_resource_list(owned=owned)
		name <- name[!is_shared_memory_pattern(name)]
	}
	if ( length(name) ) {
		size <- file.size(fix_path(name, mustWork=FALSE))
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
			update_shared_memory_max()
			status <- .Call(C_removeSharedMemory, name, PACKAGE="matter")
			rm(list=name, envir=matter_shared_resource_pool())
		}
	}
	status
}

sizeof_memory_resource <- function(name, owned = FALSE)
{
	if ( missing(name) ) {
		name <- matter_shared_resource_list(owned=owned)
		name <- name[is_shared_memory_pattern(name)]
	}
	if ( length(name) ) {
		size <- .Call(C_sizeofSharedMemory, as.character(name), PACKAGE="matter")
		size_bytes(set_names(size, name))
	} else {
		numeric(0L)
	}
}

resize_memory_resource <- function(name, value) {
	.Call(C_resizeSharedMemory, as.character(name), as.double(value), PACKAGE="matter")
}

tempmem <- function(pattern = "@") {
	paste0(pattern, uuid(FALSE)$string)
}

is_shared_memory_pattern <- function(name) {
	substr(as.character(name), 1L, 1L) == "@"
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

get_shared_RNGStream <- function() {
	matter_resources[[".Random.seed"]]
}

set_shared_RNGStream <- function(value) {
	matter_resources[[".Random.seed"]] <- value
}

get_shared_file_max <- function() {
	matter_resources[[".shared_file_max_used"]]
}

set_shared_file_max <- function(value) {
	matter_resources[[".shared_file_max_used"]] <- size_bytes(unname(value))
}

update_shared_file_max <- function()
{
	current <- sum(sizeof_file_resource(owned=TRUE))
	if ( current > get_shared_file_max() )
		set_shared_file_max(current)
	invisible(get_shared_file_max())
}

get_shared_memory_max <- function() {
	matter_resources[[".shared_memory_max_used"]]
}

set_shared_memory_max <- function(value) {
	matter_resources[[".shared_memory_max_used"]] <- size_bytes(unname(value))
}

update_shared_memory_max <- function()
{
	current <- sum(sizeof_memory_resource(owned=TRUE))
	if ( current > get_shared_memory_max() )
		set_shared_memory_max(current)
	invisible(get_shared_memory_max())
}

matter_shared_resource_pool <- function() matter_resources

matter_shared_resource_list <- function(details = FALSE, owned = FALSE)
{
	if ( owned ) {
		owned <- eapply(matter_resources, identical, Sys.getpid())
		resources <- names(owned)[unlist(owned)]
	} else {
		resources <- ls(matter_resources)
	}
	if ( details ) {
		pids <- vapply(resources,
			function(name) matter_resources[[name]], numeric(1L))
		sizes <- size_bytes(sizeof_shared_resource(resources))
		types <- ifelse(is_shared_memory_pattern(resources), "memory", "file")
		resources <- data.frame(name=resources, type=types,
			pid=pids, size=sizes, row.names=NULL)
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
			matter_error("resource to be created must be a single string")
		ans <- create_shared_resource(name)
	} else if ( !is.null(remove) ) {
		name <- as.character(remove)
		if ( !is.character(name) || length(name) != 1L )
			matter_error("resource to be removed must be a single string")
		ans <- remove_shared_resource(name)
	} else {
		matter_error("must specify one of 'create' or 'remove'")
	}
	ans
}
