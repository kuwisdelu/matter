
#### 'matter_list' class for file-based lists ####
## ----------------------------------------------

setClass("matter_list", contains = "matter_")

matter_list <- function(data, type = "double", path = NULL,
	lengths = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
	readonly = NA, append = FALSE, ...)
{
	if ( !missing(data) && !is.null(data) ) {
		if ( !is.list(data) )
			data <- list(data)
		if ( missing(type) )
			type <- as.vector(vapply(data, typeof, character(1)))
		if ( anyNA(lengths) ) {
			lengths <- as.vector(vapply(data, length, numeric(1)))
			chr <- type %in% "character"
			nch <- vapply(data[chr],
				function(ch) nchar(ch, "bytes")[1L], numeric(1))
			lengths[chr] <- nch
		} else {
			if ( length(data) == 1L && length(data[[1L]]) == 1L )
				data <- rep.int(data, length(lengths))
		}
		if ( is.null(names) )
			names <- names(data)
		valid <- valid_matter_list_elts(data)
		if ( !isTRUE(valid) )
			matter_error(valid)
	}
	if ( is.null(path) ) {
		readonly <- FALSE
		path <- tempfile(tmpdir=getOption("matter.temp.dir"), fileext=".bin")
		refs <- list(matter_shared_resource(create=path))
	} else {
		refs <- NULL
		if ( any(tolower(path) %in% ":memory:") ) {
			readonly <- FALSE
			for ( i in which(tolower(path) %in% ":memory:") ) {
				path[i] <- tempmem()
				refs <- c(refs, list(matter_shared_resource(create=path[i])))
			}
		}
	}
	path <- fix_path(path, mustWork=FALSE)
	exists <- file.exists(path) | is_shared_memory_object(path)
	if ( append ) {
		readonly <- FALSE
		eof <- sizeof_shared_resource(path)
		offset <- ifelse(exists, offset + eof, offset)
	}
	if ( is.na(readonly) )
		readonly <- all(exists) && !missing(data) && !is.null(data)
	if ( any(exists) && !readonly && !missing(data) && !is.null(data) ) {
		overwrite <- offset < file.size(path) & !is_shared_memory_pattern(path)
		if ( any(overwrite) )
			matter_warn("data may overwrite existing file(s): ",
				paste0(sQuote(path[overwrite]), collapse=", "))
	}
	if ( anyNA(lengths) && anyNA(extent) ) {
		extent <- lengths <- rep.int(0, length(lengths))
	} else if ( anyNA(extent) ) {
		extent <- lengths
	} else if ( anyNA(lengths) ) {
		lengths <- extent
	}
	type <- rep_len(type, length(extent))
	if ( length(offset) != length(extent) && length(path) == 1L ) {
		sizes <- sizeof(type) * extent
		offset <- cumsum(c(offset, sizes[-length(sizes)]))
	}
	if ( isTRUE(all.equal(lengths, extent)) ) {
		group <- seq_along(extent) - 1L
	} else {
		group <- integer(length(extent))
		i1 <- min(1L, length(extent))
		for ( j in seq_along(lengths) ) {
			len <- extent[i1]
			i2 <- i1
			while ( len < lengths[j] ) {
				i2 <- i2 + 1L
				len <- len + extent[i2]
			}
			if ( len == lengths[j] ) {
				group[i1:i2] <- j - 1L
			} else {
				matter_error("lengths and extent are incompatible")
			}
			i1 <- i2 + 1L
		}
	}
	files <- !is_shared_memory_pattern(path)
	newfiles <- !exists & files
	if ( any(newfiles) ) {
		if ( missing(data) && any(extent > 0) && !is.null(data) )
			matter_warn("creating uninitialized backing file(s): ",
				paste0(sQuote(path[newfiles]), collapse=", "))
		success <- file.create(path[newfiles])
		if ( !all(success) )
			matter_error("error creating file(s): ",
				paste0(sQuote(path[newfiles][!success]), collapse=", "))
	}
	path[files] <- fix_path(path[files], mustWork=TRUE)
	x <- new("matter_list",
		data=atoms(
			source=path,
			type=as_Ctype(type),
			offset=as.double(offset),
			extent=as.double(extent),
			group=group,
			readonly=readonly,
			refs=refs),
		type=as_Rtype(type),
		dim=lengths,
		names=names, ...)
	if ( !missing(data) ) {
		if ( any(is_shared_memory_pattern(path)) )
			requisition_atoms(atomdata(x))
		if ( !is.null(data) )
			x[] <- data
	}
	x
}

valid_matter_list_elts <- function(list)
{
	for ( x in list ) {
		if ( !is.atomic(x) || is.complex(x) || (is.character(x) && length(x) != 1L) )
			return(paste0("matter list elements must be of type ",
				"'raw', 'logical', 'integer', 'double', or _scalar_ 'character' ",
					"(", sQuote(class(x)), " provided)"))
	}
	TRUE
}

struct <- function(..., path = NULL, readonly = FALSE, offset = 0, filename)
{
	args <- list(...)
	if ( any(lengths(args) != 1L) )
		matter_error("all arguments must be length 1")
	if ( any(sapply(args, function(a) is.null(names(a)))) )
		matter_error("all arguments must be a named scalar")
	if ( !missing(filename) ) {
		.Deprecated(msg="'filename' is deprecated, use 'path' instead")
		path <- filename
	}
	if ( !is.null(path) && length(path) != 1L )
		matter_error("'path' must be a scalar string")
	names <- names(args)
	types <- sapply(args, names, USE.NAMES=FALSE)
	lens <- as.integer(unlist(args))
	offset <- offset + c(0, cumsum(sizeof(types) * lens)[-length(lens)])
	matter_list(NULL, path=path, type=types, offset=offset,
		lengths=lens, names=names, readonly=readonly)
}

setAs("matter_list", "matter_vec",
	function(from) {
		x <- new("matter_vec",
			data=ungroup_atoms(from@data),
			type=topmode_Rtype(from@type),
			dim=sum(from@dim),
			names=NULL,
			dimnames=NULL,
			ops=NULL,
			transpose=FALSE)
		if ( validObject(x) )
			x
	})

setAs("matter_list", "matter_mat",
	function(from) {
		adims <- dim(from@data)
		if ( anyNA(adims) )
			matter_error("can't coerce matter list with different lengths to a matrix")
		x <- new("matter_mat",
			data=from@data,
			type=topmode_Rtype(from@type),
			dim=adims,
			names=NULL,
			dimnames=if (is.null(from@names)) NULL else list(NULL, from@names),
			ops=NULL,
			transpose=FALSE,
			indexed=TRUE)
		if ( validObject(x) )
			x
	})

setAs("matter_mat", "matter_list",
	function(from) {
		if ( !isTRUE(from@indexed) )
			matter_error("can't coerce matter list that isn't 'indexed' to a matrix")
		x <- new("matter_list",
			data=from@data,
			type=topmode_Rtype(from@type),
			dim=rep.int(nrow(from@data), ncol(from@data)),
			names=NULL)
		if ( validObject(x) )
			x
	})

setAs("matter_list", "matter_arr",
	function(from) as(as(from, "matter_vec"), "matter_arr"))

setMethod("as.list", "matter_list",
	function(x, ...) {
		if ( getOption("matter.coerce.altrep") ) {
			as.altrep(x)
		} else {
			x[]
		}
	})

setMethod("as.vector", "matter_list",
	function(x, mode = "any") {
		y <- as.list(x)
		switch(mode,
			any=, list=y,
			logical=as.logical(y),
			integer=as.integer(y),
			numeric=as.numeric(y),
			double=as.double(y),
			complex=as.complex(y),
			character=as.character(y),
			raw=as.raw(y),
			matter_error("mode ", sQuote(mode), " not supported"))
	})

setMethod("describe_for_display", "matter_list", function(x) {
	desc1 <- paste0("<", length(x), " length> ", class(x))
	desc2 <- paste0("out-of-core list")
	paste0(desc1, " :: ", desc2)
})

setMethod("preview_for_display", "matter_list", function(x) preview_list(x))

setMethod("mem_realized", "matter_list", function(x) {
	size_bytes(sum(lengths(x) * sizeof(type(x)), na.rm=TRUE))
})

copy_to_matter_list <- function(object, type = NULL, path = NULL,
	offset = 0, readonly = TRUE, append = FALSE, ..., BPPARAM)
{
	BPPARAM <- bplocalized(BPPARAM)
	if ( is.matter(object) ) {
		type <- type %||% type(atomdata(object))
		extent <- lengths(atomdata(object))
	} else {
		type <- type %||% type(object)
		extent <- length(object)
	}
	x <- matter_list(NULL, type=type, path=path,
		lengths=object@dim, names=names(object),
		offset=offset, extent=extent, readonly=FALSE,
		append=append)
	type(x) <- type(object)
	pid <- ipcid()
	FUN <- copy_to_matter_fun(pid, x)
	chunk_lapply(object, FUN, ..., BPPARAM=BPPARAM)
	readonly(x) <- readonly
	ipcremove(pid)
	if ( validObject(x) )
		x
}

setMethod("fetch", "matter_list",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_list(object, path=":memory:", ..., BPPARAM=BPPARAM))

setMethod("flash", "matter_list",
	function(object, ..., BPPARAM = bpparam())
		copy_to_matter_list(object, ..., BPPARAM=BPPARAM))

subset_matter_list_elt <- function(x, i = NULL)
{
	if ( length(i) != 1 )
		matter_error("attempt to select more than one element")
	data <- subset_atoms2(x@data, NULL, i)
	y <- new("matter_vec", x, data=data,
		type=x@type[i], dim=x@dim[i],
		names=x@names[i])
	if ( validObject(y) )
		y
}

get_matter_list_elt <- function(x, i = NULL, j = NULL) {
	.Call(C_getMatterListElt, x, i, j, PACKAGE="matter")
}

set_matter_list_elt <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterListElt, x, i, j, value, PACKAGE="matter")
}

subset_matter_list_sublist <- function(x, i = NULL)
{
	data <- subset_atoms1(x@data, i)
	y <- new(class(x), x, data=data,
		type=x@type[i], dim=x@dim[i],
		names=x@names[i])
	if ( validObject(y) )
		y
}

get_matter_list_sublist <- function(x, i = NULL, j = NULL) {
	y <- .Call(C_getMatterListSubset, x, i, j, PACKAGE="matter")
	set_names(y, names(x), i)
}

set_matter_list_sublist <- function(x, i = NULL, j = NULL, value = NULL) {
	.Call(C_setMatterListSubset, x, i, j, value, PACKAGE="matter")
}

setMethod("[[", c(x = "matter_list"),
	function(x, i, j, ..., exact = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		get_matter_list_elt(x, i, j)
	})

setReplaceMethod("[[", c(x = "matter_list"),
	function(x, i, value) {
		i <- as_subscripts(i, x)
		set_matter_list_elt(x, i, NULL, value)
	})

setMethod("[", c(x = "matter_list"),
	function(x, i, j, ..., drop = TRUE) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( is_null_or_na(drop) ) {
			if ( !is.null(j) )
				matter_warn("ignoring array subscripts")
			subset_matter_list_sublist(x, i)
		} else {
			get_matter_list_sublist(x, i, j)
		}
	})

setReplaceMethod("[", c(x = "matter_list"),
	function(x, i, j, ..., value) {
		i <- as_subscripts(i, x)
		j <- as_subscripts(j, x)
		if ( !is.list(value) )
			value <- list(value)
		set_matter_list_sublist(x, i, j, value)
	})

setMethod("$", c(x = "matter_list"),
	function(x, name) {
		i <- pmatch(name, names(x))
		if ( !is.na(i) ) {
			get_matter_list_elt(x, i)
		} else {
			NULL
		}
	})

setReplaceMethod("$", c(x = "matter_list"),
	function(x, name, value) {
		i <- match(name, names(x))
		if ( !is.na(i) ) {
			set_matter_list_elt(x, i, NULL, value)
		} else {
			matter_error("item ", sQuote(name), " to be replaced not found")
		}
	})

setMethod("combine", "matter_list",
	function(x, y, ...) {
		data <- cbind(x@data, y@data)
		new(class(x), data=data,
			type=c(x@type, y@type),
			dim=c(x@dim, y@dim),
			names=combine_names(x, y),
			dimnames=NULL)
	})

setMethod("c", "matter_list", combine_any)

setMethod("dim", "matter_list", function(x) NULL)

setMethod("length", "matter_list", function(x) length(x@dim))

setMethod("lengths", "matter_list",
	function(x) ifelse(x@type %in% "character", 1L, x@dim))


