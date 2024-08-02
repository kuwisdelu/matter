
#### A simple logger ####
## ----------------------

setRefClass("simple_logger",
	fields = c(
		id = "character",
		buffer = "character",
		bufferlimit = "integer",
		logfile = "character",
		domain = "character"),
	methods = list(
		show = function()
		{
			if ( length(.self$logfile) ) {
				string <- basename(.self$logfile)
			} else {
				string <- "NULL"
			}
			cat(class(.self), "::", string, "\n")
		},
		flush = function()
		{
			if ( length(.self$logfile) ) {
				if ( !file.exists(.self$logfile) ) {
					if ( !file.create(file) )
						base::stop("failed to create log file ", file)
				}
				BiocParallel::ipclock(.self$id)
				con <- file(.self$logfile, open="at")
				writeLines(.self$buffer, con)
				base::close(con)
				.self$buffer <- character(0L)
				BiocParallel::ipcunlock(.self$id)
			}
			invisible(.self)
		},
		append = function(entry)
		{
			.self$buffer <- c(.self$buffer, entry)
			if ( length(.self$logfile) && 
				length(.self$buffer) > .self$bufferlimit )
			{
				.self$flush()
			}
			invisible(.self)
		},
		append_session = function()
		{
			timestamp <- paste0("[", format(Sys.time()), "]")
			info <- utils::capture.output(print(utils::sessionInfo()))
			info <- paste0(info, collapse="\n")
			entry <- paste0(timestamp, " Session info:\n", info)
			.self$append(entry)
		},
		history = function()
		{
			.self$flush()
			history <- readLines(.self$logfile)
			cat(history, sep="\n")
		},
		log = function(..., signal = FALSE, call = NULL)
		{
			timestamp <- paste0("[", format(Sys.time()), "] ")
			msg <- .makeMessage(..., domain=.self$domain)
			if ( is.logical(signal) || signal == "message" ) {
				entry <- paste0(timestamp, msg)
			} else {
				if ( is.null(call) ) {
					where <- ""
					entry <- paste0(timestamp, toupper(signal), ": ", msg)
				} else {
					where <- paste0(" in ", deparse1(call), ": ")
					entry <- paste0(timestamp, toupper(signal), where, msg)
				}
			}
			.self$append(entry)
			if ( !isFALSE(signal) ) {
				switch(signal,
					message=base::message(msg),
					warning=base::warning(where, msg, call.=FALSE),
					error=base::stop(where, msg, call.=FALSE))
			}
			invisible(.self)
		},
		message = function(...)
		{
			.self$log(..., signal="message")
		},
		warning = function(..., call = NULL)
		{
			if ( is.null(call) )
				call <- sys.call(-1L)
			.self$log(..., signal="warning", call=call)
		},
		stop = function(..., call = NULL)
		{
			if ( is.null(call) )
				call <- sys.call(-1L)
			.self$log(..., signal="error", call=call)
		},
		move = function(file)
		{
			if ( is.null(file) ) {
				if ( !file.remove(.self$logfile) ) {
					warning("failed to remove old log file: ",
						sQuote(.self$logfile))
				}
				.self$logfile <- character(0L)
				return(invisible(.self))
			}
			if ( !is.character(file) || length(file) != 1L )
				base::stop("file must be a single string")
			newfile <- normalizePath(file, mustWork=FALSE)
			if ( file.exists(newfile) )
				base::stop("file ", sQuote(newfile), " already exists")
			if ( file.create(newfile) ) {
				newfile <- normalizePath(newfile, mustWork=TRUE)
				BiocParallel::ipclock(.self$id)
				log <- c(readLines(.self$logfile), .self$buffer)
				writeLines(log, newfile)
				if ( !file.remove(.self$logfile) ) {
					warning("failed to remove old log file: ",
						sQuote(.self$logfile))
				}
				.self$buffer <- character(0L)
				.self$logfile <- newfile
				BiocParallel::ipcunlock(.self$id)
			} else {
				base::stop("failed to create new log file ", sQuote(.self$logfile))
			}
			invisible(.self)
		},
		close = function()
		{
			.self$append_session()
			.self$flush()
			.self$logfile <- character(0L)
			BiocParallel::ipcremove(.self$id)
			invisible(.self)
		}))

setMethod("path", "simple_logger",
	function(object, ...) object$logfile)

setReplaceMethod("path", "simple_logger",
	function(object, ..., value) object$move(value))

simple_logger <- function(file = NULL, domain = NULL, bufferlimit = 50L)
{
	if ( is.null(domain) )
		domain <- NA_character_
	if ( is.null(file) )
		file <- tempfile("logger", fileext=".log")
	if ( !is.character(file) || length(file) != 1L )
		stop("file must be a single string")
	file <- normalizePath(file, mustWork=FALSE)
	if ( file.exists(file) )
		stop("file ", sQuote(file), " already exists")
	if ( !file.create(file) )
		warning("failed to create log file ", file)
	logger <- new("simple_logger", id=ipcid(),
		buffer=character(0L), bufferlimit=bufferlimit,
		logfile=file, domain=domain)
	f <- local(function(logger) logger$.self$close(), envir=baseenv())
	reg.finalizer(getDataPart(logger), f, onexit=TRUE)
	logger
}

matter_logger <- function() getOption("matter.logger")

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

