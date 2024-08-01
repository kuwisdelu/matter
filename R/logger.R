
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
						stop("failed to create log file ", file)
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
			info <- capture.output(print(sessionInfo()))
			info <- paste0(info, collapse="\n")
			entry <- paste0(timestamp, " Session info:\n", info)
			.self$append(entry)
		},
		log = function(..., echo = FALSE)
		{
			timestamp <- paste0("[", format(Sys.time()), "]")
			msg <- .makeMessage(..., domain=.self$domain)
			if ( echo )
				base::message(msg)
			entry <- paste0(timestamp, " ", msg)
			.self$append(entry)
		},
		message = function(...)
		{
			.self$log(..., echo=TRUE)
		},
		warning = function(...)
		{
			timestamp <- paste0("[", format(Sys.time()), "]")
			where <- deparse1(sys.call(-1L))
			where <- paste0("in ", where, ":")
			msg <- .makeMessage(..., domain=.self$domain)
			entry <- paste(timestamp, "WARNING", where, msg)
			.self$append(entry)
			base::warning(where, " ", msg, call.=FALSE)
		},
		stop = function(...)
		{
			timestamp <- paste0("[", format(Sys.time()), "]")
			where <- deparse1(sys.call(-1L))
			where <- paste0("in ", where, ":")
			msg <- .makeMessage(..., domain=.self$domain)
			entry <- paste(timestamp, "ERROR", where, msg)
			.self$append(entry)
			base::stop(where, " ", msg, call.=FALSE)
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
				stop("file must be a single string")
			newfile <- normalizePath(file, mustWork=FALSE)
			if ( file.exists(newfile) )
				stop("file ", sQuote(newfile), " already exists")
			if ( file.create(newfile) ) {
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
				stop("failed to create new log file ", sQuote(.self$logfile))
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

logger <- function() getOption("matter.logger")

