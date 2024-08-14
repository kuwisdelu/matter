
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
					if ( !file.create(.self$logfile) )
						base::stop("failed to create log file ", file)
					.self$logfile <- normalizePath(.self$logfile)
				}
				ipclock(.self$id)
				con <- file(.self$logfile, open="at")
				writeLines(.self$buffer, con)
				base::close(con)
				.self$buffer <- character(0L)
				ipcunlock(.self$id)
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
			tstamp <- paste0("[", format(Sys.time()), "]")
			info <- capture.output(print(sessionInfo()))
			info <- paste0(info, collapse="\n")
			entry <- paste0(tstamp, " Session info:\n", info)
			.self$append(entry)
		},
		history = function(print = TRUE)
		{
			.self$flush()
			if ( length(.self$logfile) ) {
				history <- readLines(.self$logfile)
			} else {
				history <- .self$buffer
			}
			if ( print ) {
				cat(history, sep="\n")
			} else {
				history
			}
		},
		log = function(..., signal = FALSE, call = NULL)
		{
			tstamp <- paste0("[", format(Sys.time()), "] ")
			msg <- .makeMessage(..., domain=.self$domain)
			if ( is.logical(signal) || signal == "message" ) {
				entry <- paste0(tstamp, msg)
			} else {
				if ( is.null(call) ) {
					where <- ""
					entry <- paste0(tstamp, toupper(signal), ": ", msg)
				} else {
					where <- paste0(" in ", deparse1(call), ": ")
					entry <- paste0(tstamp, toupper(signal), where, msg)
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
				newfile <- character(0L)
			} else {
				if ( !is.character(file) || length(file) != 1L )
					base::stop("file must be a single string")
				newfile <- normalizePath(file, mustWork=FALSE)
			}
			oldfile <- .self$logfile
			if ( file.exists(newfile) )
				base::stop("file ", sQuote(newfile), " already exists")
			.self$log("moving logfile to: ", sQuote(newfile))
			.self$append_session()
			.self$buffer <- .self$history(FALSE)
			.self$logfile <- newfile
			.self$flush()
			if ( length(oldfile) ) {
				if ( !file.remove(oldfile) ) {
					warning("failed to remove old log file: ",
						sQuote(oldfile))
				}
			}
			invisible(.self)
		},
		close = function()
		{
			.self$append_session()
			.self$flush()
			.self$logfile <- character(0L)
			ipcremove(.self$id)
			invisible(.self)
		}))

setMethod("path", "simple_logger",
	function(object, ...) object$logfile)

setReplaceMethod("path", "simple_logger",
	function(object, ..., value) object$move(value))

simple_logger <- function(file = NULL, bufferlimit = 50L, domain = NULL)
{
	if ( is.null(domain) )
		domain <- NA_character_
	if ( is.null(file) ) {
		file <- character(0L)
	} else {
		if ( !is.character(file) || length(file) != 1L )
			stop("file must be a single string")
		file <- normalizePath(file, mustWork=FALSE)
		if ( file.exists(file) )
			stop("file ", sQuote(file), " already exists")
		if ( !file.create(file) )
			warning("failed to create log file ", file)
	}
	logger <- new("simple_logger", id=ipcid(),
		buffer=character(0L), bufferlimit=bufferlimit,
		logfile=file, domain=domain)
	handle <- getDataPart(logger)
	reg.finalizer(handle, close_logger, onexit=TRUE)
	logger
}

close_logger <- function(handle) handle$.self$close()

