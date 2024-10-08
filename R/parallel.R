
#### SnowfastParam ####
## ----------------------

setRefClass("SnowfastParam",
	contains="SnowParam",
	fields=list(),
	methods=list())

WORKER_TIMEOUT <- NA_integer_

TASKS_MAXIMUM <- .Machine$integer.max

NULLcluster <- function() structure(list(), class=c("NULLcluster", "cluster"))

snowHost <- function(local = TRUE)
{
	if ( local ) {
		host <- "localhost"
	} else {
		host <- Sys.info()[["nodename"]]
	}
	host <- Sys.getenv("MASTER", host)
	host <- getOption("bphost", host)
	host
}

snowPort <- function()
{
	port <- Sys.getenv("R_PARALLEL_PORT")
	if ( identical(port, "random") ) {
		port <- NA_integer_
	} else {
		port <- as.integer(port)
	}
	if (is.na(port))
	{
		seed <- get0(".Random.seed", envir=globalenv())
		ran1 <- sample.int(.Machine$integer.max - 1L, 1L) / .Machine$integer.max
		port <- 11000 + 1000 * ((ran1 + unclass(Sys.time()) / 300) %% 1)
		if ( is.null(seed) ) {
			rm(".Random.seed", envir=globalenv())
		} else {
			assign(".Random.seed", seed, envir=globalenv())
		}
	}
	as.integer(port)
}

SnowfastParam <- function(workers = snowWorkers(),
	tasks = 0L, stop.on.error = TRUE, progressbar = FALSE,
	RNGseed = NULL, timeout = WORKER_TIMEOUT,
	exportglobals = TRUE, exportvariables = TRUE,
	resultdir = NA_character_, jobname = "BPJOB",
	force.GC = FALSE, fallback = TRUE, useXDR = FALSE,
	manager.hostname = NA_character_, manager.port = NA_character_, ...)
{
	if ( progressbar && missing(tasks) )
		tasks <- TASKS_MAXIMUM
	if ( is.na(manager.hostname) )
		manager.hostname <- snowHost()
	if ( is.na(manager.port) )
		manager.port <- snowPort()
	cargs <- list(spec = workers, type="PSOCK", useXDR=useXDR)
	cargs <- c(cargs, list(...))
	new("SnowfastParam",
		cluster=NULLcluster(),
		.clusterargs=cargs,
		.controlled=FALSE,
		workers=workers,
		tasks=as.integer(tasks),
		stop.on.error=stop.on.error,
		progressbar=progressbar,
		RNGseed=RNGseed,
		timeout=timeout,
		exportglobals=exportglobals,
		exportvariables=exportvariables,
		log=FALSE, threshold="INFO", logdir=NA_character_,
		resultdir=resultdir,
		jobname=jobname,
		force.GC=force.GC,
		fallback=fallback,
		hostname=manager.hostname, port=manager.port)
}

bplocal <- function(x) {
	if ( is.null(x) || is(x, "SerialParam") ) {
		TRUE
	} else if ( is(x, "SnowParam") ) {
		is.numeric(bpworkers(x)) || all(bpworkers(x) %in% "localhost")
	} else {
		NA
	}
}

bplocalized <- function(x) {
	if ( is.null(x) ) {
		NULL
	} else if ( isTRUE(bplocal(x)) ) {
		as(x, "BiocParallelParam")
	} else {
		as(x, "SerialParam")
	}
}

setMethod("bpstart", "SnowfastParam",
	function(x, lenX = bpnworkers(x)) {
		if ( bpisup(x) )
			matter_error("cluster already started")
		if ( bpnworkers(x) == 0L && lenX <= 0 )
			matter_error("cluster not started; no workers specified")
		nnodes <- min(bpnworkers(x), lenX)
		cargs <- x$.clusterargs
		cargs$master <- x$hostname
		cargs$port <- x$port
		bpbackend(x) <- do.call(makeCluster, cargs)
		if ( inherits(bpbackend(x), "SOCKcluster") ) {
			BiocParallel::.bpstart_impl(x)
		} else {
			matter_error("failed to start cluster")
		}
	})

setMethod("bpstop", "SnowfastParam",
	function(x) {
		if ( !bpisup(x) )
			return(invisible(x))
		x <- BiocParallel::.bpstop_impl(x)
		stopCluster(bpbackend(x))
		bpbackend(x) <- NULLcluster()
		invisible(x)
	})

#### Parallel RNG ####
## --------------------

RNGStreams <- function(n = length(size), size = 1L) {
	if ( length(size) != n )
		size <- rep_len(size, n)
	s <- getRNGStream()
	if ( !is.null(s$seed) ) {
		oseed <- s$seed
		on.exit(setRNGStream(oseed))
	}
	seeds <- vector("list", n)
	for ( i in seq_len(n) )
	{
		seeds[[i]] <- s
		if ( s$kind == "L'Ecuyer-CMRG" ) {
			if ( size[i] > 1L ) {
				for ( j in seq_len(size[i]) )
					s$seed <- nextRNGSubStream(s$seed)
			} else {
				s$seed <- nextRNGStream(s$seed)
			}
		} else {
			runif(size[i])
			s$seed <- getRNGStream()$seed
		}
	}
	seeds
}

getRNGStream <- function() {
	if ( exists(".Random.seed", envir=globalenv()) ) {
		seed <- get(".Random.seed", envir=globalenv())
	} else {
		seed <- NULL
	}
	list(seed=seed, kind=RNGkind()[1L])
}

setRNGStream <- function(seed = NULL, kind = NULL) {
	if ( is.list(seed) ) {
		kind <- seed$kind
		seed <- seed$seed
	}
	RNGkind(kind)
	if ( is.null(seed) ) {
		if ( exists(".Random.seed", envir=globalenv()))
			rm(".Random.seed", envir=globalenv())
	} else {
		assign(".Random.seed", seed, envir=globalenv())
	}
}

