\name{SnowfastParam-class}
\docType{class}

\alias{class:SnowfastParam}
\alias{SnowfastParam}
\alias{SnowfastParam-class}

\alias{bpstart,SnowfastParam-method}
\alias{bpstop,SnowfastParam-method}

\alias{bpislocal}
\alias{bplocalized}
\alias{bptrystart}
\alias{bptrystop}

\title{Fast Simple Network of Workstations (SNOW)-style Parallel Backend}

\description{
    This class provides a enhanced version of the \code{\link{SnowParam}} parallel backend from \code{BiocParallel} based on the newer PSOCK cluster implementation from the \code{parallel} package.
}

\usage{
## Instance creation
SnowfastParam(workers = snowWorkers(),
    tasks = 0L, stop.on.error = TRUE, progressbar = FALSE,
    RNGseed = NULL, timeout = WORKER_TIMEOUT,
    exportglobals = TRUE, exportvariables = TRUE,
    resultdir = NA_character_, jobname = "BPJOB",
    force.GC = FALSE, fallback = TRUE, useXDR = FALSE,
    manager.hostname = NA_character_, manager.port = NA_character_, \dots)

## Additional methods documented below
}

\arguments{
    \item{workers}{Either the number of workers or the names of cluster nodes.}

    \item{tasks}{The number of tasks per job. See \code{\link{SnowParam}} for details.}

    \item{stop.on.error}{Enable stop on error. See \code{\link{SnowParam}} for details.}

    \item{progressbar}{Enable text progress bar. See \code{\link{SnowParam}} for details.}

    \item{RNGseed}{The seed for random number generation. See \code{\link{SnowParam}} for details.}

    \item{timeout}{Time (in seconds) allowed for workers to complete a task. See \code{\link{SnowParam}} for details.}

    \item{exportglobals}{Export \code{base::options()} from manager to workers? See \code{\link{SnowParam}} for details.}

    \item{exportvariables}{Automatically export variables defined in the global environment and used by the function? See \code{\link{SnowParam}} for details.}

    \item{resultdir}{Job results directory. See \code{\link{SnowParam}} for details.}

    \item{jobname}{The name of the job for logging and results. See \code{\link{SnowParam}} for details.}

    \item{force.GC}{Whether to invoke the garbage collector after each call to \code{FUN}. See \code{\link{SnowParam}} for details.}

    \item{fallback}{Fall back to using \code{SerialParam} if the cluster is not started and the number of workers is no greater than 1? See \code{\link{SnowParam}} for details.}

    \item{useXDR}{Should data be converted to network byte order when serializing from the manager to the workers? The default (\code{FALSE}) assumes all nodes in the cluster are little endian. Passed to \code{\link{makePSOCKcluster}}.}

    \item{manager.hostname}{Host name of the manager node.}

    \item{manager.port}{Port on the manager with which workers communicate.}

    \item{\dots}{Additional arguments passed to \code{\link{makePSOCKcluster}}.}
}

\details{
    \code{SnowfastParam} is a faster but somewhat more limited version of \code{SnowParam}. Like \code{SnowParam}, it uses simple network of workstations (SNOW)-style parallelism so that it is available on all operating systems.

    The workers are initialized in parallel, so cluster startup can often be significantly faster than \code{SnowParam} if utilizing a large number of workers.

    Because the workers are started using \code{\link{makePSOCKcluster}} from the \code{parallel} package rather than using \code{BiocParallel}'s startup script, some features of \code{BiocParallel}-managed backends such as logging are unsupported or only partially available.

    The default parameter \code{useXDR=TRUE} assumes that all nodes are little endian so that data can be sent to workers without the overhead of conversion to network byte order. This should result in overall faster performance for large datasets on compatible clusters.

    \code{SnowfastParam} is intended to be used as a drop-in replacement for \code{SnowParam} in most situations and as a more stable alternative to \code{MulticoreParam} for long-running jobs in graphical environments like RStudio where forking the R process should be avoided.
}

\section{Methods}{
    \code{\link{BiocParallelParam}} methods:
    \describe{
        \item{\code{bpstart(x):}}{Start the cluster.}

        \item{\code{bpstop(x):}}{Stop the cluster.}
    }

    Additional methods documented in \code{\link{BiocParallelParam}}.
}

\value{
    An parallel backend derived from class \code{\link{BiocParallelParam}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{SnowParam}},
    \code{\link{MulticoreParam}}
}

\examples{
x <- replicate(1000, runif(200), simplify=FALSE)

bp <- SnowfastParam(workers=4, tasks=20, progressbar=TRUE)

ans <- chunkLapply(x, sum, BPPARAM=bp)
}

\keyword{classes}
