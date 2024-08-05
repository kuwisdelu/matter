\name{mem}

\alias{mem}
\alias{memtime}

\title{Check Memory Use}

\description{
    These are utility functions for checking memory used by R and \code{matter} in the current session and/or during the timed execution of an expression.
}

\usage{
mem(x, reset = FALSE)

memtime(expr, verbose = NA, BPPARAM = NULL)
}

\arguments{
    \item{x}{An object, to summarize how much memory it is using.}

    \item{reset}{Should the maximum memory values be reset?}

    \item{expr}{An expression to be evaluated.}

    \item{verbose}{Should timing messages be printed?}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. Used to collect memory usage from a cluster during timing. Must have a \code{bpbackend} of class \code{cluster}.}
}

\details{
    These functions summarize the memory used by both traditional R objects and out-of-memory \code{matter} objects. "Real" memory managed by R is summarized using \code{\link{gc}}. "Virtual" memory managed by \code{matter} includes shared memory allocated by \code{matter} and temporary files created by \code{matter} in \code{getOption("matter.temp.dir")}.

    For timing parallel code, it is useful to use \code{memtime} in combination with its \code{BPPARAM} argument to monitor the amount of memory used by the cluster.
}

\value{
    For \code{mem} called with an \code{x} argument, a named vector summarizing the memory and storage used by the object. The named elements include:

    \itemize{
        \item{\code{"real"}: The amount of R memory used.}
        \item{\code{"shared"}: The amount of shared memory used.}
        \item{\code{"virtual"}: The amount of virtual memory used, including both file storage and shared memory.}
    }

    For \code{mem} called without an \code{x} argument, a named vector summarizing the memory and storage used by the current R session. The named elements include:

    \itemize{
        \item{\code{"real"}: The amount of R memory used.}
        \item{\code{"shared"}: The amount of shared memory used.}
        \item{\code{"max real"}: The maximum amount of R memory used since the last reset.}
        \item{\code{"max shared"}: The maximum amount of shared memory used since the last reset.}
        \item{\code{"temp"}: The total size of temporary files managed by \code{matter} in \code{getOption("matter.temp.dir")}.}
    }

    For \code{memtime}, a list including:

    \itemize{
        \item{\code{"start"}: Memory use at the start of timing.}
        \item{\code{"end"}: Memory use at the end of timing.}
        \item{\code{"cluser"}: (Optional.) A summary of the memory used by the cluster if \code{BPPARAM} is specified.}
        \item{\code{"overhead"}: The amount of "real" memory used during the execution of \code{expr} that is freed by the end of timing.}
        \item{\code{"change"}: The difference in "real" memory used before and after timing.}
        \item{\code{"total"}: For the current R session, the sum of "max real" and "max shared" memory used. For a cluster, the sum of "max real" memory used by all workers (not including shared memory or the managing R process).}
        \item{\code{"time"}: The execution time.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{gc}}
}

\examples{
x <- 1:100

mem(x)

memtime(mean(x + 1))
}

\keyword{utilities}
