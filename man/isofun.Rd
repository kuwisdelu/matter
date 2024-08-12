\name{isofun}

\alias{isofun}
\alias{isoclos}

\title{Isolate Closures for Serialization}

\description{
    These functions modify the environment of a function to isolate a closure from its original environment.
}

\usage{
isofun(fun, envir = baseenv())

isoclos(fun, envir = baseenv())
}

\arguments{
	\item{fun}{A function to isolate.}

	\item{envir}{The new parent environment for the closure.}
}

\details{
    A common challenge with parallel programming in R is unintentionally serializing large environments that have been captured by closures. The functions \code{isofun} and \code{isoclos} provide straightforward ways to isolate functions and closures from their original parent environments which may contain large objects.

    \code{isofun} simply replaces the environment of \code{fun} with \code{envir}, which defaults to the base environment. This is appropriate for simple functions that do not need to enclose any variables, instead taking all of their inputs as formal arguments.

    \code{isoclos} creates a new closure that is isolated from \code{fun}'s original environment. All objects in \code{environment(fun)} are first copied into a new environment with parent \code{envir}, and then this new environment is assigned to \code{fun}.

    Note that the default \code{envir=baseenv()} means that any functions not in the base environment must be fully qualified (e.g., \code{stats::sd} versus \code{sd}).
}

\value{
    A new function or closure, isolated from \code{environment(fun)}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{environment}}
}

\examples{
register(SerialParam())

bigfun <- function(x)
{
    # isolate 'smallfun' from 'x'
    smallfun <- isofun(function(xi) {
        (xi - mean(xi)) / stats::sd(xi)
    })
    chunkApply(x, 2L, smallfun)
}

set.seed(1)
x <- matrix(runif(100), nrow=10, ncol=10)
bigfun(x)
}

\keyword{utilities}
