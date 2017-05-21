\name{profmem}

\alias{profmem}
\alias{mem}

\title{Profile Memory Use}

\description{
    These are utility functions for profiling memory used by objects and by R during the execution of an expression.
}

\usage{
profmem(expr)

mem(x, reset = FALSE)
}

\arguments{
	\item{expr}{An expression to be evaluated.}

	\item{x}{An object, to identify how much memory it is using.}

	\item{reset}{Should the maximum memory used by R be reset?}
}

\details{
    These are wrappers around the built-in \code{\link[base]{gc}} function. Note that they only count memory managed by R.
}

\value{
    For \code{profmem}, a vector giving [1] the amount of memory used at the start of execution, [2] the amount of memory used at the end of execution, [3] the maximum amount of memory used during execution, [4] the memory overhead as defined by the maximum memory used minus the starting memory use, and [5] the execution time in seconds.

    For \code{mem}, either a single numeric value giving the memory used by an object, or a vector providing a more readable version of the information returned by \code{\link[base]{gc}} (see its help page for details).
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{gc}},
}

\examples{
x <- 1:100

mem(x)

profmem(mean(x + 1))
}

\keyword{methods}
\keyword{arith}
