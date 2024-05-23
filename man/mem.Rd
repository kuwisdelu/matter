\name{mem}

\alias{mem}
\alias{mtime}

\alias{memtime}
\alias{profmem}

\title{Check Memory Use}

\description{
    These are utility functions for checking memory used by objects and by R during the execution of an expression.
}

\usage{
mem(x, reset = FALSE)

mtime(expr)
}

\arguments{
	\item{x}{An object, to identify how much memory it is using.}

	\item{reset}{Should the maximum memory used by R be reset?}

    \item{expr}{An expression to be evaluated.}
}

\details{
    These are wrappers around the built-in \code{\link{gc}} and \code{link{proc.time}} functions. Note that they only count memory managed by R.
}

\value{
    For \code{mtime}, a vector giving [1] the amount of memory used at the start of execution, [2] the amount of memory used at the end of execution, [3] the maximum amount of memory used during execution, [4] the memory overhead as defined by the maximum memory used minus the starting memory use, and [5] the execution time in seconds.

    For \code{mem}, either a single numeric value giving the memory used by an object, or a vector providing a more readable version of the information returned by \code{\link{gc}} (see its help page for details).
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{gc}},
}

\examples{
x <- 1:100

mem(x)

mtime(mean(x + 1))
}

\keyword{utilities}
