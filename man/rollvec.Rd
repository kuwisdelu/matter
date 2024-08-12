\name{rollvec}

\alias{rollvec}
\alias{roll}

\title{Rolling Summaries of a Vector}

\description{
    Summarize a vector in rolling windows.
}

\usage{
rollvec(x, width, stat = "sum", prob = 0.5)
}

\arguments{
	\item{x}{A numeric vector.}

	\item{width}{The width of the rolling window. Must be odd.}

	\item{stat}{The statistic used to summarize the values in each bin. Must be one of "sum", "mean", "max", "min", "sd", "var", "mad", or "quantile".}

	\item{prob}{The quantile for \code{stat = "quantile"}.}
}

\value{
    An numeric vector with the same length as \code{x} with the summarized values from each rolling window.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)

x <- sort(runif(20))

rollvec(x, 5L, "mean")
}

\keyword{utilities}
