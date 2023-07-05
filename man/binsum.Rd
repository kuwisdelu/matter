\name{binsum}

\alias{binsum}
\alias{binvec}

\title{Bin a signal}

\description{
    Summarize a signal in the bins at the specified indices.
}

\usage{
binsum(x, lower, upper, stat = "sum")
}

\arguments{
	\item{x}{A numeric vector.}

	\item{lower, upper}{The (inclusive) lower and upper indices of the bins}

	\item{stat}{The statistic used to summarize the values in each bin. Must be one of "sum", "mean", "max", "min", "sd", and "var".}
}

\value{
    An vector of the summarized (binned) values.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)

x <- sort(runif(20))

binsum(x, c(1,6,11,16), c(5,10,15,20))

binsum(x, seq(from=1, to=21, by=5), stat="mean")
}

\keyword{utilities}
