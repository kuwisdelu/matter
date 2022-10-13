\name{binvec}

\alias{binvec}

\title{Bin a vector}

\description{
    Bin a vector based on intervals or groups.
}

\usage{
binvec(x, u, v, method = "sum")
}

\arguments{
	\item{x}{A numeric vector.}

	\item{u, v}{The (inclusive) lower and upper indices of the bins, or a factor providing the groupings.}

	\item{method}{The method used to bin the values. This is efficiently implemented for "sum", "mean", "min" or "max". Providing a function will use a less-efficient fallback.}
}

\value{
    An vector of the summarized (binned) values.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)

x <- sort(runif(20))

binvec(x, c(1,6,11,16), c(5,10,15,20), method="mean")

binvec(x, seq(from=1, to=21, by=5), method="mean")
}

\keyword{utilities}
