\name{shingles}

\alias{shingles}

\title{Cleveland-Style Shingles}

\description{
    Shingles are a generalization of factors to continuous  variables. Cleveland-style shingles distribute the range of a continuous variable into overlapping discrete intervals, which can be more useful for visualization than mutually-exclusive bins.
}

\usage{
shingles(x, breaks, overlap = 0.5, labels = NULL)
}

\arguments{
	\item{x}{A numeric vector.}

	\item{breaks}{Either the number of intervals or a matrix of breaks (as returned by \code{\link{co.intervals}}).}

    \item{overlap}{The fraction of overlap between intervals.}

    \item{labels}{The names of the intervals.}
}

\value{
    A list giving the indices of \code{x} in each shingle with the following attributes:

    \itemize{
        \item{\code{breaks}: A matrix where each row gives the lower and upper limits for each shingle.}
        \item{\code{counts}: The number of observations in each shingle.}
        \item{\code{mids}: The center of each shingle.}
    }
}

\author{Kylie A. Bemis}

\references{
    W. S. Cleveland. \emph{Visualizing Data.} New Jersey: Summit Press. 1993.
}

\seealso{
    \code{\link{co.intervals}}
}

\examples{
set.seed(1)
x <- rnorm(100)

shingles(x, 6)
}

\keyword{dplot}
