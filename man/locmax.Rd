\name{locmax}

\alias{locmax}

\title{Local Maxima}

\description{
    Find the indices of the local maxima of a vector.
}

\usage{
locmax(x, width = 5)
}

\arguments{
	\item{x}{A numeric vector.}

	\item{width}{The number of vector elements to consider when determining if the center of the window is a local maximum.}

	\item{findLimits}{If \code{TRUE}, then also return the approximate boundaries of the peak.}
}

\details{
    For this function, a local maximum is defined as an element greater than all of the elements within \code{width / 2} elements to the left of it, and greater than or equal to all of the elements within \code{width / 2} elements to the right of it.

    The boundaries are found by descending the local maxima until the elements are no longer non-increasing. Small increases within \code{halfWindow} of the local maxima are ignored.
}

\value{
    An logical vector indicating whether each element is a local maximum.
}

\author{Kylie A. Bemis}

\examples{
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)

locmax(x)
}

\keyword{utilities}
