\name{findpeaks}

\alias{findpeaks}
\alias{locmax}

\title{Find Peaks Based on Local Maxima}

\description{
    Find peaks in a signal based on its local maxima, as determined by a sliding window.
}

\usage{
findpeaks(x, prominence = NULL, bounds = TRUE)

locmax(x, width = 5)

locmin(x, width = 5)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{prominence}{The minimum peak prominence used for filtering the peaks.}

    \item{bounds}{Whether the boundaries of each peak should be calculated and returned.}

	\item{width}{The number of signal elements to consider when determining if the center of the sliding window is a local extremum.}
}

\details{
    For this function, a local maximum is defined as an element greater than all of the elements within \code{width / 2} elements to the left of it, and greater than or equal to all of the elements within \code{width / 2} elements to the right of it.

    The peaks in this case are simply the local maxima of the signal. The peak boundaries are found by descending a local maximum until a local minimum is found on either side, using the same criteria as above.
}

\value{
    For \code{locmax()}, an logical vector indicating whether each element is a local maximum.

    For \code{findpeaks()}, an integer vector giving the indices of the peaks, with attributes 'left_bounds' and 'right_bounds' giving the left and right boundaries of the peak as determined using the rule above.
}

\author{Kylie A. Bemis}

\examples{
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)

locmax(x)

findpeaks(x)
}

\keyword{utilities}
