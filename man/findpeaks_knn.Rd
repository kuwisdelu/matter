\name{findpeaks_knn}

\alias{findpeaks_knn}
\alias{knnmax}
\alias{knnmin}

\title{Peak Detection using K-nearest neighbors}

\description{
    Find peaks in a signal based on its local maxima, as determined by K-nearest neighbors.
}

\usage{
# Find peaks with KNN
findpeaks_knn(x, index, k = 5L,
    snr = NULL, noise = "quant", relheight = 0.005,
    arr.ind = is.array(x), useNames = TRUE, ...)

# Local maxima
knnmax(x, index, k = 5L)

# Local minima
knnmin(x, index, k = 5L)
}

\arguments{
	\item{x}{A numeric vector or array.}

    \item{index}{A matrix or list of domain vectors giving the sampling locations of \code{x} if it is a multidimensional signal. May be omitted if \code{x} is an array.}

    \item{k}{The number of nearest neighbors to consider when determining whether a sample is a local extremum.}

    \item{snr}{The minimum signal-to-noise ratio used for filtering the peaks.}

    \item{noise}{The method used to estimate the noise. See \emph{Details}.}

    \item{relheight}{The minimum relative height (proportion of the maximum peak value) used for filtering the peaks.}

    \item{arr.ind}{Should array indices be returned? Otherwise, return linear indices.}

    \item{useNames}{Passed to \code{arrayInd}.}

    \item{\dots}{Arguments passed to the noise estimation function.}
}

\details{
    For \code{knnmax()} and \code{knnmin()}, a local extremum is defined as an element greater (or less) than all of its nearest neighbors with a lesser index, and greater (or less) than or equal to all of its nearest neighbors with a greater or equal index. That is, ties are resolved such that the sample with the lowest index is considered the local extremum.

    For \code{findpeaks_knn()}, the peaks are simply the local maxima of the signal. The peaks are optionally filtered based on their relative heights.

    Optionally, the signal-to-noise ratio (SNR) can be estimated and used for filtering the peaks. These use the functions \code{\link{estnoise_quant}}, \code{\link{estnoise_diff}}, \code{\link{estnoise_filt}}, etc., to estimate the noise in the signal.
}

\value{
    For \code{knnmax()} and \code{knnmin()}, an logical vector or array indicating whether each element is a local extremum.

    For \code{findpeaks_knn()}, an integer vector (or matrix if \code{arr.ind=TRUE}) giving the indices of the peaks.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{findpeaks}},
    \code{\link{estnoise_quant}},
    \code{\link{estnoise_sd}},
    \code{\link{estnoise_mad}},
    \code{\link{estnoise_diff}},
    \code{\link{estnoise_filt}}
}

\examples{
# simple 2D signal
x <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 2, 0, 0, 1, 4, 2, 0, 1, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 3, 2, 1, 0, 0,
    0, 0, 0, 0, 1, 3, 3, 0, 0, 1, 4, 4, 3, 1, 0,
    0, 0, 0, 0, 0, 3, 2, 0, 1, 0, 3, 2, 3, 0, 0,
    0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 2, 3, 0, 0)
x <- matrix(x, nrow=7, ncol=15, byrow=TRUE)

# find peaks in 2D using KNN
findpeaks_knn(x)
}

\keyword{spatial}
