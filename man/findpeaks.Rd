\name{findpeaks}

\alias{locmax}
\alias{locmin}
\alias{findpeaks}

\alias{peakwidths}
\alias{peakareas}

\alias{binpeaks}
\alias{mergepeaks}

\title{Peak Processing}

\description{
    Find peaks in a signal based on its local maxima, as determined by a sliding window.
}

\usage{
# Local maxima
locmax(x, width = 5)

# Local minima
locmin(x, width = 5)

# Find peaks
findpeaks(x, prominence = NULL, bounds = TRUE)

# Get peak widths
peakwidths(x, peaks, domain = NULL,
    fmax = 0.5, ref = c("height", "prominence"))

# Get peak areas
peakareas(x, peaks, domain = NULL)

# Bin a list of peaks
binpeaks(peaklist, domain = NULL, xlist = peaklist,
    tol = NA_real_, tol.ref = "abs", merge = TRUE,
    na.drop = TRUE)

# Merge peaks
mergepeaks(peaks, n = nobs(peaks), x = peaks,
    tol = NA_real_, tol.ref = "abs",
    na.drop = TRUE)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{width}{The number of signal elements to consider when determining if the center of the sliding window is a local extremum.}

    \item{prominence}{The minimum peak prominence used for filtering the peaks. The prominence of a peak is the height that the peak rises above the higher of its bases (i.e., its lowest contour line). A peak's bases are found as the local minima between the peak and the next higher peaks on either side.}

    \item{bounds}{Whether the boundaries of each peak should be calculated and returned. A peak's boundaries are found as the nearest local minima on either side.}

    \item{peaks}{The indices (or domain values) of peaks which should be merged, or for which the widths or areas should be calculated.}

	\item{domain}{The domain variable of the signal.}

    \item{fmax}{The fraction of the peak's height used for determining the peak's width.}

    \item{ref}{The reference value of the peak for determining the peak width: either the peak height of the peak prominence.}

    \item{peaklist, xlist}{A list of vectors of peak indices (or domain values), and the values to be binned according to the peak locations.}

    \item{tol, tol.ref}{A tolerance specifying the maximum allowed distance between binned or merged peaks. See \code{\link{bsearch}} for details. If missing, \code{binpeaks} estimates it as one half the minimum gap between same-signal peaks, and \code{mergepeaks} estimates it as one hundredth of the average gap beteen peaks.}

    \item{merge}{Should the binned peaks be merged?}

    \item{na.drop}{Should missing values be dropped from the result?}

    \item{n}{The number of times each peak was observed. This is used to weight the averaging.}
}

\details{
    For \code{locmax()} and \code{locmin()}, a local extremum is defined as an element greater (or less) than all of the elements within \code{width / 2} elements to the left of it, and greater (or less) than or equal to all of the elements within \code{width / 2} elements to the right of it.

    For \code{findpeaks()}, the peaks are simply the local maxima of the signal. The peak boundaries are found by descending a local maximum until a local minimum is found on either side, using the same criteria as above. The peaks are optionally filtered based on their prominences.

    \code{peakwidths()} and \code{peakareas()} calculate the widths or areas of the peaks.

    \code{binpeaks()} is used to bin a list of peaks from multiple signals to a set of common peaks. This is done by creating a dummy reference based on the given tolerance and the range of the domain. The peaks (or corresponding values) are then binned to the dummy reference and averaged within each bin.

    \code{mergepeaks()} is used to merge any peaks with gaps smaller than the given tolerance. The merged peaks are averaged together.
}

\value{
    For \code{locmax()} and \code{locmin()}, an logical vector indicating whether each element is a local maximum.

    For \code{findpeaks()}, an integer vector giving the indices of the peaks, with attributes 'left_bounds' and 'right_bounds' giving the left and right boundaries of the peak as determined using the rule above.
}

\author{Kylie A. Bemis}

\examples{
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)

locmax(x)

findpeaks(x)
}

\keyword{ts}
