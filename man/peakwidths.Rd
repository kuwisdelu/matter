\name{peakwidths}

\alias{peakwidths}
\alias{peakareas}
\alias{peakheights}

\title{Peak Summarization}

\description{
    Summarize peaks based on their shapes and properties.
}

\usage{
# Get peak widths
peakwidths(x, peaks, domain = NULL,
    fmax = 0.5, ref = c("height", "prominence"))

# Get peak areas
peakareas(x, peaks, domain = NULL)

# Get peak heights
peakheights(x, peaks)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{peaks}{The indices (or domain values) of peaks for which the widths or areas should be calculated.}

	\item{domain}{The domain variable of the signal.}

    \item{fmax}{The fraction of the peak's height used for determining the peak's width.}

    \item{ref}{The reference value of the peak for determining the peak width: either the peak height of the peak prominence.}
}

\value{
    A numeric vector giving the widths, areas, or heights of the peaks with attributes 'left_bounds' and 'right_bounds' giving the left and right boundaries of the peak.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{findpeaks}},
    \code{\link{findpeaks_cwt}},
    \code{\link{binpeaks}},
    \code{\link{mergepeaks}}
}

\examples{
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)

p <- findpeaks(x)

peakareas(x, p)
peakheights(x, p)
}

\keyword{ts}
