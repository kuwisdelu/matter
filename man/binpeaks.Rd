\name{binpeaks}

\alias{binpeaks}
\alias{mergepeaks}

\title{Peak Processing}

\description{
    Combine peaks from multiple signals.
}

\usage{
# Bin a list of peaks
binpeaks(peaklist, domain = NULL, xlist = peaklist,
    tol = NA_real_, tol.ref = "abs", merge = FALSE,
    na.drop = TRUE)

# Merge peaks
mergepeaks(peaks, n = nobs(peaks), x = peaks,
    tol = NA_real_, tol.ref = "abs",
    na.drop = TRUE)
}

\arguments{
    \item{peaklist, xlist}{A list of vectors of peak indices (or domain values), and the values to be binned according to the peak locations.}

    \item{peaks, x}{The indices (or domain values) of peaks which should be merged, or for which the corresponding values should be averaged. If \code{n} is not provided, this should be a numeric \code{stream_stat} vector produced by \code{binpeaks()}.}

	\item{domain}{The domain variable of the signal.}

    \item{tol, tol.ref}{A tolerance specifying the maximum allowed distance between binned or merged peaks. See \code{\link{bsearch}} for details. If missing, \code{binpeaks} estimates it as one half the minimum gap between same-signal peaks, and \code{mergepeaks} estimates it as one hundredth of the average gap beteen peaks.}

    \item{merge}{Should the binned peaks be merged?}

    \item{na.drop}{Should missing values be dropped from the result?}

    \item{n}{The number of times each peak was observed. This is used to weight the averaging.}
}

\details{
    \code{binpeaks()} is used to bin a list of peaks from multiple signals to a set of common peaks. This is done by creating a dummy reference based on the given tolerance and the range of the domain. The peaks (or corresponding values) are then binned to the dummy reference and averaged within each bin.

    \code{mergepeaks()} is used to merge any peaks with gaps smaller than the given tolerance. The merged peaks are averaged together.
}

\value{
    A numeric \code{stream_stat} vector, giving the average locations of each peak.
}

\author{Kylie A. Bemis}

\examples{
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)
y <- c(0, 1, 1, 3, 2, 2, 1, 5, 4, 1, 1, 0)

p1 <- findpeaks(x)
p2 <- findpeaks(y)
binpeaks(list(p1, p2), merge=FALSE)
}

\keyword{ts}
