\name{findpeaks_cwt}

\alias{findpeaks_cwt}
\alias{findridges}
\alias{cwt}

\alias{ricker}

\title{CWT-based Peak Detection}

\description{
    Find peaks in a signal using continuous wavelet transform (CWT).
}

\usage{
# Find peaks with CWT
findpeaks_cwt(x, snr = 2, wavelet = ricker, scales = NULL,
    maxdists = scales, ngaps = 3L, ridgelen = length(scales) \%/\% 4L,
    qnoise = 0.95, width = length(x) \%/\% 20L, bounds = TRUE)

# Find ridges lines in a matrix
findridges(x, maxdists, ngaps)

# Continuous Wavelet Transform
cwt(x, wavelet = ricker, scales = NULL)
}

\arguments{
	\item{x}{A numeric vector for \code{findpeaks_cwt()} and \code{cwt()}. A matrix of CWT coefficients for \code{findridges()}.}

    \item{snr}{The minimum signal-to-noise ratio used for filtering the peaks.}

    \item{wavelet}{The wavelet to be convolved with the signal. Must be a function that takes two arguments: the number of points in the wavelet \code{n} as the first argument and the scale \code{a} of the wavelet as the second argument. The default \code{ricker()} function satisfies this.}

    \item{scales}{The scales at which to perform CWT. A reasonable sequence is generated automatically if not provided.}

    \item{maxdists}{The maximum allowed shift distance between local maxima allowed when connecting maxima into ridge lines. Should be a vector the same length as \code{scales}.}

    \item{ngaps}{The number of gaps allowed in a ridge line before it is removed from the search space.}

    \item{ridgelen}{The minimum ridge length allowed when filtering peaks.}

    \item{qnoise}{The quantile of the CWT coefficients at the smallest scale used to estimate the noise.}

    \item{width}{The width of the rolling estimation of noise quantile.}

    \item{bounds}{Whether the boundaries of each peak should be calculated and returned. A peak's boundaries are found as the nearest local minima on either side.}
}

\details{
    \code{findpeaks_cwt()} uses the peak detection method based on continuous wavelet transform (CWT) proposed by Du, Kibbe, and Lin (2006).

    The raw signal is convolved with a wavelet (by default, a Ricker wavelet is used) at a range of different scales. This produces a matrix of CWT coefficients with a number of rows equal to the length of the original signal and each column representing a different scale of convolution.

    The convolution at the smallest scales represent a good estimate of noise and peak location. The larger scales represent a smoother signal where larger peaks are prominent and smaller peaks are removed.

    The method proceeds by identifying ridge lines in the CWT coefficient matrix using \code{findridges()}. Local maxima are identified at each scale and connected across each scale, forming the ridge lines.

    Finally, the local noise is estimated from the CWT coefficients at the smallest scale. The peaks are filtered based on signal-to-noise ratio and the length of their ridge lines.
}

\value{
    For \code{findpeaks_cwt()}, an integer vector giving the indices of the peaks, with attributes 'left_bounds' and 'right_bounds' giving the left and right boundaries of the peak as determined using the rule above.

    For \code{findridges()}, a list of matrices giving the row and column indices of the entries of each detected ridge line.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{findpeaks}},
    \code{\link{peakwidths}},
    \code{\link{peakareas}},
    \code{\link{peakheights}},
    \code{\link{binpeaks}},
    \code{\link{mergepeaks}}
}

\examples{
# simple signal
x <- c(0, 1, 1, 2, 3, 2, 1, 4, 5, 1, 1, 0)
locmax(x)
findpeaks(x)

# simulated spectrum
set.seed(1)
x <- simspec(size=5000)

# find peaks with snr >= 3
p <- findpeaks_cwt(x, snr=3)
plot(x, type="l")
points(p, x[p], col="red")

# plot ridges
ridges <- attr(p, "ridges")
plot(c(0, length(x)), c(0, 25), type="n")
for ( ri in ridges )
    lines(ri, type="o", pch=20, cex=0.5)
}

\keyword{ts}
