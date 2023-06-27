\name{filt1}

\alias{filt1}

\alias{filt1_ma}
\alias{filt1_gauss}
\alias{filt1_bi}
\alias{filt1_adapt}
\alias{filt1_guide}
\alias{filt1_pag}

\title{Smoothing Filters in 1D}

\description{
    Smooth a uniformly sampled 1D signal.
}

\usage{
# Moving average filter
filt1_ma(x, width = 5L)

# Gaussian filter
filt1_gauss(x, width = 5L, sd = (width \%/\% 2) / 2)

# Bilateral filter
filt1_bi(x, width = 5L,
    sddist = (width \%/\% 2) / 2, sdrange = 2 * qmad(x))

# Bilateral filter with adaptive parameters
filt1_adapt(x, width = 5L, spar = 1)

# Guided filter
filt1_guide(x, width = 5L, guide = x,
    sdreg = 2 * qmad(x))

# Peak-aware guided filter
filt1_pag(x, width = 5L, guide = x,
    sdreg = 2 * qmad(x), ftol = 1/10)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{width}{The width of the smoothing window in number of samples. Must be positive. Must be odd.}

    \item{sd, sddist}{The spatial parameter for kernel-based filters. This controls the strength of smoothing for samples farther from the center of the smoothing window.}

    \item{sdrange}{The range parameter for kernel-based filters. This controls the strength of the smoothing for samples with signal values very different from the center of the smoothing window.}

    \item{spar}{The strength of the smoothing when calculating the adaptive bilateral filtering parameters. The larger the number, the stronger the smoothing. Must be positive.}

    \item{guide}{The guide signal for guided filtering. This is the signal used to determine the degree of filtering for different regions of the sample. By default, it is the same as the signal to be smoothed.}

    \item{sdreg}{The regularization parameter for guided filtering. This is analagous to the range parameter for kernel-based filters. Signal regions with variance much smaller than this value are smoothed, while signal regions with varaince much larger than this value are preserved.}

	\item{ftol}{Specifies how large the signal value must be before it is considered a peak, expressed as a fraction of the maximum value in the signal.}
}

\details{
    \code{filt1_ma()} performs mean filtering in O(n) time. This is fast and especially useful for calculating other filters that can be constructed as a combination of mean filters.

    \code{filt1_gauss()} performs Gaussian filtering.

    \code{filt1_bi()} and \code{filt1_adapt()} perform edge-preserving bilateral filtering. The latter calculates the kernel parameters adapatively based on the local signal, using a strategy adapted from Joseph and Periyasamy (2018).

    \code{filt1_guide()} and \code{filt1_pag()} perform edge-preserving guided filtering. Guided filtering performs similarly to bilateral filtering, but is often faster, as it is implemented as a combination of mean filters. The latter uses a regularization parameter that focuses on preserving peaks rather than edges, using a strategy adapted from Liu and He (2022).
}

\value{
    A numeric vector the same length as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\references{
    J. Joseph and R. Perisamy. ``An image driven bilateral filter with adaptive range and spatial parameters for denoising Magnetic Resonance Images.'' Computers and Electrical Engineering, vol. 69, pp. 782-795, July 2018.

    K. He, J. Sun, and X. Tang. ``Guided Image Filtering.'' IEEE Transactions on Pattern Analysis and Machine Intelligence, vol. 35, no. 6, pp. 1397-1409, June 2013.

    D. Liu and C. He. ``Peak-aware guided filtering for spectrum signal denoising.'' Chemometrics and Intelligent Laboratory Systems, vol. 222, March 2022.
}

\examples{
set.seed(1)
t <- seq(from=0, to=6 * pi, length.out=5000)
y <- sin(t) + 0.6 * sin(2.6 * t)
x <- y + runif(length(y))
xs <- filt1_gauss(x, width=25)
if ( interactive() ) {
    plot(x, type="l")
    lines(xs, col="red")
}
}

\keyword{ts}
\keyword{smooth}
