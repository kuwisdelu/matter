\name{filt2}

\alias{filt2}

\alias{filt2_ma}
\alias{filt2_conv}
\alias{filt2_gauss}
\alias{filt2_bi}
\alias{filt2_adapt}
\alias{filt2_diff}
\alias{filt2_guide}

\title{Smoothing Filters in 2D}

\description{
    Smooth a uniformly sampled 2D signal.
}

\usage{
# Moving average filter
filt2_ma(x, width = 5L)

# Linear convolution filter
filt2_conv(x, weights)

# Gaussian filter
filt2_gauss(x, width = 5L, sd = (width \%/\% 2) / 2)

# Bilateral filter
filt2_bi(x, width = 5L, sddist = (width \%/\% 2) / 2,
    sdrange = 2 * mad(x, na.rm = TRUE))

# Bilateral filter with adaptive parameters
filt2_adapt(x, width = 5L, spar = 1)

# Nonlinear diffusion
filt2_diff(x, niter = 5, kappa = 50,
    rate = 0.25, method = 1L)

# Guided filter
filt2_guide(x, width = 5L, guide = x,
    sdreg = 2 * mad(x, na.rm = TRUE))
}

\arguments{
	\item{x}{A numeric matrix.}

    \item{width}{The width of the smoothing window in number of samples. Must be positive. Must be odd.}

    \item{weights}{A matrix of weights for the linear convolution kernel. Dimensions must be odd.}

    \item{sd, sddist}{The spatial parameter for kernel-based filters. This controls the strength of smoothing for samples farther from the center of the smoothing window.}

    \item{sdrange}{The range parameter for kernel-based filters. This controls the strength of the smoothing for samples with signal values very different from the center of the smoothing window.}

    \item{spar}{The strength of the smoothing when calculating the adaptive bilateral filtering parameters. The larger the number, the stronger the smoothing. Must be positive.}

    \item{kappa}{The constant for the conduction coefficient for nonlinear diffusion. Must be positive.}

    \item{rate}{The rate of diffusion. Must be between 0 and 0.25 for stability.}

    \item{method}{The diffusivity method, where \code{1} and \code{2} correspond to the two diffusivity functions proposed by Perona and Malik (1990). For \code{1}, this is \code{exp(-(|grad x|/K)^2)}, and \code{2} is \code{1/(1+(|grad x|/K)^2)}.}

    \item{niter}{The number of iterations for nonlinear diffusion. Must be positive.}

    \item{guide}{The guide signal for guided filtering. This is the signal used to determine the degree of filtering for different regions of the sample. By default, it is the same as the signal to be smoothed.}

    \item{sdreg}{The regularization parameter for guided filtering. This is analagous to the range parameter for kernel-based filters. Signal regions with variance much smaller than this value are smoothed, while signal regions with varaince much larger than this value are preserved.}
}

\details{
    \code{filt2_ma()} performs mean filtering in O(n) time. This is fast and especially useful for calculating other filters that can be constructed as a combination of mean filters.

    \code{filt2_gauss()} performs Gaussian filtering.

    \code{filt2_bi()} and \code{filt2_adapt()} perform edge-preserving bilateral filtering. The latter calculates the kernel parameters adapatively based on the local signal, using a strategy adapted from Joseph and Periyasamy (2018).

    \code{filt2_diff()} performs the nonlinear diffusion filtering of Perona and Malik (1990). Rather than relying on a filter width, it progressively diffuses (smooths) the signal over multiple iterations. More iterations will result in a smoother image.

    \code{filt2_guide()} performs edge-preserving guided filtering. Guided filtering uses a local linear model based on the structure of a so-called "guidance signal". By default, the guidance signal is often the same as the input signal. Guided filtering performs similarly to bilateral filtering, but is often faster (though with more memory use), as it is implemented as a combination of mean filters.
}

\value{
    A numeric matrix the same dimensions as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\references{
    J. Joseph and R. Perisamy. ``An image driven bilateral filter with adaptive range and spatial parameters for denoising Magnetic Resonance Images.'' Computers and Electrical Engineering, vol. 69, pp. 782-795, July 2018.

    P. Perona and J. Malik. ``Scale-space and edge detection using anisotropic diffusion.'' IEEE Transactions on Pattern Analysis and Machine Intelligence, vol. 12, issue 7, pp. 629-639, July 1990.

    K. He, J. Sun, and X. Tang. ``Guided Image Filtering.'' IEEE Transactions on Pattern Analysis and Machine Intelligence, vol. 35, no. 6, pp. 1397-1409, June 2013.
}

\examples{
set.seed(1)
i <- seq(-4, 4, length.out=12)
j <- seq(1, 3, length.out=9)
co <- expand.grid(i=i, j=j)
x <- matrix(atan(co$i / co$j), nrow=12, ncol=9)
x <- 10 * (x - min(x)) / diff(range(x))
x <- x + 2.5 * runif(length(x))
xs <- filt2_gauss(x)
if ( interactive() ) {
    par(mfcol=c(1,2))
    image(x, col=hcl.colors(256), main="original")
    image(xs, col=hcl.colors(256), main="smoothed")
}
}

\keyword{ts}
\keyword{smooth}
