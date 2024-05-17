\name{enhance}

\alias{enhance}

\alias{enhance_adj}
\alias{enhance_hist}
\alias{enhance_adapt}

\title{Contrast Enhancement}

\description{
    Enhance the contrast in a 2D signal.
}

\usage{
# Adjust by extreme values
enhance_adj(x, high = 0.01, low = 0.01)

# Histogram equalization
enhance_hist(x, nbins = 256L)

# Contrast-limited adaptive histogram equalization (CLAHE)
enhance_adapt(x, width = sqrt(nrow(x) * ncol(x)) \%/\% 5L,
    clip = 0.1, nbins = 256L)
}

\arguments{
	\item{x}{A numeric matrix.}

    \item{high, low}{The quantile at which to clip the maximum and minimum of the signal.}

    \item{nbins}{The number of gray levels in the output image.}

    \item{clip}{The normalized clip limit, expressed as a fraction of the neighborhood size. This is used to limit the maximum value of any bin in the adaptive histograms, in order avoid amplifying local noise.}

    \item{width}{The width of the sliding window used when calculating the local adaptive histograms.}
}

\details{
    \code{enhance_heq()} performs histogram equalization. Histogram equalization transforms the pixel values so that the histogram of the image is approximately flat. This is done by replacing the original pixel values with their associated probability in the image's empirical cumulative distribution.

    \code{enhance_aheq()} performs contrast-limited adaptive histogram equalization (CLAHE) from Zuiderveld (1994). While ordinary histogram equalization performs a global transformation on the image, adaptive histogram equalization calculates a histogram in a local neighborhood around each pixel to perform the transformation, thereby enhancing the local contrast across the image. However, this can amplify local noise, so to avoid this, the histogram is clipped to a maximum allowed bin value before transforming the pixel values. To speed up the computation, it is implemented here using a sliding window technique as described by Wang and Tao (2006).

    These methods rescale the output image so that its median equals the median of the original image and it has equal interquartile range (IQR).
}

\value{
    A numeric matrix the same dimensions as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\references{
    K. Zuiderveld. ``Contrast Limited Adaptive Histogram Equalization.'' Graphics Gems IV, Academic Press, pp. 474-485, 1994.

    Z. Wang and J. Tao. ``A Fast Implementation of Adaptive Histogram Equalization.'' IEEE 8th international Conference on Signal Processing, Nov 2006.
}

\examples{
set.seed(1)
x <- matrix(0, nrow=32, ncol=32)
x[9:24,9:24] <- 10
x <- x + runif(length(x))
y <- x + rlnorm(length(x))
z <- enhance_hist(y)

par(mfcol=c(1,3))
image(x, col=hcl.colors(256), main="original")
image(y, col=hcl.colors(256), main="multiplicative noise")
image(z, col=hcl.colors(256), main="histogram equalization")
}

\keyword{spatial}
\keyword{smooth}
