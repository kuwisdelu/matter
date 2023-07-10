\name{enhance}

\alias{enhance}

\alias{enhance_heq}
\alias{enhance_aheq}

\title{Contrast Enhancement}

\description{
    Enhance the contrast in a 2D signal.
}

\usage{
# Histogram equalization
enhance_heq(x, nbins = 256L)

# Adaptive histogram equalization
enhance_aheq(x, width = sqrt(length(x)) \%/\% 5L, nbins = 256L)
}

\arguments{
	\item{x}{A numeric matrix.}

    \item{nbins}{The number of gray levels in the output image.}

    \item{width}{The width of the sliding window when calculating the histograms.}
}

\details{
    \code{enhance_heq()} performs histogram equalization. Histogram equalization transforms the pixel values so that the histogram of the image is approximately flat. This is done by replacing the original pixel values with their associated probability in the image's empirical cumulative distribution.

    \code{enhance_aheq()} performs adaptive histogram equalization. While ordinary histogram equalization performs a global transformation on the image, adaptive histogram equalization calculates a histogram in a local neighborhood around each pixel to perform the transformation, thereby enhancing the local contrast across the image. To speed up the computation, it is implemented here using a sliding window technique similar to Wang and Tao (2006).

    These methods rescale the output image so that its median equals the median of the original image and it has the same interquartile range (IQR).
}

\value{
    A numeric matrix the same dimensions as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\references{
    Z. Wang and J. Tao. ``A Fast Implementation of Adaptive Histogram Equalization.'' IEEE 8th international Conference on Signal Processing, Nov 2006.
}

\examples{
set.seed(1)
x <- matrix(0, nrow=32, ncol=32)
x[9:24,9:24] <- 10
x <- x + runif(length(x))
y <- x + rlnorm(length(x))
z <- enhance_heq(y)
if ( interactive() ) {
    par(mfcol=c(1,3))
    image(x, col=hcl.colors(256), main="original")
    image(y, col=hcl.colors(256), main="multiplicative noise")
    image(z, col=hcl.colors(256), main="histogram equalization")
}
}

\keyword{utilities}
