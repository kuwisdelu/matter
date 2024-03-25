\name{to_raster}

\alias{is_gridded}
\alias{to_raster}
\alias{to_raster3}

\title{Rasterize a Scattered 2D or 3D Signal}

\description{
    Estimate the raster dimensions of a scattered 2D or 3D signal based on its pixel coordinates.
}

\usage{
# Rasterize a 2D signal
to_raster(x, y, vals)

# Rasterize a 3D signal
to_raster3(x, y, z, vals)

# Check if coordinates are gridded
is_gridded(x, tol = 0.5)
}

\arguments{
	\item{x, y, z}{The coordinates of the data to be rasterized. For \code{is_gridded()}, a numeric matrix or data frame where each column gives the pixel coordinates for a different dimension.}

    \item{vals}{The data values to be rasterized.}

    \item{tol}{The tolerance allowed when estimating the resolution. Noise in the sampling rate will be allowed up to this amount when determining if the data is approximately gridded or not.}
}

\details{
    This is meant to be a more efficient version of \code{approx2()} when the data is already (approximately) gridded. Otherwise, \code{approx2()} is used.
}

\value{
    A numeric vector giving the estimated raster dimensions.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{approx2}}
}

\examples{
# create an image
set.seed(1)
i <- seq(-4, 4, length.out=12)
j <- seq(1, 3, length.out=9)
co <- expand.grid(x=i, y=j)
z <- matrix(atan(co$x / co$y), nrow=12, ncol=9)
vals <- 10 * (z - min(z)) / diff(range(z))

# scatter coordinates and flatten image
d <- expand.grid(x=jitter(1:12), y=jitter(1:9))
d$vals <- as.vector(z)

# rasterize
to_raster(d$x, d$y, d$vals)
}

\keyword{dplot}
