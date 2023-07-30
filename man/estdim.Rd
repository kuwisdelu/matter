\name{estdim}

\alias{estdim}

\title{Estimate Raster Dimensions}

\description{
    Estimate the raster dimensions of a scattered 2D signal based on its pixel coordinates.
}

\usage{
estdim(x, tol = 1e-6)
}

\arguments{
	\item{x}{A numeric matrix or data frame where each column gives the pixel coordinates for a different dimension. Only 2 or 3 dimensions are supported if the coordinates are irregular. Otherwise, any number of dimensions are supported.}

    \item{tol}{The tolerance allowed when  estimating the resolution (i.e., pixel sizes) using \code{estres()}. If estimating the resolution this way fails, then it is estimated from the coordinate ranges instead.}
}

\value{
    A numeric vector giving the estimated raster dimensions.
}

\author{Kylie A. Bemis}

\examples{
co <- expand.grid(x=1:12, y=1:9)
co$x <- jitter(co$x)
co$y <- jitter(co$y)

estdim(co)
}

\keyword{dplot}
