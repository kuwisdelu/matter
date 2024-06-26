\name{filtn}

\alias{filtn}

\alias{filtn_ma}
\alias{filtn_conv}
\alias{filtn_gauss}

\title{Smoothing Filters using K-nearest neighbors}

\description{
    Smooth a nonuniformly sampled signal in K-nearest neighbors.
}

\usage{
# Moving average filter
filtn_ma(x, index, k = 5L, metric = "euclidean", p = 2)

# Linear convolution filter
filtn_conv(x, index, weights, metric = "euclidean", p = 2)

# Gaussian filter
filtn_gauss(x, index, k = 5L, sd = NA_real_,
    metric = "euclidean", p = 2)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{index}{A matrix or list of domain vectors giving the sampling locations of \code{x}.}

    \item{k}{The number of K-nearest neighbors to use in the smoothing (including the sample being smoothed). Must be positive.}

    \item{weights}{A vector of weights for the linear convolution kernel, in order from nearest to farthest neighbors. The first weight applies to the sample being smoothed.}

    \item{sd}{The standard deviation for Gaussian smoothing. If \code{NA}, then this is calculated as half the median distance to the Kth nearest neighbors.}

    \item{metric}{Distance metric to use when finding the nearest neighbors. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

    \item{p}{The power for the Minkowski distance.}
}

\details{
    These functions must first perform a K-nearest neighbors search on the sample locations (\code{index}) before smoothing the signal (\code{x}), but they can be applied to nonuniformly sampled signals in an arbitrary number of dimensions. The nearest neighbor search is performed using a kd-tree. It is efficient for low dimensional signals, but performance will degrade in high dimensions. In general, the complexity of these filters is O(k * n log(n)).
}

\value{
    A numeric vector the same dimensions as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)

# signal intensities
x <- rlnorm(100)

# 3D sampling locations
index <- replicate(3, runif(100))

filtn_ma(x, index, k=5)
}

\keyword{spatial}
\keyword{smooth}
