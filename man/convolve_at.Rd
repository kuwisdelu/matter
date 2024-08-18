\name{convolve_at}

\alias{convolve_at}

\title{Convolution at Arbitrary Indices}

\description{
    Convolve a signal with weights at arbitrary indices.
}

\usage{
convolve_at(x, index, weights, margin = NULL, na.rm = FALSE)
}

\arguments{
	\item{x}{A numeric vector or matrix.}

    \item{index}{A list or matrix of numeric vectors giving the indices to convolve. If \code{index} is a list, then it must have the same length as \code{x} and element lengths that match \code{weights}. If \code{x} is a matrix, then its rows must correspond to \code{x} and its columns must correspond to \code{weights}.}

    \item{weights}{A list giving the weights of the kernels to convolve for each element of \code{x}. Lengths must match \code{index}.}

    \item{margin}{If \code{x} is a matrix, then the margin to convolve over.}

    \item{na.rm}{Should missing values (including \code{NaN}) be removed?}
}

\details{
    This is essentially just a weighted sum defined by \code{x[i] = sum(weights[[i]] * x[index[[i]]])}.
}

\value{
    A numeric vector the same length as \code{x} with the smoothed result.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
t <- seq(from=0, to=6 * pi, length.out=5000)
y <- sin(t) + 0.6 * sin(2.6 * t)
x <- y + runif(length(y))

i <- roll(seq_along(x), width=15)
wt <- dnorm((-7):7, sd=7/2)
wt <- wt / sum(wt)

xs <- convolve_at(x, i, wt)

plot(x, type="l")
lines(xs, col="red")
}

\keyword{ts}
\keyword{smooth}
