\name{approx1}

\alias{approx1}

\title{Resampling in 1D with Interpolation}

\description{
    Resample the given data at specified points. Interpolation can be performed within a tolerance using several interpolation methods.
}

\usage{
approx1(x, y, xout, interp = "linear", n = length(x),
	tol = NA_real_, tol.ref = "abs", extrap = NA_real_)
}

\arguments{
	\item{x, y}{The data to be interpolated.}

	\item{xout}{A vector of values where the resampling should take place.}

	\item{interp}{Interpolation method. One of 'none', 'sum', 'mean', 'max', 'min', 'area', 'linear', 'cubic', 'gaussian', or 'lanczos'.}

	\item{n}{If \code{xout} is not given, then interpolation is performed at \code{n} equally spaced data points along the range of \code{x}.}

	\item{tol}{The tolerance for the data points used for interpolation. Must be nonnegative. If \code{NA}, then the tolerance is estimated from the maximum differences in \code{x}.}

	\item{tol.ref}{If 'abs', then comparison is done by taking the absolute difference. If 'x', then relative differences are used.}

	\item{extrap}{The value to be returned when performing extrapolation, i.e., in the case when there is no data within \code{tol}.}
}

\details{
    The algorithm is implemented in C and provides several fast interpolation methods. Note that interpolation is limited to using data within the given tolerance. This is also used to specify the width for kernel-based interpolation methods such as \code{interp = "gaussian"}. The use of a tolerance also means that interpolating within the range of the data but where no data points are within the tolerance window is considered extrapolation. This can be useful when resampling sparse signals with large empty regions, by setting \code{extrap = 0}, and setting an appropriate tolerance.
}

\value{
    A vector of the same length as \code{xout}, giving the resampled data.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{asearch}},
	\code{\link{approx}}
	\code{\link{approx2}}
}

\examples{
x <- c(1.11, 2.22, 3.33, 5.0, 5.1)
y <- x^1.11

approx1(x, y, 2.22) # 2.42359
approx1(x, y, 3.0) # NA
approx1(x, y, 3.0, tol=0.2, tol.ref="x") # 3.801133
}

\keyword{ts}
