\name{approx2}

\alias{approx2}

\title{Resampling in 2D with Interpolation}

\description{
    Resample the (potentially scattered) 2D data to a rectilinear grid at the specified coordinates. Interpolation can be performed within a tolerance using several interpolation methods.
}

\usage{
approx2(x, y, z, xout, yout,
	interp = "linear", nx = length(x), ny = length(y),
	tol = NA_real_, tol.ref = "abs", extrap = NA_real_)
}

\arguments{
	\item{x, y, z}{The data to be interpolated. Alternatively, \code{x} can be a matrix, in which case the matrix elements are used for \code{z} and \code{x} and \code{y} are generated from the matrix's dimensions.}

	\item{xout, yout}{The coordinate (grid lines) where the resampling should take place. These are expanded into a rectilinear grid using \code{expand.grid()}.}

	\item{interp}{Interpolation method. One of 'none', 'sum', 'mean', 'max', 'min', 'area', 'linear', 'cubic', 'gaussian', or 'lanczos'.}

	\item{nx, ny}{If \code{xout} is not given, then interpolation is performed at \code{nx * ny} equally spaced data points along the range of \code{x} and \code{y}.}

	\item{tol}{The tolerance for the data points used for interpolation. Must be nonnegative. May be length-2 to have different tolerances for \code{x} and \code{y}. If \code{NA}, then the tolerance is estimated from the maximum differences in \code{x} and \code{y}.}

	\item{tol.ref}{If 'abs', then comparison is done by taking the absolute difference. If 'x', then relative differences are used.}

	\item{extrap}{The value to be returned when performing extrapolation, i.e., in the case when there is no data within \code{tol}.}
}

\details{
    See \code{\link{approx1}} for details of the 1D implementation. The 2D implementation is mostly the same, except it uses a kd-tree to quickly find neighboring points.

    Note that \code{interp = "linear"} and \code{interp = "cubic"} use a kernel-based approximation. Traditionally, bilinear and bicubic interpolation use 4 and 16 neighboring points, respectively. However, to support scattered data, \code{approx2} will use as many points as are found within the given tolerance, and scale the kernels accordingly. If the input data falls on a regular grid already, then the tolerance should be specified accordingly. Set \code{tol} equal to the sampling rate for \code{interp = "linear"} and twice the sampling rate for \code{interp = "cubic"}.
}

\value{
    A vector of the same length as \code{xout}, giving the resampled data.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{expand.grid}},
	\code{\link{asearch}},
	\code{\link{approx}},
	\code{\link{approx1}}
}

\examples{
x <- matrix(1:25, nrow=5, ncol=5)

approx2(x, nx=10, ny=10, interp="cubic") # upsampling
}

\keyword{ts}
