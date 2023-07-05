\name{trans2d}

\alias{trans2d}

\title{2D Spatial Transformation}

\description{
    Perform linear spatial transformations on a matrix, including rigid, similarity, and affine transformations.
}

\usage{
trans2d(x, y, z, pmat,
	rotate = 0, translate = c(0, 0), scale = c(1, 1),
	interp = "linear", dimout = dim(z), \dots)
}

\arguments{
	\item{x, y, z}{The data to be interpolated. Alternatively, \code{x} can be a matrix, in which case the matrix elements are used for \code{z} and \code{x} and \code{y} are generated from the matrix's dimensions.}

	\item{pmat}{A 3 x 2 transformation matrix for performing an affine transformation. Automatically generated from \code{rotate}, \code{translate}, and \code{scale} if not provided.}

	\item{rotate}{Rotation in degrees.}

	\item{translate}{Translation vector, in the same units as \code{x} and \code{y}, if given.}

	\item{scale}{Scaling factors.}

	\item{interp}{Interpolation method. See \code{\link{approx2}}.}

	\item{dimout}{The dimensions of the returned matrix.}

	\item{\dots}{Additional arguments passed to \code{\link{approx2}}.}
}

\value{
    If \code{x} is a matrix or \code{z} is provided, returns a transformed matrix with the dimensions of \code{dimout}.

    Otherwise, only the transformed coordinates are returned in a \code{data.frame}.
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{approx2}}
}

\examples{
set.seed(1)
x <- matrix(0, nrow=32, ncol=32)
x[9:24,9:24] <- 10
x <- x + runif(length(x))
xt <- trans2d(x, rotate=15, translate=c(-5, 5))
if ( interactive() ) {
    par(mfcol=c(1,2))
    image(x, col=hcl.colors(256), main="original")
    image(xt, col=hcl.colors(256), main="transformed")
}
}

\keyword{utilities}
