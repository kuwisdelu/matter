\name{warp2}

\alias{warp2}

\alias{warp2_trans}
\alias{mi}

\title{Warping to Align 2D Signals}

\description{
    Register two images by warping a "moving" image to align with the "fixed" image.
}

\usage{
# Transformation-based registration
warp2_trans(x, y, control = list(),
    trans = c("rigid", "similarity", "affine"),
    metric = c("cor", "mse", "mi"), nbins = 64L,
    scale = TRUE, dimout = dim(y))

# Mutual information
mi(x, y, n = 64L)
}

\arguments{
	\item{x, y}{Images to be aligned by warping \code{x} (which is "moving") to match \code{y} (which is "fixed").}

    \item{control}{A list of optimization control parameters. Passed to \code{optim()}.}

    \item{trans}{The type of transformation allowed: "rigid" means translation and rotation, "similarity" means translation, rotation, and scaling, and "affine" means a general affine transformation.}

    \item{metric}{The metric to optimize.}

    \item{nbins, n}{The number of histogram bins to use when calculating mutual information.}

    \item{scale}{Should the images be normalized to have the same intensity range?}

    \item{dimout}{The dimensions of the returned matrix.}
}

\details{
    \code{warp2_trans()} performs a simple transformation-based registration using \code{\link{optim}} for optimization.
}

\value{
    A numeric vector the same length as \code{y} with the warped \code{x}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{trans2d}},
    \code{\link{optim}}
}

\examples{
set.seed(1)
x <- matrix(0, nrow=32, ncol=32)
x[9:24,9:24] <- 10
x <- x + runif(length(x))
xt <- trans2d(x, rotate=15, translate=c(-5, 5))
xw <- warp2_trans(xt, x)

par(mfcol=c(1,3))
image(x, col=hcl.colors(256), main="original")
image(xt, col=hcl.colors(256), main="transformed")
image(xw, col=hcl.colors(256), main="registered")
}

\keyword{spatial}
