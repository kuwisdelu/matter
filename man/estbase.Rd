\name{estbase}

\alias{estbase}

\alias{estbase_loc}
\alias{estbase_hull}
\alias{estbase_snip}
\alias{estbase_med}

\title{Continuum Estimation}

\description{
    Estimate the continuum (baseline) of a signal.
}

\usage{
# Continuum based on local extrema
estbase_loc(x,
    smooth = c("none", "loess", "spline"),
    span = 1/10, spar = NULL, upper = FALSE)

# Convex hull
estbase_hull(x, upper = FALSE)

# Sensitive nonlinear iterative peak clipping (SNIP)
estbase_snip(x, width = 100L, decreasing = TRUE)

# Running medians
estbase_med(x, width = 100L)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{smooth}{A smoothing method to be applied after linearly interpolating the continuum.}

    \item{span, spar}{Smoothing parameters for loess and spline smoothing, respectively.}

    \item{upper}{Should the upper continuum be estimated instead of the lower continuum?}    

    \item{width}{The width of the smoothing window in number of samples.}

    \item{decreasing}{Use a decreasing clipping window instead of an increasing window.}
}

\details{
    \code{estbase_loc()} uses a simple method based on linearly interpolating from local extrema. It typically performs well enough for most situations. Signals with strong noise or wide peaks may require stronger smoothing after the interpolation step.

    \code{estbase_hull()} estimates the continuum by finding the lower or upper convex hull using the monotonic chain algorithm of A. M. Andrew (1979).

    \code{estbase_snip()} performs sensitive nonlinear iterative peak (SNIP) clipping using the adaptive clipping window from M. Morhac (2009).

    \code{estbase_med()} estimates the continuum from running medians.
}

\value{
    A numeric vector the same length as \code{x} with the estimated continuum.
}

\author{Kylie A. Bemis}

\references{
    A. M. Andrew. ``Another efficient algorithm for convex hulls in two dimensions.'' Information Processing Letters, vol. 9, issue 5, pp. 216-219, Dec. 1979.

    M. Morhac. ``An algorithm for determination of peak regions and baseline elimination in spectroscopic data.'' Nuclear Instruments and Methods in Physics Research A, vol. 600, issue 2, pp. 478-487, Mar. 2009.
}

\examples{
set.seed(1)
t <- seq(from=0, to=6 * pi, length.out=2000)
x <- sin(t) + 0.6 * sin(2.6 * t)
lo <- estbase_hull(x)
hi <- estbase_hull(x, upper=TRUE)

plot(x, type="l")
lines(lo, col="red")
lines(hi, col="blue")
}

\keyword{ts}
