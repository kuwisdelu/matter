\name{warp1}

\alias{warp1}

\alias{warp1_loc}
\alias{warp1_dtw}
\alias{warp1_cow}
\alias{icor}

\title{Warping to Align 1D Signals}

\description{
    Two signals often need to be aligned in order to compare them. The signals may contain a similar pattern, but shifted or stretched by some amount. These functions warp the first signal to align it as closely as possible to the second signal.
}

\usage{
# Signal warping based on local events
warp1_loc(x, y, tx = seq_along(x), ty = seq_along(y),
    events = c("maxmin", "max", "min"), lx = NULL, ly = NULL,
    interp = c("linear", "loess", "spline"), n = length(y),
    tol = NA_real_, tol.ref = "abs")

# Dynamic time warping
warp1_dtw(x, y, tx = seq_along(x), ty = seq_along(y),
    n = length(y), tol = NA_real_, tol.ref = "abs")

# Correlation optimized warping
warp1_cow(x, y, tx = seq_along(x), ty = seq_along(y),
    nbins = NA_integer_, n = length(y),
    tol = NA_real_, tol.ref = "abs")
}

\arguments{
	\item{x, y}{Signals to be aligned by warping \code{x} to match \code{y}.}

    \item{tx, ty}{The domain variable of the signals.}

    \item{events}{The type of events to use for calculating the alignment.}

    \item{lx, ly}{The domain locations of the events, if already known.}

    \item{n}{The number of samples in the warped \code{x} to be returned. By default, it matches the length of \code{y}.}

    \item{interp}{The interpolation method used when warping the signal.}

    \item{tol, tol.ref}{A tolerance specifying the maximum allowed distance between aligned samples. See \code{\link{bsearch}} for details. If missing, the tolerance is estimated as 5\% of the signal's domain range.}

    \item{nbins}{The number of signal segments used for warping. The correlation is maximized for each segment.}
}

\details{
    \code{warp1_loc()} uses a simple event-based alignment. Events are defined as local extrema. The events are matched between each signal based on proximity. The shift between the events in \code{x} and \code{y} are calculated and interpolated to find shifts for each sample. The warped signal is then calculated from these shifts. This is a simple heuristic method, but it is relatively fast and typically good enough for aligning peak locations.

    \code{warp1_dtw()} performs dynamic time warping. In dynamic time warping, each sample in \code{x} is matched to a corresponding sample in \code{y} using dynamic programming to find the optimal matches. The version implemented here is constrained by the given tolerance. This both reduces the necessary memory, and in practice tends to give more realistic (and therefore accurate) results than an unconstrained alignment. An unconstrained alignment can still be obtained by setting a high tolerance, but this may use a lot of memory.

    \code{warp1_cow()} performs correlation optimized. In correlation optimized warping, each signal is divided into some number of segments. Dynamic programming is then used to find the placement of the segment boundaries that maximizes the correlation of all the segments.
}

\value{
    A numeric vector the same length as \code{y} with the warped \code{x}.
}

\author{Kylie A. Bemis}

\references{
    G. Tomasi, F. van den Berg, and C. Andersson. ``Correlation optimized warping and dynamic time warping as preprocessing methods for chromatographic data.'' Journal of Chemometrics, vol. 18, pp. 231-241, July 2004.

    N. V. Nielsen, J. M. Carstensen, and J. Smedsgaard. ``Aligning of single and multiple wavelength chromatographic profiles for chemometric data analysis using correlation optimised warping.'' Journal of Chromatography A, vol. 805, pp. 17-35, Jan. 1998.
}

\examples{
set.seed(1)
t <- seq(from=0, to=6 * pi, length.out=2000)
dt <- 0.3 * (sin(t) + 0.6 * sin(2.6 * t))
x <- sin(t + dt) + 0.6 * sin(2.6 * (t + dt))
y <- sin(t) + 0.6 * sin(2.6 * t)
xw <- warp1_dtw(x, y)
if ( interactive() ) {
    plot(y, type="l")
    lines(x, col="blue")
    lines(xw, col="red", lty=2)
}
}

\keyword{spatial}
