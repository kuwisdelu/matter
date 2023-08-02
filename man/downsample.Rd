\name{downsample}

\alias{downsample}
\alias{findbins}

\title{Downsample a Signal}

\description{
    Downsamples a signal for the purposes of visualization. A subset of the original samples are used to represent the signal. The downsampled signal is intended to resemble the original when visualized and should not typically be used for downstream processing.
}

\usage{
downsample(x, n = length(x) / 10L, domain = NULL,
	method = c("lttb", "ltob", "dynamic"))
}

\arguments{
	\item{x}{A numeric vector.}

	\item{n}{The length of the downsampled signal.}

	\item{domain}{The domain variable of the signal.}

	\item{method}{The downsampling method to be used. Must be one of \code{"lttb"}, \code{"ltob"}, or \code{"dynamic"}.}
}

\details{
    This function implements the downsampling methods from Sveinn Steinarsson's 2013 MSc thesis \emph{Downsampling Time Series for Visual Representation}, including largest-triangle-three-buckets (LTTB), largest-triangle-one-bucket (LTOB), and dynamic binning.
}

\value{
    A vector of length \code{n}, giving the downsampled signal.
}

\author{Kylie A. Bemis}

\references{
    S. Steinarsson. ``Downsampling Time Series for Visual Representation.'' MSc, School of Engineering and Natural Sciences, University of Iceland, Reykjavik, Iceland, June 2013.
}

\seealso{
	\code{\link{approx1}}
}

\examples{
set.seed(1)
t <- seq(from=0, to=6 * pi, length.out=2000)
x <- sin(t) + 0.6 * sin(2.6 * t)
x <- x + runif(length(x))
xs <- downsample(x, n=200)
s <- attr(xs, "sample")

plot(x, type="l")
points(s, xs, col="red", type="b")
}

\keyword{ts}
