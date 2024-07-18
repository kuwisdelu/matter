\name{simspec}

\alias{simspec}
\alias{simspec1}

\title{Simulate Spectra}

\description{
    Simulate spectra from noise and a list of peaks.
}

\usage{
simspec(n = 1L, npeaks = 50L,
	x = rlnorm(npeaks, 7, 0.3), y = rlnorm(npeaks, 1, 0.9),
	domain = c(0.9 * min(x), 1.1 * max(x)), size = 10000,
	sdx = 1e-5, sdy = sdymult * log1p(y), sdymult = 0.2,
	sdnoise = 0.1, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units = "relative")

simspec1(x, y, xout, peakwidths = NA_real_,
	sdnoise = 0, resolution = 1000, fmax = 0.5)
}

\arguments{
	\item{n}{The number of spectra to simulate.}

    \item{npeaks}{The number of peaks to simulate. Not used if \code{x} and \code{y} are provided.}

    \item{x, y}{The locations and values of the spectral peaks.}

    \item{xout, domain}{The output domain variable or its range.}

    \item{size}{The number of samples in each spectrum.}

    \item{sdx}{The standard deviation of the error in the observed locations of peaks, in units indicated by \code{units}.}

    \item{sdy}{The standard deviation(s) for the distributions of observed peak values on the log scale.}

    \item{sdymult}{A multiplier used to calculate \code{sdy} based on the mean values of the peaks; used to simulate multiplicative variance. Not used if \code{sdy} is provided.}

    \item{sdnoise}{The standard deviation of the random noise in the spectra on the log scale.}

    \item{resolution}{The resolution as defined by \code{x / dx}, where \code{x} is the observed peak location and \code{dx} is the width of the peak at a proportion of its maximum height defined by \code{fmax} (defaults to full-width-at-half-maximum -- FWHM -- definition).}

    \item{fmax}{The fraction of the maximum peak height to use when defining the resolution.}

    \item{peakwidths}{The peak widths at \code{fmax}. Typically, these are calculated automatically from \code{resolution}.}

    \item{baseline}{The maximum intensity of the baseline. Note that \code{baseline=0} means there is no baseline.}

    \item{decay}{A constant used to calculate the exponential decay of the baseline. Larger values mean the baseline decays more sharply.}

    \item{units}{The units for \code{sdx}. Either \code{"absolute"} or \code{"relative"}.}
}

\value{
    Either a numeric vector of the same length as \code{size}, giving the simulated spectrum, or a \code{size} x \code{n} matrix of simulated spectra.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
y <- simspec(2)
x <- attr(y, "domain")

plot(x, y[,1], type="l", ylim=c(-max(y), max(y)))
lines(x, -y[,2], col="red")
}

\keyword{ts}
\keyword{datagen}
