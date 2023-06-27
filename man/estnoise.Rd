\name{estnoise}

\alias{estnoise}

\alias{estnoise_sd}
\alias{estnoise_mad}
\alias{estnoise_diff}
\alias{estnoise_loess}
\alias{estnoise_filt}

\title{Local Noise Estimation}

\description{
    Estimate the local noise in a signal.
}

\usage{
# SD-based noise estimation
estnoise_sd(x, nbins = 1L, dynamic = TRUE)

# MAD-based noise estimation
estnoise_mad(x, nbins = 1L, dynamic = TRUE)

# Derivative-based noise estimation
estnoise_diff(x, nbins = 1L, dynamic = TRUE)

# Noise estimated from loess smoothing
estnoise_loess(x, span = 2/3)

# Dynamic noise level filtering
estnoise_filt(x, snr = 2, nbins = 1L,
    threshold = 0.5, peaks = FALSE)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{nbins}{The number of bins to divide the signal into before estimating the noise. The noise is estimated locally in each bin.}

    \item{dynamic}{Should the bins be equally spaced (\code{FALSE}) or dynamically spaced (\code{TRUE}) based on the local signal?}

    \item{span}{Smoothing parameter for loess.}

    \item{snr}{The minimum signal-to-noise ratio for distinguishing signal peaks from noise peaks.}

    \item{threshold}{The required signal-to-noise difference for the first non-noise peak.}

    \item{peaks}{Does \code{x} represent a signal profile (\code{FALSE}) or peaks (\code{TRUE})?}
}

\details{
    \code{estnoise_sd()} and \code{estnoise_mad()} estimate the local noise from the standard deviation (SD) or median absolute deviation (MAD), respectively. The local noise is estimated in each bin, and then interpolated across the entire signal using local regression.

    Similarly, \code{estnoise_diff()} estimates the local noise from the mean absolute deviation of the derivative of the signal in each bin.

    \code{estnoise_loess()} simply uses loess smoothing to estimate the noise across the entire signal.

    \code{estnoise_filt()} uses the dynamic noise level filtering algorithm of Xu and Freitas (2010) based on the local signal in an approach similar to Gallia et al. (2013). The peaks in the signal are sorted, and the smallest peak is assumed to be noise and is used to estimate the noise level. Each peak is then compared to the previous peak. A peak is labeled a signal peak only if it exceeds a minimum signal-to-noise ratio. Otherwise, the peak is labeled noise, and the noise level is re-estimated. This process continues until a signal peak is found, and the noise level is estimated from the noise peaks.
}

\value{
    A numeric vector the same length as \code{x} with the estimated local noise level.
}

\author{Kylie A. Bemis}

\references{
    H. Xu and M. A. Freitas. ``A Dynamic Noise Level Algorithm for Spectral Screening of Peptide MS/MS Spectra.'' BMC Bioinformatics, vol. 11, no. 436, Aug. 2010.

    J. Gallia, K. Lavrich, A. Tan-Wilson, and P. H. Madden. ``Filtering of MS/MS data for peptide identification.'' BMC Genomics, vol. 14, suppl. 7, Nov. 2013.
}

\examples{
set.seed(1)
n <- 500
x <- rnorm(n)
x <- x + 90 * dnorm(seq_along(x), mean=n/4)
x <- x + 80 * dnorm(seq_along(x), mean=n/2)
x <- x + 70 * dnorm(seq_along(x), mean=3*n/4)
ns <- estnoise_mad(x)
if ( interactive() ) {
    plot(x, type="l")
    lines(ns, col="blue")
}
}

\keyword{ts}
