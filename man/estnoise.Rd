\name{estnoise}

\alias{estnoise}

\alias{estnoise_quant}
\alias{estnoise_diff}
\alias{estnoise_filt}
\alias{estnoise_sd}
\alias{estnoise_mad}

\title{Local Noise Estimation}

\description{
    Estimate the noise across a signal.
}

\usage{
# Quantile-based noise estimation
estnoise_quant(x, width = 25L, prob = 0.95, niter = 3L)

# Derivative-based noise estimation
estnoise_diff(x, nbins = 1L, dynamic = FALSE)

# Dynamic noise level filtering
estnoise_filt(x, snr = 2, nbins = 1L,
    threshold = 0.5, peaks = FALSE)

# SD-based noise estimation
estnoise_sd(x, width = 25L, wavelet = ricker)

# MAD-based noise estimation
estnoise_mad(x, width = 25L, wavelet = ricker)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{width}{The width of the rolling estimation of quantile, standard deviation, or median absolute deviation.}

    \item{prob}{The quantile used to estimate the noise.}

    \item{niter}{The number of iterations of nonlinear diffusion smoothing to be applied to the signal.}

    \item{nbins}{The number of bins to divide the signal into before estimating the noise. The noise is estimated locally in each bin.}

    \item{dynamic}{Should the bins be equally spaced (\code{FALSE}) or dynamically spaced (\code{TRUE}) based on the local signal?}

    \item{snr}{The minimum signal-to-noise ratio for distinguishing signal peaks from noise peaks.}

    \item{threshold}{The required signal-to-noise difference for the first non-noise peak.}

    \item{peaks}{Does \code{x} represent a signal profile (\code{FALSE}) or peaks (\code{TRUE})?}

    \item{wavelet}{The wavelet to be convolved with the signal to produce to the noise estimate. This should be a function that takes as its first argument the width of the wavelet (in number of points). If this is \code{NULL}, then no convolution is performed, and the raw signal is used.}
}

\details{
    \code{estnoise_quant()} estimates the local noise by first smoothing the signal with a nonlinear diffusion filter, and then subtracting the raw signal from the smoothed signal to isolate the noise component. A rolling quantile of this noise component is used to estimate the local noise in the signal.

    \code{estnoise_diff()} estimates the local noise from the mean absolute deviation of the derivative of the signal in each bin. For noisy signals, the derivative is dominated by the noise, making it a useful estimator of the noise.

    \code{estnoise_filt()} uses the dynamic noise level filtering algorithm of Xu and Freitas (2010) based on the local signal in an approach similar to Gallia et al. (2013). The peaks in the signal are sorted, and the smallest peak is assumed to be noise and is used to estimate the noise level. Each peak is then compared to the previous peak. A peak is labeled a signal peak only if it exceeds a minimum signal-to-noise ratio. Otherwise, the peak is labeled noise, and the noise level is re-estimated. This process continues until a signal peak is found, and the noise level is estimated from the noise peaks.

    \code{estnoise_sd()} and \code{estnoise_mad()} estimate the local noise from the standard deviation (SD) or median absolute deviation (MAD), respectively, after (optionally) convolving the signal with a wavelet.
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
ns <- estnoise_quant(x)
if ( interactive() ) {
    plot(x, type="l")
    lines(ns, col="blue")
}
}

\keyword{ts}
