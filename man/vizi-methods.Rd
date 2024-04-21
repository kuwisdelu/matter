\name{plot-vizi}

\alias{vizi_points}
\alias{vizi_lines}
\alias{vizi_peaks}
\alias{vizi_text}
\alias{vizi_intervals}
\alias{vizi_boxplot}
\alias{vizi_pixels}
\alias{vizi_voxels}

\alias{plot.vizi_points}
\alias{plot.vizi_lines}
\alias{plot.vizi_peaks}
\alias{plot.vizi_text}
\alias{plot.vizi_intervals}
\alias{plot.vizi_boxplot}
\alias{plot.vizi_pixels}
\alias{plot.vizi_voxels}

\alias{plot,vizi_points,ANY-method}
\alias{plot,vizi_lines,ANY-method}
\alias{plot,vizi_peaks,ANY-method}
\alias{plot,vizi_text,ANY-method}
\alias{plot,vizi_intervals,ANY-method}
\alias{plot,vizi_boxplot,ANY-method}
\alias{plot,vizi_pixels,ANY-method}
\alias{plot,vizi_voxels,ANY-method}

\title{Plotting Graphical Marks}

\description{
    These functions provide plotting methods for various graphical marks. They are not intended to be called directly.
}

\usage{
\S3method{plot}{vizi_points}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb", jitter = "")

\S3method{plot}{vizi_lines}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb", jitter = "")

\S3method{plot}{vizi_peaks}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb", jitter = "")

\S3method{plot}{vizi_text}(x, plot = NULL, add = FALSE, \dots,
    adj = NULL, pos = NULL, offset = 0.5)

\S3method{plot}{vizi_intervals}(x, plot = NULL, add = FALSE, \dots,
    length = 0.25, angle = 90)

\S3method{plot}{vizi_boxplot}(x, plot = NULL, add = FALSE, \dots,
    range = 1.5, notch = FALSE, pars = NULL)

\S3method{plot}{vizi_pixels}(x, plot = NULL, add = FALSE, \dots,
    enhance = FALSE, smooth = FALSE, scale = FALSE,
    useRaster = TRUE)

\S3method{plot}{vizi_voxels}(x, plot = NULL, add = FALSE, \dots,
    xslice = NULL, yslice = NULL, zslice = NULL)
}

\arguments{
    \item{x}{A graphical mark.}

    \item{plot}{A \code{vizi_plot} object.}

    \item{add}{Should the plot be added to the current panel or start a new panel?}

    \item{\dots}{Additional graphical parameters passed to the underlying base graphics plotting function.}

    \item{n}{\emph{Transformation}. Maximum number of points to plot. This is useful for downsampling series with far more data points than are useful to plot. See \code{\link{downsample}} for details.}

    \item{downsampler}{\emph{Transformation}. If \code{n} is less than the number of points, then this is the downsampling method to use. See \code{\link{downsample}} for details.}

    \item{jitter}{\emph{Transformation}. Should jitter be applied to one or more position channels? One of \code{""}, \code{"x"}, \code{"y"}, or \code{"xy"}.}

    \item{adj, pos, offset}{See \code{\link[graphics]{text}}.}

    \item{length, angle}{See \code{\link[graphics]{arrows}}.}

    \item{range, notch, pars}{See \code{\link[graphics]{boxplot}}.}

    \item{enhance}{\emph{Transformation}. The name of a contrast enhancement method, such as \code{"hist"} or \code{"adapt"} for \code{enhance_hist()} and \code{enhance_adapt()}, etc. See \code{\link{enhance}} for details.}

    \item{smooth}{\emph{Transformation}. The name of a smoothing method, such as \code{"gauss"} or \code{"bi"} for \code{filt2_gauss()} and \code{filt2_bi()}, etc. See \code{\link{filt2}} for details.}

    \item{scale}{\emph{Transformation}. If \code{TRUE}, then all image values will be scaled to the range [0, 100]. This is useful for comparing images with differing intensity levels across facets or layers.}

    \item{useRaster}{Should a bitmap raster be used for plotting? This is typically faster on supported devices. A fallback to polygon-based plotting is used if raster plotting is not supported.}

    \item{xslice, yslice, zslice}{Numeric vectors giving the x, y, and/or z coordinates of the volumetric slices to plot. If none are provided, defaults to plotting all z-slices.}
}

\seealso{
    \code{\link{vizi}},
    \code{\link{add_mark}}
}

\author{Kylie A. Bemis}

\keyword{aplot}
\keyword{hplot}
