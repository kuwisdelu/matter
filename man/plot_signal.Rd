\name{plot_signal}

\alias{plot_signal}
\alias{plot_image}

\title{Plot a Signal or Image}

\description{
    Plot a list of superposed or faceted signals or images (in 2D or 3D).
}

\usage{
plot_signal(x, y, by = names(y), group = NULL,
    xlim = NULL, ylim = NULL, col = NULL, byrow = FALSE,
    xlab = NULL, ylab = NULL, layout = NULL, free = "",
    n = Inf, downsampler = "lttb", key = TRUE, grid = TRUE,
    isPeaks = FALSE, annPeaks = 0, engine = NULL, \dots)

plot_image(x, y, z, vals, by = names(vals), group = NULL,
    zlim = NULL, xlim = NULL, ylim = NULL, col = NULL, byrow = FALSE,
    zlab = NULL, xlab = NULL, ylab = NULL, layout = NULL, free = "",
    enhance = NULL, smooth = NULL, scale = NULL, key = TRUE,
    rasterImages = NULL, rasterParams = NULL, useRaster = TRUE,
    grid = TRUE, asp = 1, engine = NULL, \dots)
}

\arguments{
    \item{x, y, z, vals}{Lists of vectors to plot such that \code{x[[i]]} and \code{y[[i]]} indicate the plotting coordinates for the \emph{i}th signal or image. Attempts are made to flexibly coerce these into the expected format. If only \code{x} is provided, it is interpreted as the signal or image, and the indices or coordinates are inferred from the length or dimensions, respectively. For 2D images, only one of \code{z} or \code{vals} should be provided.}

    \item{by}{A vector of labels indicating facets (i.e., which values should be plotted as separate sub-plots).}

    \item{group}{A vector of labels indicating groups (i.e., which values should be indicated by color as belonging to the same group).}

    \item{xlim, ylim, zlim}{The plot limits. See \code{\link[graphics]{plot.window}}}

    \item{xlab, ylab, zlab}{Plotting labels.}

    \item{col}{A vector giving the color map for encoding the image, or a function that returns a vector of \code{n} colors.}

    \item{byrow}{If \code{vals} is a matrix, should its rows or columns be plotted?}

    \item{layout}{A vector of the form \code{c(nrow, ncol)} specifying the number of rows and columns in the facet grid.}

    \item{free}{A string specifying the free spatial dimensions during faceting. E.g., \code{""}, \code{"x"}, \code{"y"}, or \code{"xy"}.}

    \item{n, downsampler}{See \code{\link{downsample}} for details.}

    \item{key}{Should a color key be generated for the image?}

    \item{grid}{Should a rectangular grid be included in the plot?}

    \item{isPeaks}{Whether the signal should be plotted as peaks or as a continuous signal.}

    \item{annPeaks}{If \code{isPeaks} is \code{TRUE}, either an integer giving the number of peaks to annotate (i.e., label with their \code{x}-value), or a plotting symbol (e.g., "circle", "cross", etc.) to indicate the peak locations.}

    \item{engine}{The plotting engine. Default is to use base graphics. Using "plotly" requires the \code{plotly} package to be installed.}

    \item{\dots}{Additional graphical parameters (as in \code{\link[graphics]{par}}) or arguments to the \code{\link{vizi}} plotting method.}

    \item{enhance}{The name of a contrast enhancement method, such as \code{"hist"} or \code{"adapt"} for \code{enhance_hist()} and \code{enhance_adapt()}, etc. See \code{\link{enhance}} for details.}

    \item{smooth}{The name of a smoothing method, such as \code{"gauss"} or \code{"bi"} for \code{filt2_gauss()} and \code{filt2_bi()}, etc. See \code{\link{filt2}} for details.}

    \item{scale}{If \code{TRUE}, then all image values will be scaled to the range [0, 100]. This is useful for comparing images with differing intensity levels across facets or layers.}

    \item{asp}{The aspect ratio. See \code{\link[graphics]{plot.window}}.}

    \item{rasterImages, rasterParams}{A list of rasters and raster parameters (e.g., \code{xmin}, \code{xmax}, etc.) to plot before plotting \code{vals}. These should be numeric arrays of 3 or 4 color channels with values from 0 to 1. If the raster parameters are omitted, then the raster limits are taken from the range of \code{x} and \code{y}. If the raster list has names, then these are matched against the levels of \code{by} and plotted accordingly.}

    \item{useRaster}{Should a bitmap raster be used for plotting? For 2D images, this is typically faster on supported devices. A fallback to polygon-based plotting is used if raster plotting is not supported. For 3D images, \code{TRUE} means to plot raster surfaces, and \code{FALSE} means to plot individual voxels as points.}
}

\value{
	An object of class \code{vizi_plot}.
}

\seealso{
    \code{\link{vizi}},
    \code{\link{vizi_pixels}}
}

\author{Kylie A. Bemis}

\examples{
require(datasets)

# plot signals
set.seed(1)
s <- simspec(6)
plot_signal(domain(s), s, group=colnames(s))

# volcano image
pos <- expand.grid(x=1:nrow(volcano), y=1:ncol(volcano))
plot_image(pos$x, pos$y, volcano, col=cpal("plasma"))

# plot original and transformed images
volcano2 <- trans2d(volcano, rotate=15, translate=c(-5, 5))
plot_image(list(original=volcano, transformed=volcano2))
}

\keyword{hplot}
