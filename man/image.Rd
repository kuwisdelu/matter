\name{image}

\alias{image}
\alias{image,formula-method}

\title{Display a Color Image from a Formula}

\description{
    Plot a false-color image using a formula interface.
}

\usage{
\S4method{image}{formula}(x, data = NULL, zlim, xlim, ylim, col = NULL,
        add = FALSE, key = FALSE, zlab, xlab, ylab, \dots)
}

\arguments{
    \item{x}{A \code{formula} specifying the image to be plotted. The LHS should be the color variable, and the RHS should include the x and y variables.}

    \item{data}{A \code{data.frame}.}

    \item{xlim, ylim, zlim}{The plot limits. See \code{\link[graphics]{plot.window}}}

    \item{col}{A vector giving the color map for encoding the image, or a function that returns a vector of \code{n} colors.}

    \item{add}{Should a new plot be created or should the image be added to an existing plot?}

    \item{key}{Should a color key be generated for the image?}

    \item{xlab, ylab, zlab}{Plotting labels.}

    \item{\dots}{Additional graphical parameters (as in \code{\link[graphics]{par}}) or arguments to the \code{\link{vizi}} plotting method.}
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

coords <- expand.grid(x=1:nrow(volcano), y=1:ncol(volcano))

# volcano image
image(volcano ~ x + y, data=coords, zlab="elevation", key=TRUE)

volcano2 <- trans2d(volcano, rotate=15, translate=c(-5, 5))
df <- data.frame(
    x=c(coords$x, coords$x),
    y=c(coords$y, coords$y),
    z=c(as.vector(volcano), as.vector(volcano2)),
    g=rep(c("original", "transformed"), each=nrow(coords)))

# plot original and transformed images
image(z ~ x + y | g, data=df, col=cpal("plasma"))
}

\keyword{hplot}
