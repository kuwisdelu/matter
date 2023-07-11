\name{inpoly}

\alias{inpoly}

\title{Point in polygon}

\description{
    Check if a series of x-y points are contained in a closed 2D polygon.
}

\usage{
inpoly(points, poly)
}

\arguments{
	\item{points}{A 2-column numeric matrix with the points to check.}

    \item{poly}{A 2-column numeric matrix with the vertices of the polygon.}
}

\value{
    A logical vector that is \code{TRUE} for points that are fully inside the polygon, a vertex, or on an edge, and \code{FALSE} for points fully outside the polygon.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{kdsearch}}
}

\examples{
poly <- data.frame(
        x=c(3,5,5,3),
        y=c(3,3,5,5))
xy <- data.frame(
    x=c(4,6,4,2,3,5,5,3,4,5,4,4,
        3.5,4.5,4.0,3.5),
    y=c(2,4,6,4,3,3,5,5,3,4,5,3,
        4.0,4.0,4.5,4.0),
    ref=c(
        rep("out", 4),
        rep("vertex", 4),
        rep("edge", 4),
        rep("in", 4)))

xy$test <- inpoly(xy[,1:2], poly)
xy
}

\keyword{spatial}
\keyword{utilities}
