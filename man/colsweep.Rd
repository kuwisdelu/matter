\name{colsweep}
\docType{methods}

\alias{colsweep}
\alias{colsweep,ANY-method}
\alias{colsweep,matter_mat-method}
\alias{colsweep,sparse_mat-method}
\alias{rowsweep}
\alias{rowsweep,ANY-method}
\alias{rowsweep,matter_mat-method}
\alias{rowsweep,sparse_mat-method}

\title{Sweep out Matrix Summaries Based on Grouping}

\description{
    Apply the equivalent of \code{\link[base]{sweep}} to either columns or rows of a matrix, using a grouping variable.
}

\usage{
\S4method{colsweep}{ANY}(x, STATS, FUN = "-", group = NULL, \dots)

\S4method{colsweep}{matter_mat}(x, STATS, FUN = "-", group = NULL, \dots)

\S4method{colsweep}{sparse_mat}(x, STATS, FUN = "-", group = NULL, \dots)

\S4method{rowsweep}{ANY}(x, STATS, FUN = "-", group = NULL, \dots)

\S4method{rowsweep}{matter_mat}(x, STATS, FUN = "-", group = NULL, \dots)

\S4method{rowsweep}{sparse_mat}(x, STATS, FUN = "-", group = NULL, \dots)
}

\arguments{
    \item{x}{A matrix-like object.}
    
    \item{STATS}{The summary statistic to be swept out, with length equal to the number of columns of 'x' (for \code{colsweep()}) or the number of the rows of 'x' (for \code{rowsweep()}). If a grouping variable is given, then this must be a matrix with number of columns equal to the number of groups.}

    \item{FUN}{The function to be used to carry out the sweep.}

    \item{group}{A vector or factor giving the groupings with length equal to the number of rows of 'x' (for \code{colsweep()}) or the number of the columns of 'x' (for \code{rowsweep()}).}

    \item{\dots}{Ignored.}
}

\details{
	See \code{\link[base]{sweep}} for details.
}

\value{
	A matrix-like object (usually of the same class as \code{x}) with the statistics swept out.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{sweep}}
}

\examples{
set.seed(1)

x <- matrix(1:100, nrow=10, ncol=10)

colsweep(x, colStats(x, "mean"))
}

\keyword{array}
