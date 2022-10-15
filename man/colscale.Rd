\name{colscale}
\docType{methods}

\alias{scale}
\alias{scale,matter_mat-method}

\alias{colscale}
\alias{colscale,ANY-method}
\alias{rowscale}
\alias{rowscale,ANY-method}

\title{Scaling and Centering by Row or Column Based on Grouping}

\description{
    Apply the equivalent of \code{\link[base]{scale}} to either columns or rows of a matrix, using a grouping variable.
}

\usage{
\S4method{colscale}{ANY}(x, center=TRUE, scale=TRUE,
    group = NULL, \dots, BPPARAM = bpparam())

\S4method{rowscale}{ANY}(x, center=TRUE, scale=TRUE,
    group = NULL, \dots, BPPARAM = bpparam())
}

\arguments{
    \item{x}{A matrix-like object.}
    
    \item{center}{Either a logical value or a numeric vector of length equal to the number of columns of 'x' (for \code{colscale()}) or the number of the rows of 'x' (for \code{rowscale()}). If a grouping variable is given, then this must be a matrix with number of columns equal to the number of groups.}

    \item{scale}{Either a logical value or a numeric vector of length equal to the number of columns of 'x' (for \code{colscale()}) or the number of the rows of 'x' (for \code{rowscale()}). If a grouping variable is given, then this must be a matrix with number of columns equal to the number of groups.}

    \item{group}{A vector or factor giving the groupings with length equal to the number of rows of 'x' (for \code{colscale()}) or the number of the columns of 'x' (for \code{rowscale()}).}

    \item{\dots}{Arguments passed to \code{rowStats()} or \code{colStats()} respectively, if \code{center} or \code{scale} must be calculated.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\details{
	See \code{\link[base]{scale}} for details.
}

\value{
	A matrix-like object (usually of the same class as \code{x}) with either `col-scaled:center' and `col-scaled:scale' attributes or `row-scaled:center' and `row-scaled:scale' attributes.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[base]{scale}}
}

\examples{
x <- matter(1:100, nrow=10, ncol=10)

colscale(x)
}

\keyword{methods}
