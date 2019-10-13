\name{colStats}
\docType{methods}

\alias{colStats}
\alias{rowStats}

\alias{colStats,ANY-method}
\alias{colStats,matter_matr-method}
\alias{colStats,sparse_matr-method}
\alias{colStats,virtual_matr-method}

\alias{rowStats,ANY-method}
\alias{rowStats,matter_matc-method}
\alias{rowStats,sparse_matc-method}
\alias{rowStats,virtual_matc-method}

\title{Row and Column Summary Statistics}

\description{
    These functions perform calculation of summary statistics over matrix rows and columns, for each level of a grouping variable (optionally), and with implicit row/column scaling and centering if desired.
}

\usage{
\S4method{colStats}{ANY}(x, stat, groups,
    na.rm = FALSE, tform = identity,
    col.center = NULL, col.scale = NULL,
    row.center = NULL, row.scale = NULL,
    drop = TRUE, BPPARAM = bpparam(), \dots)

\S4method{rowStats}{ANY}(x, stat, groups,
    na.rm = FALSE, tform = identity,
    col.center = NULL, col.scale = NULL,
    row.center = NULL, row.scale = NULL,
    drop = TRUE, BPPARAM = bpparam(), \dots)
}

\arguments{
    \item{x}{A matrix on which to calculate summary statistics.}

    \item{stat}{The name of summary statistics to compute over the rows or columns of a matrix. Allowable values include: "min", "max", "prod", "sum", "mean", "var", "sd", "any", and "all".}

    \item{groups}{A factor or vector giving the grouping. If not provided, no grouping will be used.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}

    \item{tform}{A dimensionality-preserving transformation to be applied to the matrix (e.g., \code{log()} or \code{sqrt()}).}

    \item{col.center}{A vector of column centers to substract from each row. (Or a matrix with a column for each level of \code{groups}.)}

    \item{col.scale}{A vector of column scaling factors to divide from each row. (Or a matrix with a column for each level of \code{groups}.)}

    \item{row.center}{A vector of row centers to substract from each column. (Or a matrix with a column for each level of \code{groups}.)}

    \item{row.scale}{A vector of row centers to scaling factors to divide from each column. (Or a matrix with a column for each level of \code{groups}.)}

    \item{drop}{If only a single summary statistic is calculated, return the results as a vector (or matrix) rather than a list.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments.}
}

\details{
    The summary statistics methods are calculated over chunks of the matrix using \code{\link{colstreamStats}} and \code{\link{rowstreamStats}}. For \code{matter} objects, the iteration is performed over the major dimension for IO efficiency.
}

\value{
    A list for each \code{stat} requested, where each element is either a vector (if no grouping variable is provided) or a matrix where each column corresponds to a different level of \code{groups}.

    If \code{drop=TRUE}, and only a single statistic is requested, then the result will be unlisted and returned as a vector or matrix.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{colSums}}
}

\examples{
register(SerialParam())

set.seed(1)

x <- matrix(runif(100^2), nrow=100, ncol=100)

groups <- as.factor(rep(letters[1:5], each=20))

colStats(x, "mean", groups=groups)
}

\keyword{methods}
\keyword{univar}
