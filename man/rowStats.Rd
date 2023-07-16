\name{colStats}
\docType{methods}

\alias{colStats}
\alias{rowStats}

\alias{.colStats}
\alias{.rowStats}

\alias{colStats,ANY-method}
\alias{colStats,matter_mat-method}
\alias{colStats,sparse_mat-method}

\alias{rowStats,ANY-method}
\alias{rowStats,matter_mat-method}
\alias{rowStats,sparse_mat-method}

\title{Row and Column Summary Statistics Based on Grouping}

\description{
    These functions perform calculation of summary statistics over matrix rows and columns for each level of a grouping variable.
}

\usage{
\S4method{rowStats}{ANY}(x, \dots, BPPARAM = bpparam())

\S4method{colStats}{ANY}(x, \dots, BPPARAM = bpparam())

\S4method{rowStats}{matter_mat}(x, \dots, BPPARAM = bpparam())

\S4method{colStats}{matter_mat}(x, \dots, BPPARAM = bpparam())

\S4method{rowStats}{sparse_mat}(x, \dots, BPPARAM = bpparam())

\S4method{colStats}{sparse_mat}(x, \dots, BPPARAM = bpparam())

.rowStats(x, stat, group = NULL,
    na.rm = FALSE, simplify = TRUE, drop = TRUE,
    iter.dim = 1L, BPPARAM = bpparam(), \dots)

.colStats(x, stat, group = NULL,
    na.rm = FALSE, simplify = TRUE, drop = TRUE,
    iter.dim = 2L, BPPARAM = bpparam(), \dots)
}

\arguments{
    \item{x}{A matrix on which to calculate summary statistics.}

    \item{stat}{The name of summary statistics to compute over the rows or columns of a matrix. Allowable values include: "min", "max", "prod", "sum", "mean", "var", "sd", "any", "all", and "nnzero".}

    \item{group}{A factor or vector giving the grouping. If not provided, no grouping will be used.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}

    \item{simplify}{Simplify the results from a list to a vector or array. This also drops any additional attributes (besides names).}

    \item{drop}{If only a single summary statistic is calculated, return the results as a vector (or matrix) rather than a list.}

    \item{iter.dim}{The dimension to iterate over. Must be 1 or 2, where 1 indicates rows and 2 indicates columns.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}

    \item{\dots}{Additional arguments passed to \code{chunk_rowapply()} or \code{chunk_colapply()}, such as the number of chunks.}
}

\details{
    The summary statistics methods are calculated over chunks of the matrix using \code{\link{s_colstats}} and \code{\link{s_rowstats}}. For \code{matter} objects, the iteration is performed over the major dimension for IO efficiency.
}

\value{
    A list for each \code{stat} requested, where each element is either a vector (if no grouping variable is provided) or a matrix where each column corresponds to a different level of \code{group}.

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

g <- as.factor(rep(letters[1:5], each=20))

colStats(x, "mean", group=g)
}

\keyword{array}
\keyword{univar}
