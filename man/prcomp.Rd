\name{prcomp}
\docType{methods}

\alias{prcomp}
\alias{prcomp_lanczos}
\alias{prcomp,matter_mat-method}
\alias{prcomp,sparse_mat-method}


\title{Principal Components Analysis for ``matter'' Matrices}

\description{
    This method allows computation of a truncated principal components analysis of \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices using the implicitly restarted Lanczos method from the ``irlba'' package.
}

\usage{
\S4method{prcomp}{matter_mat}(x, k = 3L, retx = TRUE, center = TRUE, scale. = FALSE, \dots)

\S4method{prcomp}{sparse_mat}(x, k = 3L, retx = TRUE, center = TRUE, scale. = FALSE, \dots)

prcomp_lanczos(x, k = 3L, retx = TRUE,
    center = TRUE, scale. = FALSE, transpose = FALSE, \dots)
}

\arguments{
    \item{x}{A \code{\linkS4class{matter}} matrix, or any matrix-like object for \code{prcomp_lanczos}.}

    \item{k}{The number of principal components to return, must be less than \code{min(dim(x))}.}

    \item{retx}{A logical value indicating whether the rotated variables should be returned.}

    \item{center}{A logical value indicating whether the variables should be shifted to be zero-centered, or a centering vector of length equal to the number of columns of \code{x}. The centering is performed implicitly and does not change the out-of-memory data in \code{x}.}

    \item{scale.}{A logical value indicating whether the variables should be scaled to have unit variance, or a scaling vector of length equal to the number of columns of \code{x}. The scaling is performed implicitly and does not change the out-of-memory data in \code{x}.}

    \item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This can be useful if the input matrix is (P x N) instead of (N x P) and calculating the transpose is expensive. This is not necessary for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} objects, but can be useful for large in-memory (P x N)  matrices.}

    \item{\dots}{Additional options passed to \code{\link[irlba]{irlba}} or \code{prcomp_lanczos}.}
}

\value{
	An object of class `prcomp'. See \code{?\link[stats]{prcomp}} for details.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[irlba]{irlba}}
    \code{\link[irlba]{prcomp_irlba}}
}

\examples{
set.seed(1)

x <- matter_mat(rnorm(1000), nrow=100, ncol=10)

prcomp(x)
}

\keyword{multivariate}
