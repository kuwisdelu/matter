\name{nnmf}

\alias{nnmf}
\alias{nnmf_als}
\alias{nnmf_mult}
\alias{predict.nnmf}
\alias{nndsvd}

\title{Nonnegative Matrix Factorization}

\description{
    Nonnegative matrix factorization (NMF) decomposes a nonnegative data matrix into a matrix of basis variables and a matrix of activations (or coefficients). The factorization is approximate and may be less accurate than alternative methods such as PCA, but can greatly improve the interpretability of the reduced dimensions.
}

\usage{
# Alternating least squares
nnmf_als(x, k = 3L, s = 1e-9, transpose = FALSE,
	niter = 100L, tol = 1e-5, verbose = NA, \dots)

# Multiplicative updates
nnmf_mult(x, k = 3L, s = 1e-9, cost = c("euclidean", "KL", "IS"),
	transpose = FALSE, niter = 100L,
	tol = 1e-5, verbose = NA, \dots)

\S3method{predict}{nnmf}(object, newdata, \dots)
}

\arguments{
	\item{x}{A nonnegative matrix.}

	\item{k}{The number of NMF components to extract.}

	\item{s}{A regularization parameter to prevent singularities.}

	\item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This can be useful if the input matrix is (P x N) instead of (N x P) and storing the transpose is expensive. This is not necessary for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} objects, but can be useful for large in-memory (P x N)  matrices.}

	\item{niter}{The maximum number of iterations.}

	\item{tol}{The tolerance for convergence, as measured by the Frobenius norm of the differences between the W and H matrices in successive iterations.}

	\item{verbose}{Should progress be printed for each iteration?}

	\item{cost}{The cost function (i.e., error measure between the reconstructed matrix and original \code{x}) to optimize, where 'euclidean' is the Frobenius norm, 'KL' is the Kullback-Leibler divergence, and 'IS' is the Itakura-Saito divergence. See \strong{Details}.}

	\item{\dots}{Additional options passed to \code{\link[irlba]{irlba}}.}

	\item{object}{An object inheriting from \code{nmf}.}

	\item{newdata}{An optional data matrix to use for the prediction.}
}

\details{
    These functions implement nonnegative matrix factorization (NMF) using either alternating least squares as described by Berry at al. (2007) or multiplicative updates from Lee and Seung (2000) and further described by Burred (2014). The algorithms are initialized using nonnegative double singular value decomposition (NNDSVD) from Boutsidis and Gallopoulos (2008).

    The algorithm using multiplicative updates (\code{nnmf_mult()}) tends to be more stable but converges more slowly. The alternative least squares algorithm (\code{nnmf_als()}) tends to converge faster to more accurate results, but can be less numerically stable than the multiplicative updates algorithm.

    Note for \code{nnmf_mult()} that \code{method = "euclidean"} is the only method that can handle out-of-memory \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices. \code{x} will be coerced to an in-memory matrix for other methods.
}

\value{
    An object of class \code{nnmf}, with the following components:

    \itemize{
        \item{\code{activation}: The (transposed) coefficient matrix (H).}
        \item{\code{x}: The basis variable matrix (W).}
        \item{\code{iter}: The number of iterations performed.}
    }
}

\author{Kylie A. Bemis}

\references{
	M. W. Berry, M. Browne, A. N. Langville, V. P. Pauca, R. J. Plemmons. ``Algorithms and applications for approximate nonnegative matrix factorization.'' Computational Statistics and Data Analysis, vol. 52, issue 1, pp. 155-173, Sept. 2007.

    D. D. Lee and H. S. Seung. ``Algorithms for non-negative matrix factorization.''  Proceedings of the 13th International Conference on Neural Information Processing Systems (NIPS), pp. 535-541, Jan. 2000.

    C. Boutsidis and E. Gallopoulos. ``SVD based initialization: A head start for nonnegative matrix factorization.''  Pattern Recognition, vol. 41, issue 4, pp. 1350-1362, Apr. 2008.

    J. J. Burred. ``Detailed derivation of multiplicative update rules for NMF.'' Techical report, Paris, March 2014.
}

\seealso{
	\code{\link{svd}},
	\code{\link{prcomp}}
}

\examples{
set.seed(1)

a <- matrix(sort(runif(500)), nrow=50, ncol=10)
b <- matrix(rev(sort(runif(500))), nrow=50, ncol=10)
x <- cbind(a, b)

mf <- nnmf_als(x, k=3)
}

\keyword{multivariate}
