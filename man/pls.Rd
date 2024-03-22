\name{pls}

\alias{pls}
\alias{pls_nipals}
\alias{pls_simpls}
\alias{pls_kernel}
\alias{predict.pls}

\alias{opls}
\alias{opls_nipals}
\alias{predict.opls}

\alias{vip}

\title{Partial Least Squares}

\description{
    Partial least squares (PLS), also called projection to latent structures, performs multivariate regression between a data matrix and a response matrix by decomposing both matrixes in a way that explains the maximum amount of covariation between them. It is especially useful when the number of predictors is greater than the number of observations, or when the predictors are highly correlated. Orthogonal partial least squares (OPLS) is also provided.
}

\usage{
# NIPALS algorithm
pls_nipals(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-5,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), \dots)

# SIMPLS algorithm
pls_simpls(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, method = 1L, retscores = TRUE,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), \dots)

# Kernel algorithm
pls_kernel(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, method = 1L, retscores = TRUE,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), \dots)

\S3method{predict}{pls}(object, newdata, k,
	type = c("response", "class"),
	verbose = NA, nchunks = NA,
	BPPARAM = bpparam(), \dots)

# O-PLS algorithm
opls_nipals(x, y, k = 3L, center = TRUE, scale. = FALSE,
	transpose = FALSE, niter = 100L, tol = 1e-9,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), \dots)

\S3method{predict}{opls}(object, newdata, k,
	verbose = NA, nchunks = NA,
	BPPARAM = bpparam(), \dots)

# Variable importance in projection
vip(object, type = c("projection", "weights"))
}

\arguments{
	\item{x}{The data matrix of predictors.}

	\item{y}{The response matrix. (Can also be a factor.)}

	\item{k}{The number of PLS components to use.}

	\item{center}{A logical value indicating whether the variables should be shifted to be zero-centered, or a centering vector of length equal to the number of columns of \code{x}. The centering is performed implicitly and does not change the out-of-memory data in \code{x}.}

    \item{scale.}{A logical value indicating whether the variables should be scaled to have unit variance, or a scaling vector of length equal to the number of columns of \code{x}. The scaling is performed implicitly and does not change the out-of-memory data in \code{x}.}

    \item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This can be useful if the input matrix is (P x N) instead of (N x P) and storing the transpose is expensive. This is not necessary for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} objects, but can be useful for large in-memory (P x N)  matrices.}

	\item{niter}{The maximum number of iterations (per component).}

	\item{tol}{The tolerance for convergence (per component).}

	\item{verbose}{Should progress be printed for each iteration?}

	\item{nchunks}{The number of chunks to use (for centering and scaling only).}

	\item{method}{The kernel algorithm to use, where \code{1} and \code{2} correspond to the two kernel algorithms described by Dayal and MacGregor (1997). For \code{1}, only of the covariance matrix \code{t(X) \%*\% Y} is computed. For \code{2}, the variance matrix \code{t(X) \%*\% X} is also computed. Typically \code{1} will be faster if the number of predictors is large. For a smaller number of predictors, \code{2} will be more efficient.}

	\item{retscores}{Should the scores be computed and returned? This also computes the amount of explained covariance for each component. This is done automatically for NIPALS, but requires additional computation for the kernel algorithms.}

	\item{\dots}{Not currently used.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}. Currently only used for centering and scaling. Use \code{options(matter.matmul.bpparam=TRUE)} to enable parallel matrix multiplication for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices.}

	\item{object}{An object inheriting from \code{pls} or \code{opls}.}

	\item{newdata}{An optional data matrix to use for the prediction.}

	\item{type}{The type of prediction, where \code{"response"} means the fitted response matrix and \code{"class"} will be the vector of class predictions (only valid for discriminant analyses).}
}

\details{
    These functions implement partial least squares (PLS) using the original NIPALS algorithm by Wold et al. (1983), the SIMPLS algorithm by de Jong (1993), or the kernel algorithms by Dayal and MacGregor (1997). A function for calculating orthogonal partial least squares (OPLS) processing using the NIPALS algorithm by Trygg and Wold (2002) is also provided.

    Both regression and classification can be performed. If passed a \code{factor}, then partial least squares discriminant analysis (PLS-DA) will be performed as described by M. Barker and W. Rayens (2003).

    The SIMPLS algorithm (\code{pls_simpls()}) is relatively fast as it does not require the deflation of the data matrix. However, the results will differ slightly from the NIPALS and kernel algorithms for multivariate responses. In these cases, only the first component will be identical. The differences are not meaningful in most cases, but it is worth noting.

    The kernel algorithms (\code{pls_kernel()}) tend to be faster than NIPALS for larger data matrices. The original NIPALS algorithm (\code{pls_nipals()}) is the reference implementation. The results from these algorithms are proven to be equivalent for both univariate and multivariate responses.

    Note that the NIPALS algorithms cannot handle out-of-memory \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices due to the need to deflate the data matrix for each component. \code{x} will be coerced to an in-memory matrix.

    Variable importance in projection (VIP) scores proposed by Wold et al. (1993) measure of the influence each variable has on the PLS model. They can be calculated with \code{vip()}. Note that non-NIPALS models must have \code{retscores = TRUE} for VIP to be calculated. In practice, a VIP score greater than ~1 is a useful criterion for variable selection, although there is no statistical basis for this rule.
}

\value{
    An object of class \code{pls}, with the following components:

    \itemize{
        \item{\code{coefficients}: The regression coefficients.}
        \item{\code{projection}: The projection weights of the regression used to calculate the coefficients from the y-loadings or to project the data to the scores.}
        \item{\code{residuals}: The residuals from regression.}
        \item{\code{fitted.values}: The fitted y matrix.}
        \item{\code{weights}: (Optional) The x-weights of the regression.}
        \item{\code{loadings}: The x-loadings of the latent variables.}
        \item{\code{scores}: (Optional) The x-scores of the latent variables.}
        \item{\code{y.loadings}: The y-loadings of the latent variables.}
        \item{\code{y.scores}: (Optional) The y-scores of the latent variables.}
        \item{\code{cvar}: (Optional) The covariance explained by each component.}
    }

    Or, an object of class \code{opls}, with the following components:

    \itemize{
        \item{\code{weights}: The orthogonal x-weights.}
        \item{\code{loadings}: The orthogonal x-loadings.}
        \item{\code{scores}: The orthogonal x-scores.}
        \item{\code{ratio}: The ratio of the orthogonal weights to the PLS loadings for each component. This provides a measure of how much orthogonal variation is being removed by each component and can be interpreted as a scree plot similar to PCA.}
        \item{\code{x}: The processed data matrix with orthogonal variation removed.}
    }
}

\author{Kylie A. Bemis}

\references{
	S. Wold, H. Martens, and H. Wold. ``The multivariate calibration method in chemistry solved by the PLS method.'' Proceedings on the Conference on Matrix Pencils, Lecture Notes in Mathematics, Heidelberg, Springer-Verlag, pp. 286 - 293, 1983.

	S. de Jong. ``SIMPLS: An alternative approach to partial least squares regression.'' Chemometrics and Intelligent Laboratory Systems, vol. 18, issue 3, pp. 251 - 263, 1993.

	B. S. Dayal and J. F. MacGregor. ``Improved PLS algorithms.'' Journal of Chemometrics, vol. 11, pp. 73 - 85, 1997.

	M. Barker and W. Rayens. ``Partial least squares for discrimination.'' Journal of Chemometrics, vol. 17, pp. 166-173, 2003.

	J. Trygg and S. Wold. ``Orthogonal projections to latent structures.'' Journal of Chemometrics, vol. 16, issue 3, pp. 119 - 128, 2002.

	S. Wold, A. Johansson, and M. Cocchi. ``PLS: Partial least squares projections to latent structures.'' 3D QSAR in Drug Design: Theory, Methods and Applications, ESCOM Science Publishers: Leiden, pp. 523 - 550, 1993.
}

\seealso{
	\code{\link{prcomp}}
}

\examples{
register(SerialParam())

x <- cbind(
		c(-2.18, 1.84, -0.48, 0.83),
		c(-2.18, -0.16, 1.52, 0.83))
y <- as.matrix(c(2, 2, 0, -4))

pls_nipals(x, y, k=2)
}

\keyword{multivariate}
\keyword{regression}
\keyword{classif}
