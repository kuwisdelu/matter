\name{nscentroids}

\alias{nscentroids}
\alias{fitted.nscentroids}
\alias{predict.nscentroids}
\alias{logLik.nscentroids}

\title{Nearest Shrunken Centroids}

\description{
    Nearest shrunken centroids performs regularized classification of high-dimensional data. Originally developed for classification of microarrays, it calculates test statistics for each feature/dimension based on the deviation between the class centroids and the global centroid. It applies regularization (via soft thresholding) to these test statistics to produce shrunken centroids for each class.
}

\usage{
# Nearest shrunken centroids
nscentroids(x, y, s = 0, distfun = NULL,
	priors = table(y), center = NULL, transpose = FALSE,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), \dots)

\S3method{fitted}{nscentroids}(object, type = c("response", "class"), \dots)

\S3method{predict}{nscentroids}(object, newdata,
	type = c("response", "class"), \dots)

\S3method{logLik}{nscentroids}(object, \dots)
}

\arguments{
	\item{x}{The data matrix.}

	\item{y}{The response. (Coerced to a factor.)}

	\item{s}{The sparsity (soft thresholding) parameter used to shrink the test statistics. May be a vector.}

	\item{distfun}{The function of the form \code{function(x, y, \dots)} used to generate a distance function of the form \code{function(i)} giving the distances between the \code{i}th object(s) in \code{x} and \emph{all} objects in \code{y}.}

	\item{priors}{The prior probabilities or sample sizes for each class. (Will be normalized.)}

	\item{center}{An optional vector giving the pre-calculated global centroid.}

    \item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This can be useful if the input matrix is (P x N) instead of (N x P) and storing the transpose is expensive. This is not necessary for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} objects, but can be useful for large in-memory (P x N)  matrices.}

	\item{verbose}{Should progress be printed for each iteration? \emph{Not} passed to \code{distfun}.}

	\item{nchunks}{The number of chunks to use (for centering and scaling only). \emph{Passed} to \code{distfun}.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}. \emph{Passed} to \code{distfun}.}

	\item{\dots}{Additional options passed to \code{distfun}.}

	\item{object}{An object inheriting from \code{nscentroids}.}

	\item{newdata}{An optional data matrix to use for the prediction.}

	\item{type}{The type of prediction, where \code{"response"} means the posterior probability matrix and \code{"class"} will be the vector of class predictions.}
}

\details{
    This functions implements nearest shrunken centroids based on the original algorithm by Tibshirani et al. (2002). It provides a sparse strategy for classification based on regularized class centroids. The class centroids are shrunken toward the global centroid. The shrunken test statistics used to perform the regularization can then be interpreted to determine which features are relevant to the classification. (Important features will have nonzero test statitistics after soft thresholding.)

    Unlike the original algorithm, this implementation allows specifying a custom dissimilarity function. If not provided, then this defaults to \code{rowDistFun()} if \code{transpose=FALSE} or \code{colDistFun()} if \code{transpose=TRUE}.

    If a custom function is passed, it should take the form \code{function(x, y, \dots)}, and it must \emph{return} a function of the form \code{function(i)}. The returned function should return the distances between the \code{i}th object(s) in \code{x} and \emph{all} objects in \code{y}. \code{rowDistFun()} and \code{colDistFun()} are examples of functions that satisfy these properties.
}

\value{
    An object of class \code{nscentroids}, with the following components:

    \itemize{
        \item{\code{class}: The predicted classes.}
        \item{\code{probability}: A matrix of posterior class probabilities.}
        \item{\code{centers}: The shrunken class centroids used for classification.}
        \item{\code{statistic}: The shrunken test statistics.}
        \item{\code{sd}: The pooled within-class standard deviations for each feature.}
        \item{\code{priors}: The prior class probabilities.}
        \item{\code{s}: The regularization (soft thresholding) parameter.}
        \item{\code{distfun}: The function used to generate the dissimilarity function.}
    }
}

\author{Kylie A. Bemis}

\references{
	R. Tibshirani, T. Hastie, B. Narasimhan, and G. Chu. ``Diagnosis of multiple cancer types by shrunken centroids of gene expression.'' Proceedings of the National Academy of Sciences of the USA, vol. 99, no. 10, pp. 6567-6572, 2002.

	R. Tibshirani, T. Hastie, B. Narasimhan, and G. Chu. ``Class prediction by nearest shrunken with applications to DNA microarrays.'' Statistical Science, vol. 18, no. 1, pp. 104-117, 2003.
}

\seealso{
	\code{\link{rowDistFun}},
	\code{\link{colDistFun}}
}

\examples{
register(SerialParam())

set.seed(1)
n <- 100
p <- 5
x <- matrix(rnorm(n * p), nrow=n, ncol=p)
colnames(x) <- paste0("x", seq_len(p))
y <- ifelse(x[,1L] > 0 | x[,2L] < 0, "a", "b")

nscentroids(x, y, s=1.5)
}

\keyword{classif}
