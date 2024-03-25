\name{sgmix}

\alias{sgmix}
\alias{sgmixn}
\alias{fitted.sgmix}
\alias{logLik.sgmix}

\title{Spatial Gaussian Mixture Model}

\description{
    Spatially segment a single-channel image using a Dirichlet Gaussian mixture model (DGMM).
}

\usage{
# Spatial Gaussian mixture model
sgmix(x, y, vals, r = 1, k = 2,
	weights = c("gaussian", "bilateral", "adaptive"),
	metric = "maximum", p = 2, neighbors = NULL,
	annealing = TRUE, epsilon = 1e-3, niter = 10L,
	verbose = NA, \dots)

# Spatial Gaussian mixture model with groups
sgmixn(x, y, vals, r = 1, k = 2, group = NULL,
	weights = c("gaussian", "bilateral", "adaptive"),
	metric = "maximum", p = 2, neighbors = NULL,
	verbose = NA, ...)

\S3method{fitted}{sgmix}(object,
	type = c("probability", "class", "mean"), \dots)

\S3method{logLik}{sgmix}(object, \dots)
}

\arguments{
	\item{x, y, vals}{Pixel coordinates and their intensity values. Alternatively, \code{x} can be a matrix, in which case the matrix elements are used for \code{vals} and \code{x} and \code{y} are generated from the matrix's dimensions.}

	\item{r}{The spatial smoothing radius.}

	\item{k}{The number of segments (per group, if applicable).}

	\item{weights}{The type of spatial weights to use.}

	\item{metric}{Distance metric to use when finding neighboring pixels. Supported metrics include "euclidean", "maximum", "manhattan", and "minkowski".}

	\item{p}{The power for the Minkowski distance.}

	\item{neighbors}{An optional list giving the neighboring pixel indices for each pixel.}

	\item{annealing}{Should simulated annealing be attempted \emph{every} iteration? (If \code{FALSE}, simulated annealing will still be attempted if the log-likelihood decreases instead of increases during an iteration.)}

	\item{epsilon}{The stopping tolerance. Stop iterating when the change in log-likelihood is less than this value.}

	\item{niter}{The maximum number of iterations.}

	\item{group}{A vector of pixel groups. Pixels belonging to each group will be segmented independently, and will be assigned to different segments.}

	\item{verbose}{Should progress be printed for each iteration?}

	\item{\dots}{Additional options passed to \code{kmeans}.}

	\item{object}{An object inheriting from \code{sgmix}.}

	\item{type}{The type of fitted values, where \code{"probability"} means the posterior probability matrix, \code{"class"} will be the vector of class predictions, and \code{"mean"} will be the vector of class means for each pixel.}
}

\details{
    Spatial segmentation is performed using a Gaussian mixture model from Guo et al. (2019) that uses Dirichlet priors to incorporate spatial dependence. The strength of the spatial smoothing depends on the smoothing radius (\code{r}) and the type of spatial \code{weights}. The "bilateral" and "adaptive" weights can preserve edges better than the standard "gaussian" weights at the expense of a (potentially) noisier segmentation.

    The segmentation is initialized using k-means clustering. An expectation-maximization (E-M) algorithm with gradient descent is then used to estimate the model parameters based on log-likelihood. Optionally, simulated annealing can be used to avoid the model from getting stuck in local maxima.

    To spatially segment different sections of an image independently, use \code{sgmixn}.
}

\value{
    An object of class \code{sgmix}, with the following components:

    \itemize{
        \item{\code{class}: The predicted classes.}
        \item{\code{probability}: A matrix of posterior class probabilities.}
        \item{\code{mu}: The fitted class means.}
        \item{\code{sigma}: The fitted class standard deviations.}
        \item{\code{alpha}: The fitted Dirichlet priors.}
        \item{\code{beta}: The estimated strength of the spatial dependence.}
        \item{\code{group}: (Optional) The corresponding pixel group for the above parameter estimates.}
    }
}

\author{Kylie A. Bemis}

\references{
	D. Guo, K. Bemis, C. Rawlins, J. Agar, and O. Vitek. ``Unsupervised segmentation of mass spectrometric ion images characterizes morphology of tissues'' Bioinformatics, vol. 35, issue 14, pp. i208-i217, 2019.
}

\seealso{
	\code{\link{kmeans}}
}

\examples{
require(datasets)

set.seed(1)
seg <- sgmix(volcano, k=3)

image(fitted(seg, "class"))
}

\keyword{models}
\keyword{cluster}
\keyword{spatial}
