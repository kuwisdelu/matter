\name{mi_learn}

\alias{mi_learn}

\title{Multiple Instance Learning}

\description{
    Multiple instance learning is a strategy for training classifiers when the class labels are observed at a coarser level than the individual data points. For example, if an entire image is classified as "positive" or "negative" but the classifier is trained and predicts at the pixel level.
}

\usage{
mi_learn(fn, x, y, group, pos = 1L,
	threshold = 0.01, verbose = NA, \dots)
}

\arguments{
	\item{fn}{The function used to train the classifier.}

	\item{x}{The data matrix.}

	\item{y}{The response. This can be the same length as the data, or the same length as the number of groups/bags. Must have exactly two levels when coerced to a factor.}

	\item{group}{The group (or "bag") to which each data point belongs. The class labels are observed per-group rather than per data point.}

	\item{pos}{The positive class label, as a string matching one of the levels of \code{y}, or the index of the level.}

	\item{threshold}{The stopping criterion. The learning stops when the proportion of updated labels between iterations is less than this value.}

	\item{verbose}{Should progress be printed for each iteration? \emph{Not} passed to \code{fn}.}

	\item{\dots}{Additional options passed to \code{fn}.}
}

\details{
    This is a generic wrapper for applying a multiple instance learning strategy for any classifier that satisfies certain criteria. The labels must be binary (positive and negative).

    The multiple instance learning algorithm here assumes that if a single data point is positive, then the entire group (or "bag") to which it belongs is labeled as positive. For example, if a single pixel in an image indicates the presence of disease, then the entire image is labeled as disease.

    The model returned by \code{fn} must support a \code{fitted} method with a second argument for extracting the predictions as either \code{"response"} or \code{"class"} values. See the \code{nscentroids} function as an example of such a model.
}

\value{
    A model object returned by \code{fn}.
}

\author{Kylie A. Bemis}

\references{
	D. Guo, M. C. Foell, V. Volkmann, K. Enderle-Ammour, P. Bronsert, O. Shilling, and O. Vitek. ``Deep multiple instance learning classifies subtissue locations in mass spectrometry images from tissue-level annotations.'' Bioinformatics, vol. 36, issue Supplement_1, pp. i300-i308, 2020.
}

\seealso{
	\code{\link{nscentroids}}
}

\examples{
register(SerialParam())
set.seed(1)
n <- 100
p <- 5
g <- 5
group <- rep(paste0("s", seq_len(g)), each=n \%/\% g)
group <- factor(rep_len(group, n))
x <- matrix(rnorm(n * p), nrow=n, ncol=p)
colnames(x) <- paste0("x", seq_len(p))

# create bagged labels
y <- ifelse(group \%in\% c("s1", "s2", "s3"), "pos", "neg")
y <- factor(y, levels=c("pos", "neg"))
ipos <- which(y == "pos")
ineg <- which(y == "neg")
z <- y

# create "true" labels (with some within-bag noise)
z[ipos] <- sample(c("pos", "neg"), length(ipos), replace=TRUE)
jpos <- which(z == "pos")
jneg <- which(z == "neg")

# create data
x[jpos,] <- x[jpos,] + rnorm(p * length(jpos), mean=1)
x[jneg,] <- x[jneg,] - rnorm(p * length(jneg), mean=1)

# fit ordinary NSC and mi-NSC
fit0 <- nscentroids(x=x, y=y)
fit1 <- mi_learn(nscentroids, x=x, y=y, group=group, priors=1)

# improved performance on "true" labels
mean(fitted(fit0, "class") == z)
mean(fitted(fit1, "class") == z)
}

\keyword{classif}
