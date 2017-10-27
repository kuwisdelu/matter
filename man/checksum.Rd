\name{checksum}
\docType{methods}

\alias{checksum}
\alias{checksum,matter-method}

\title{Create checksums and hashes}

\description{
    This is a generic function for applying cryptographic hash functions and calculating checksums for arbitrary R objects.
}

\usage{
checksum(x, \dots)

\S4method{checksum}{matter}(x, algo = c("sha1", "md5"), \dots)
}

\arguments{
    \item{x}{An object to be hashed.}
    
    \item{algo}{The hash function to use.}

    \item{\dots}{Additional arguments to be passed to the hash function.}
}

\details{
	The method for \code{\link[matter]{matter}} objects calculates checksums of each of the files in the object's \code{paths}.
}

\value{
	A character vector giving the hash or hashes of the object.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[digest]{digest}}
}

\examples{
x <- matter(1:10)
y <- matter(1:10)

checksum(x)
checksum(y) # should be the same
}

\keyword{utilities}
