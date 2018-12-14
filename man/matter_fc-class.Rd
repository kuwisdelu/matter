\name{matter_fc-class}
\docType{class}

\alias{class:matter_fc}
\alias{matter_fc}
\alias{matter_fc-class}

\alias{[,matter_fc-method}
\alias{[,matter_fc,ANY,missing,ANY-method}
\alias{[,matter_fc,ANY,missing,NULL-method}
\alias{[<-,matter_fc-method}
\alias{[<-,matter_fc,ANY,missing,ANY-method}

\alias{levels,matter_fc-method}
\alias{levels<-,matter_fc-method}

\title{Out-of-Memory Factors}

\description{
    The \code{matter_fc} class implements out-of-memory factors.
}

\usage{
## Instance creation
matter_fc(data, datamode = "int", paths = NULL,
            filemode = ifelse(all(file.exists(paths)), "r", "rw"),
            offset = 0, extent = length, length = 0L, names = NULL,
            levels = base::levels(as.factor(data)), \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to the data in virtual memory if provided.}

        \item{datamode}{Must be an integral type for factors.}

        \item{paths}{A 'character' vector of the paths to the files where the data are stored. If 'NULL', then a temporary file is created using \code{\link{tempfile}}.}

        \item{filemode}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'paths', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'paths', specifying the number of elements of size 'datamode' to be accessed from each file.}

        \item{length}{An optional number giving the total length of the data across all files, equal to the sum of 'extent'. This is ignored and calculated automatically if 'extent' is specified.}

        \item{names}{The names of the data elements.}

        \item{levels}{The levels of the factor.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data in virtual memory and within the files.}

        \item{\code{datamode}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'character' vector of with possible values 'raw', 'logical', 'integer', 'numeric', or 'virtual'.}

        \item{\code{paths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{\code{chunksize}:}{The maximum number of elements which should be loaded into memory at once. Used by methods implementing summary statistics and linear algebra. Ignored when explicitly subsetting the dataset.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{ops}:}{Delayed operations to be applied on atoms.}

        \item{\code{levels}:}{The levels of the factor.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}},
   \code{\linkS4class{matter_vec}}
}

\section{Creating Objects}{
    \code{matter_fc} instances can be created through \code{matter_fc()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i], x[i] <- value}:}{Get or set the elements of the factor.}

        \item{\code{levels(x), levels(x) <- value}:}{Get or set the levels of the factor.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_fc}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}},
    \code{\linkS4class{matter_vec}}
}

\examples{
x <- matter_fc(c("a", "a", "b"), levels=c("a", "b", "c"))
x[]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
