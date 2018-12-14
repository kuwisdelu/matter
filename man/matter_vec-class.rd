\name{matter_vec-class}
\docType{class}

\alias{class:matter_vec}
\alias{matter_vec}
\alias{matter_vec-class}

\alias{[,matter_vec-method}
\alias{[,matter_vec,ANY,missing,ANY-method}
\alias{[,matter_vec,ANY,missing,NULL-method}
\alias{[,matter_vec,missing,missing,ANY-method}
\alias{[<-,matter_vec-method}
\alias{[<-,matter_vec,ANY,missing,ANY-method}
\alias{[<-,matter_vec,missing,missing,ANY-method}

\alias{c,matter_vec-method}

\alias{t,matter_vec-method}

\alias{dim<-,matter_vec-method}

\alias{as.vector,matter_vec-method}
\alias{as.matrix,matter_vec-method}
\alias{as.array,matter_vec-method}

\title{Vectors Stored on Disk}

\description{
    The \code{matter_vec} class implements on-disk vectors.
}

\usage{
## Instance creation
matter_vec(data, datamode = "double", paths = NULL,
            filemode = ifelse(all(file.exists(paths)), "r", "rw"),
            offset = 0, extent = length, length = 0L, names = NULL, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to the data on disk if provided.}

        \item{datamode}{A 'character' vector giving the storage mode of the data on disk. Allowable values are the C types ('char', 'uchar', short', 'ushort', 'int', 'uint', 'long', 'ulong', 'float') and their R equivalents ('raw', 'logical', 'integer', 'numeric').}

        \item{paths}{A 'character' vector of the paths to the files where the data are stored. If 'NULL', then a temporary file is created using \code{\link{tempfile}}.}

        \item{filemode}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'paths', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'paths', specifying the number of elements of size 'datamode' to be accessed from each file.}

        \item{length}{An optional number giving the total length of the data across all files, equal to the sum of 'extent'. This is ignored and calculated automatically if 'extent' is specified.}

        \item{names}{The names of the data elements.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'character' vector of length one with value 'integer' or 'numeric'.}

        \item{\code{paths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{\code{chunksize}:}{The maximum number of elements which should be loaded into memory at once. Used by methods implementing summary statistics and linear algebra. Ignored when explicitly subsetting the dataset.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{ops}:}{Delayed operations to be applied on atoms.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{matter_vec} instances can be created through \code{matter_vec()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i], x[i] <- value}:}{Get or set the elements of the vector.}

        \item{\code{c(x, ...)}:}{Combine vectors.}

        \item{\code{t(x)}:}{Transpose a vector (to a row matrix). This is a quick operation which only changes metadata and does not touch the on-disk data.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_vec}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_vec(1:100)
x[]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
