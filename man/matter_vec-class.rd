\name{matter_vec-class}
\docType{class}

\alias{class:matter_vec}
\alias{matter_vec}
\alias{matter_vec-class}

\alias{[,matter_vec-method}
\alias{[<-,matter_vec-method}

\title{Vectors Stored on Disk}

\description{
    The \code{matter_vec} class implements on-disk vectors.
}

\usage{
## Instance creation
matter_vec(data, datamode = "double", filepath = NULL,
            filemode = ifelse(is.null(filepath), "rb+", "rb"),
            offset = 0, extent = length, length = 0L, names = NULL)

## Additional methods documented below
}

\arguments{
        \item{\code{data}}{An optional data vector which will be initially written to the data on disk if provided.}

        \item{\code{datamode}}{A 'character' vector giving the storage mode of the data on disk. Allowable values are 'short', 'int', 'long', 'float', and 'double'.}

        \item{\code{filepath}}{A 'character' vector of the paths to the files where the data are stored. If 'NULL', then a temporary file is created using \code{\link[base]{tempfile}}.}

        \item{\code{filemode}}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

        \item{\code{offset}}{A vector giving the offsets in number of bytes from the beginning of each file in 'filepath', specifying the start of the data to be accessed for each file.}

        \item{\code{extent}}{A vector giving the length of the data for each file in 'filepath', specifying the number of elements of size 'datamode' to be accessed from each file.}

        \item{\code{length}}{An optional number giving the total length of the data across all files, equal to the sum of 'extent'. This is ignored and calculated automatically if 'extent' is specified.}

        \item{\code{names}}{The names of the data elements.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the accessed data when read into R. This should a 'character' vector of length one with value 'integer' or 'numeric'.}

        \item{\code{filepath}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}
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
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
\dontrun{
## Create an matter_vec object
matter_vec()
}
}

\keyword{classes}
\keyword{array}
\keyword{IO}
