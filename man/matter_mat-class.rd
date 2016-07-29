\name{matter_mat-class}
\docType{class}

\alias{class:matter_mat}
\alias{matter_mat}
\alias{matter_mat-class}

\alias{[,matter_mat-method}
\alias{[<-,matter_mat-method}

\title{Matrices Stored on Disk}

\description{
    The \code{matter_mat} class implements on-disk matrices.
}

\usage{
## Instance creation
matter_mat(data, datamode = "double", filepath = NULL,
            filemode = ifelse(is.null(filepath), "rb+", "rb"),
            offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
            extent = if (rowMaj) rep(ncol, nrow) else rep(nrow, ncol),
            nrow = 0, ncol = 0, rowMaj = FALSE, dimnames = NULL, \dots)

## Additional methods documented below
}

\arguments{
        \item{\code{data}}{An optional data vector which will be initially written to the data on disk if provided.}

        \item{\code{datamode}}{A 'character' vector giving the storage mode of the data on disk. Allowable values are 'short', 'int', 'long', 'float', and 'double'.}

        \item{\code{filepath}}{A 'character' vector of the paths to the files where the data are stored. If 'NULL', then a temporary file is created using \code{\link[base]{tempfile}}.}

        \item{\code{filemode}}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

        \item{\code{offset}}{A vector giving the offsets in number of bytes from the beginning of each file in 'filepath', specifying the start of the data to be accessed for each file.}

        \item{\code{extent}}{A vector giving the length of the data for each file in 'filepath', specifying the number of elements of size 'datamode' to be accessed from each file.}

        \item{\code{nrow}}{An optional number giving the total number of rows.}

        \item{\code{ncol}}{An optional number giving the total number of columns.}

        \item{\code{rowMaj}}{Whether the data should be stored in row-major order (as opposed to column-major order) on disk. Defaults to 'FALSE', for efficient access to columns. Set to 'TRUE' for more efficient access to rows instead.}

        \item{\code{dimnames}}{The names of the matrix dimensions.}

        \item{\code{\dots}}{Additional arguments to be passed to constructor.}
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
    \code{matter_mat} instances can be created through \code{matter_mat()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i,j], x[i,j] <- value}:}{Get or set the elements of the vector.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
\dontrun{
## Create an matter_mat object
matter_mat()
}
}

\keyword{classes}
\keyword{array}
\keyword{IO}
