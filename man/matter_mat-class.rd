\name{matter_mat-class}
\docType{class}

\alias{class:matter_mat}
\alias{matter_mat}
\alias{matter_matc}
\alias{matter_matr}
\alias{matter_mat-class}
\alias{matter_matc-class}
\alias{matter_matr-class}

\alias{[,matter_mat-method}
\alias{[,matter_mat,ANY,ANY,logical-method}
\alias{[,matter_mat,ANY,ANY,missing-method}
\alias{[,matter_mat,ANY,missing,logical-method}
\alias{[,matter_mat,ANY,missing,missing-method}
\alias{[,matter_mat,missing,ANY,logical-method}
\alias{[,matter_mat,missing,ANY,missing-method}
\alias{[,matter_mat,missing,missing,missing-method}
\alias{[<-,matter_mat-method}
\alias{[<-,matter_mat,ANY,ANY,ANY-method}
\alias{[<-,matter_mat,ANY,missing,ANY-method}
\alias{[<-,matter_mat,missing,ANY,ANY-method}
\alias{[<-,matter_mat,missing,missing,ANY-method}

\alias{cbind,matter-method}
\alias{rbind,matter-method}

\alias{t.matter}
\alias{t,matter_matc-method}
\alias{t,matter_matr-method}

\alias{\%*\%,matter,matter-method}
\alias{\%*\%,matrix,matter_mat-method}
\alias{\%*\%,matter_mat,matrix-method}
\alias{\%*\%,matter_matc,numeric-method}
\alias{\%*\%,matter_matr,numeric-method}
\alias{\%*\%,numeric,matter_matc-method}
\alias{\%*\%,numeric,matter_matr-method}

\alias{crossprod,matter,ANY-method}
\alias{crossprod,ANY,matter-method}
\alias{tcrossprod,matter,ANY-method}
\alias{tcrossprod,ANY,matter-method}

\title{Matrices Stored on Disk}

\description{
    The \code{matter_mat} class implements on-disk matrices.
}

\usage{
## Instance creation
matter_mat(data, datamode = "double", paths = NULL,
            filemode = ifelse(all(file.exists(paths)), "rb", "rb+"),
            offset = c(0, cumsum(sizeof(datamode) * extent)[-length(extent)]),
            extent = if (rowMaj) rep(ncol, nrow) else rep(nrow, ncol),
            nrow = 0, ncol = 0, rowMaj = FALSE, dimnames = NULL, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data matrix which will be initially written to the data on disk if provided.}

        \item{datamode}{A 'character' vector giving the storage mode of the data on disk. Allowable values are the C types ('char', 'uchar', short', 'ushort', 'int', 'uint', 'long', 'ulong', 'float') and their R equivalents ('raw', 'logical', 'integer', 'numeric').}

        \item{paths}{A 'character' vector of the paths to the files where the data are stored. If 'NULL', then a temporary file is created using \code{\link[base]{tempfile}}.}

        \item{filemode}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'paths', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'paths', specifying the number of elements of size 'datamode' to be accessed from each file.}

        \item{nrow}{An optional number giving the total number of rows.}

        \item{ncol}{An optional number giving the total number of columns.}

        \item{rowMaj}{Whether the data should be stored in row-major order (as opposed to column-major order) on disk. Defaults to 'FALSE', for efficient access to columns. Set to 'TRUE' for more efficient access to rows instead.}

        \item{dimnames}{The names of the matrix dimensions.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the accessed data when read into R. This should a 'character' vector of length one with value 'integer' or 'numeric'.}

        \item{\code{paths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

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
    \code{matter_mat} instances can be created through \code{matter_mat()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i,j], x[i,j] <- value}:}{Get or set the elements of the matrix.}

        \item{\code{x \%*\% y}:}{Matrix multiplication. At least one matrix must be an in-memory R matrix (or vector).}

        \item{\code{crossprod(x, y)}:}{Alias for t(x) \%*\% y.}

        \item{\code{tcrossprod(x, y)}:}{Alias for x \%*\% t(y).}

        \item{\code{cbind(x, ...), rbind(x, ...)}:}{Combine matrices by row or column.}

        \item{\code{t(x)}:}{Transpose a matrix. This is a quick operation which only changes metadata and does not touch the on-disk data.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_mat}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_mat(1:100, nrow=10, ncol=10)
x[]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
