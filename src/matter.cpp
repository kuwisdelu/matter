
#include "matter.h"

//// Vector methods implemented for class Matter
//-----------------------------------------------

template<typename RType>
SEXP Matter :: readVector() {
    SEXP retVec;
    Atoms atoms(data());
    PROTECT(retVec = allocVector(DataType<RType>(), length()));
    RType * pRetVec = DataPtr<RType>(retVec);
    atoms.read<RType>(pRetVec, 0, length(), files());
    UNPROTECT(1);
    return retVec;
}

template<typename RType>
void Matter :: writeVector(SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    Atoms atoms(data());
    atoms.write<RType>(pValue, 0, length(), files());
}

template<typename RType>
SEXP Matter :: readVectorElements(SEXP i) {
    SEXP retVec;
    PROTECT(retVec = allocVector(DataType<RType>(), XLENGTH(i)));
    RType * pRetVec = DataPtr<RType>(retVec);
    double * pIndex = REAL(i);
    Atoms atoms(data());
    atoms.readAt<RType>(pRetVec, pIndex, XLENGTH(i), files());
    UNPROTECT(1);
    return retVec;
}

template<typename RType>
void Matter :: writeVectorElements(SEXP i, SEXP value) {
    RType * pValue = DataPtr<RType>(value);
    double * pIndex = REAL(i);
    Atoms atoms(data());
    atoms.writeAt(pValue, pIndex, XLENGTH(i), files());
}

//// Matrix methods implemented for class Matter
//-----------------------------------------------

template<typename RType>
SEXP Matter :: readMatrix() {
    SEXP retMat;
    int nrows = this->nrows(), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    switch(S4class()) {
        case 2:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col));
                atoms.read<RType>(pRetMat + col * nrows, 0, nrows, files());
            }
            break;
        case 3:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row));
                atoms.read<RType>(pRetMat + row, 0, ncols, files(), nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrix(SEXP value) {
    int nrows = this->nrows(), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    switch(S4class()) {
        case 2:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col));
                atoms.write<RType>(pValue + col * nrows, 0, nrows, files());
            }
            break;
        case 3:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row));
                atoms.write<RType>(pValue + row, 0, ncols, files(), nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixRows(SEXP i) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = this->ncols();
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    double * pRow = REAL(i);
    switch(S4class()) {
        case 2:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col));
                atoms.readAt<RType>(pRetMat + col * nrows, pRow, nrows, files());
            }
            break;
        case 3:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_type row = static_cast<index_type>(pRow[l]);
                    Atoms atoms(data(row));
                    atoms.read<RType>(pRetMat + l, 0, ncols, files(), nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixRows(SEXP i, SEXP value) {
    int nrows = LENGTH(i), ncols = this->ncols();
    RType * pValue = DataPtr<RType>(value);
    double * pRow = REAL(i);
    switch(S4class()) {
        case 2:
            for ( int col = 0; col < ncols; col++ ) {
                Atoms atoms(data(col));
                atoms.writeAt(pValue + col * nrows, pRow, nrows, files());
            }
            break;
        case 3:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_type row = static_cast<index_type>(pRow[l]);
                Atoms atoms(data(row));
                atoms.write<RType>(pValue + l, 0, ncols, files(), nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixCols(SEXP j) {
    SEXP retMat;
    int nrows = this->nrows(), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    double * pCol = REAL(j);
    switch(S4class()) {
        case 2:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_type col = static_cast<index_type>(pCol[l]);
                    Atoms atoms(data(col));
                    atoms.read<RType>(pRetMat + l * nrows, 0, nrows, files());
                }
            }
            break;
        case 3:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row));
                atoms.readAt<RType>(pRetMat + row, pCol, ncols, files(), nrows);
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixCols(SEXP j, SEXP value) {
    int nrows = this->nrows(), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType>(value);
    double * pCol = REAL(j);
    switch(S4class()) {
        case 2:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_type col = static_cast<index_type>(pCol[l]);
                Atoms atoms(data(col));
                atoms.write<RType>(pValue + l * nrows, 0, nrows, files());
            }
            break;
        case 3:
            for ( int row = 0; row < nrows; row++ ) {
                Atoms atoms(data(row));
                atoms.writeAt(pValue + row, pCol, ncols, files(), nrows);
            }
            break;
    }
}

template<typename RType>
SEXP Matter :: readMatrixElements(SEXP i, SEXP j) {
    SEXP retMat;
    int nrows = LENGTH(i), ncols = LENGTH(j);
    PROTECT(retMat = allocMatrix(DataType<RType>(), nrows, ncols));
    RType * pRetMat = DataPtr<RType>(retMat);
    double * pRow = REAL(i);
    double * pCol = REAL(j);
    switch(S4class()) {
        case 2:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    fillNA<RType>(pRetMat + l * nrows, nrows);
                else {
                    index_type col = static_cast<index_type>(pCol[l]);
                    Atoms atoms(data(col));
                    atoms.readAt<RType>(pRetMat + l * nrows, pRow, nrows, files());
                }
            }
            break;
        case 3:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    fillNA<RType>(pRetMat + l, ncols, nrows);
                else {
                    index_type row = static_cast<index_type>(pRow[l]);
                    Atoms atoms(data(row));
                    atoms.readAt<RType>(pRetMat + l, pCol, ncols, files(), nrows);
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

template<typename RType>
void Matter :: writeMatrixElements(SEXP i, SEXP j, SEXP value) {
    int nrows = LENGTH(i), ncols = LENGTH(j);
    RType * pValue = DataPtr<RType>(value);
    double * pRow = REAL(i);
    double * pCol = REAL(j);
    switch(S4class()) {
        case 2:
            for ( int l = 0; l < ncols; l++ ) {
                if ( ISNA(pCol[l]) )
                    continue;
                index_type col = static_cast<index_type>(pCol[l]);
                Atoms atoms(data(col));
                atoms.writeAt(pValue + l * nrows, pRow, nrows, files());
            }
            break;
        case 3:
            for ( int l = 0; l < nrows; l++ ) {
                if ( ISNA(pRow[l]) )
                    continue;
                index_type row = static_cast<index_type>(pRow[l]);
                Atoms atoms(data(row));
                atoms.writeAt(pValue + l, pCol, ncols, files(), nrows);
            }
            break;
    }
}

//// Functions to be called from R
//---------------------------------

extern "C" {

    SEXP getVector(SEXP x) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case 1:
                return mVec.readVector<int>();
            case 2:
                return mVec.readVector<double>();
            default:
                return R_NilValue;
        }
    }

    void setVector(SEXP x, SEXP value) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case 1:
                mVec.writeVector<int>(value);
                break;
            case 2:
                mVec.writeVector<double>(value);
                break;
        }
    }

    SEXP getVectorElements(SEXP x, SEXP i) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case 1:
                return mVec.readVectorElements<int>(i);
            case 2:
                return mVec.readVectorElements<double>(i);
            default:
                return R_NilValue;
        }
    }

    void setVectorElements(SEXP x, SEXP i, SEXP value) {
        Matter mVec(x);
        switch(mVec.datamode()) {
            case 1:
                mVec.writeVectorElements<int>(i, value);
                break;
            case 2:
                mVec.writeVectorElements<double>(i, value);
                break;
        }
    }

    SEXP getMatrix(SEXP x) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                return mMat.readMatrix<int>();
            case 2:
                return mMat.readMatrix<double>();
            default:
                return R_NilValue;
        }
    }

    void setMatrix(SEXP x, SEXP value) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                mMat.writeMatrix<int>(value);
                break;
            case 2:
                mMat.writeMatrix<double>(value);
                break;
        }
    }

    SEXP getMatrixRows(SEXP x, SEXP i) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                return mMat.readMatrixRows<int>(i);
            case 2:
                return mMat.readMatrixRows<double>(i);
            default:
                return R_NilValue;
        }
    }

    void setMatrixRows(SEXP x, SEXP i, SEXP value) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                mMat.writeMatrixRows<int>(i, value);
                break;
            case 2:
                mMat.writeMatrixRows<double>(i, value);
                break;
        }
    }

    SEXP getMatrixCols(SEXP x, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                return mMat.readMatrixCols<int>(j);
            case 2:
                return mMat.readMatrixCols<double>(j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixCols(SEXP x, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                mMat.writeMatrixCols<int>(j, value);
                break;
            case 2:
                mMat.writeMatrixCols<double>(j, value);
                break;
        }
    }

    SEXP getMatrixElements(SEXP x, SEXP i, SEXP j) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                return mMat.readMatrixElements<int>(i, j);
            case 2:
                return mMat.readMatrixElements<double>(i, j);
            default:
                return R_NilValue;
        }
    }

    void setMatrixElements(SEXP x, SEXP i, SEXP j, SEXP value) {
        Matter mMat(x);
        switch(mMat.datamode()) {
            case 1:
                mMat.writeMatrixElements<int>(i, j, value);
                break;
            case 2:
                mMat.writeMatrixElements<double>(i, j, value);
                break;
        }
    }

}
