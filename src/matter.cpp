
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

//// Statistical functions applied on MatterAccessor
//--------------------------------------------------

double sum(MatterAccessor<double> & x, bool na_rm) {
    double retVal = 0;
    while ( x ) {
        if ( !na_rm && !R_FINITE(*x) )
        {
            retVal = R_NaN;
            break;
        }
        else if ( R_FINITE(*x) )
        {
            retVal += *x;
        }
        ++x;
    }
    return retVal;
}

double mean(MatterAccessor<double> & x, bool na_rm) {
    double retVal = 0;
    index_type n = 0;
    while ( x ) {
        if ( !na_rm && !R_FINITE(*x) )
        {
            retVal = R_NaN;
            break;
        }
        else if ( R_FINITE(*x) )
        {
            retVal += *x;
            n++;
        }
        ++x;
    }
    return retVal /= n;
}

double var(MatterAccessor<double> & x, bool na_rm) {
    double m_old, m_new, s_old, s_new;
    index_type n = 1;
    m_new = *x;
    s_new = 0;
    ++x;
    while ( x ) {
        if ( !na_rm && !R_FINITE(*x) )
            return R_NaN;
        else if ( R_FINITE(*x) )
        {
            n++;
            m_old = m_new;
            s_old = s_new;
            m_new = m_old + (*x - m_old) / n;
            s_new = s_old + (*x - m_old) * (*x - m_new);
        }
        ++x;
    }
    if ( n < 2 )
        return R_NaN;
    else
        return s_new / (n - 1);
}

//// Statistical methods implemented for class Matter
//---------------------------------------------------

SEXP Matter :: sum(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::sum(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: mean(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::mean(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: var(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(1));
    MatterAccessor<double> x(*this);
    *REAL(retVal) = ::var(x, na_rm);
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::sum(x, na_rm);
            }
            break;
        case 3:
            for ( int j = 0; j < ncols(); j++ )
                pRetVal[j] = 0;
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                int j = 0;
                while ( x ) {
                    if ( !R_FINITE(pRetVal[j]) )
                        pRetVal[j] = R_NaN;
                    else if ( !na_rm && !R_FINITE(*x) )
                        pRetVal[j] = R_NaN;
                    else if ( R_FINITE(*x) )
                        pRetVal[j] = pRetVal[j] += *x;
                    j++;
                    ++x;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::mean(x, na_rm);
            }
            break;
        case 3:
            {
                double * n = (double *) Calloc(ncols(), double);
                for ( int j = 0; j < ncols(); j++ ) {
                    pRetVal[j] = 0;
                    n[j] = 0;
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    MatterAccessor<double> x(*this, i);
                    int j = 0;
                    while ( x ) {
                        if ( !R_FINITE(pRetVal[j]) )
                            pRetVal[j] = R_NaN;
                        else if ( !na_rm && !R_FINITE(*x) )
                            pRetVal[j] = R_NaN;
                        else if ( R_FINITE(*x) )
                        {
                            pRetVal[j] = pRetVal[j] += *x;
                            n[j]++;
                        }
                        j++;
                        ++x;
                    }
                }
                for ( int j = 0; j < ncols(); j++ )
                    pRetVal[j] = pRetVal[j] /= n[j];
                Free(n);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: colvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(ncols()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                pRetVal[j] = ::var(x, na_rm);
            }
            break;
        case 3:
            {
                double * m_old = (double *) Calloc(ncols(), double);
                double * m_new = (double *) Calloc(ncols(), double);
                double * n = (double *) Calloc(ncols(), double);
                for ( int j = 0; j < ncols(); j++ ) {
                    pRetVal[j] = 0;
                    n[j] = 0;
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    MatterAccessor<double> x(*this, i);
                    int j = 0;
                    while ( x ) {
                        if ( !R_FINITE(pRetVal[j]) )
                            pRetVal[j] = R_NaN;
                        else if ( !na_rm && !R_FINITE(*x) )
                            pRetVal[j] = R_NaN;
                        else if ( R_FINITE(*x) )
                        {
                            if ( n[j] < 1 )
                            {
                                n[j]++;
                                m_new[j] = *x;
                                pRetVal[j] = 0;
                            }
                            else
                            {
                                n[j]++;
                                m_old[j] = m_new[j];
                                m_new[j]= m_old[j] + (*x - m_old[j]) / n[j];
                                pRetVal[j] = pRetVal[j] + (*x - m_old[j]) * (*x - m_new[j]);
                            }
                        }
                        j++;
                        ++x;
                    }
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    if ( n[j] < 2 )
                        pRetVal[j] = R_NaN;
                    else
                        pRetVal[j] = pRetVal[j] / (n[j] - 1);
                }
                Free(m_old);
                Free(m_new);
                Free(n);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowsums(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            for ( int i = 0; i < nrows(); i++ )
                pRetVal[i] = 0;
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                int i = 0;
                while ( x ) {
                    if ( !R_FINITE(pRetVal[i]) )
                        pRetVal[i] = R_NaN;
                    else if ( !na_rm && !R_FINITE(*x) )
                        pRetVal[i] = R_NaN;
                    else if ( R_FINITE(*x) )
                        pRetVal[i] = pRetVal[i] += *x;
                    i++;
                    ++x;
                }
            }
            break;
        case 3:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::sum(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowmeans(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            {
                double * n = (double *) Calloc(nrows(), double);
                for ( int i = 0; i < nrows(); i++ ) {
                    pRetVal[i] = 0;
                    n[i] = 0;
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    MatterAccessor<double> x(*this, j);
                    int i = 0;
                    while ( x ) {
                        if ( !R_FINITE(pRetVal[i]) )
                            pRetVal[i] = R_NaN;
                        else if ( !na_rm && !R_FINITE(*x) )
                            pRetVal[i] = R_NaN;
                        else if ( R_FINITE(*x) )
                        {
                            pRetVal[i] = pRetVal[i] += *x;
                            n[i]++;
                        }
                        i++;
                        ++x;
                    }
                }
                for ( int i = 0; i < nrows(); i++ )
                    pRetVal[i] = pRetVal[i] /= n[i];
                Free(n);
            }
            break;
        case 3:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::mean(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rowvar(bool na_rm) {
    SEXP retVal;
    PROTECT(retVal = NEW_NUMERIC(nrows()));
    double * pRetVal = REAL(retVal);
    switch(S4class()) {
        case 1:
            error("'x' must be an array of at least two dimensions");
        case 2:
            {
                double * m_old = (double *) Calloc(nrows(), double);
                double * m_new = (double *) Calloc(nrows(), double);
                double * n = (double *) Calloc(nrows(), double);
                for ( int i = 0; i < nrows(); i++ ) {
                    pRetVal[i] = R_NaN;
                    n[i] = 0;
                }
                for ( int j = 0; j < ncols(); j++ ) {
                    MatterAccessor<double> x(*this, j);
                    int i = 0;
                    while ( x ) {
                        if ( !R_FINITE(pRetVal[i]) )
                            pRetVal[i] = R_NaN;
                        else if ( !na_rm && !R_FINITE(*x) )
                            pRetVal[i] = R_NaN;
                        else if ( R_FINITE(*x) )
                        {
                            if ( n[i] < 1 )
                            {
                                n[i]++;
                                m_new[i] = *x;
                                pRetVal[i] = 0;
                            }
                            else
                            {
                                n[i]++;
                                m_old[i] = m_new[i];
                                m_new[i]= m_old[i] + (*x - m_old[i]) / n[i];
                                pRetVal[i] = pRetVal[i] + (*x - m_old[i]) * (*x - m_new[i]);
                            }
                        }
                        i++;
                        ++x;
                    }
                }
                for ( int i = 0; i < nrows(); i++ ) {
                    if ( n[i] < 2 )
                        pRetVal[i] = R_NaN;
                    else
                        pRetVal[i] = pRetVal[i] / (n[i] - 1);
                }
                Free(m_old);
                Free(m_new);
                Free(n);
            }
            break;
        case 3:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                pRetVal[i] = ::var(x, na_rm);
            }
            break;
    }
    UNPROTECT(1);
    return retVal;
}

SEXP Matter :: rmult(SEXP y) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(DataType<double>(), nrows(), ::ncols(y)));
    double * pRetMat = REAL(retMat);
    double * pY = REAL(y);
    for ( int k = 0; k < LENGTH(retMat); k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case 2:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> x(*this, j);
                int i = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ::ncols(retMat); jj++ ) {
                        double val = (*x) * pY[j + jj * ::nrows(y)];
                        pRetMat[i + jj * ::nrows(retMat)] += val;
                    }
                    i++;
                    ++x;
                }
            }
            break;
        case 3:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> x(*this, i);
                int j = 0;
                while ( x ) {
                    for ( int jj = 0; jj < ::ncols(retMat); jj++ ) {
                        double val = (*x) * pY[j + jj * ::nrows(y)];
                        pRetMat[i + jj * ::nrows(retMat)] += val;
                    }
                    j++;
                    ++x;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
}

SEXP Matter :: lmult(SEXP x) {
    SEXP retMat;
    PROTECT(retMat = allocMatrix(DataType<double>(), ::nrows(x), ncols()));
    double * pRetMat = REAL(retMat);
    double * pX = REAL(x);
    for ( int k = 0; k < LENGTH(retMat); k++ )
        pRetMat[k] = 0;
    switch(S4class()) {
        case 2:
            for ( int j = 0; j < ncols(); j++ ) {
                MatterAccessor<double> y(*this, j);
                int i = 0;
                while ( y ) {
                    for ( int ii = 0; ii < ::nrows(retMat); ii++ ) {
                        double val = pX[ii + i * ::nrows(x)] * (*y);
                        pRetMat[ii + j * ::nrows(retMat)] += val;
                    }
                    i++;
                    ++y;
                }
            }
            break;
        case 3:
            for ( int i = 0; i < nrows(); i++ ) {
                MatterAccessor<double> y(*this, i);
                int j = 0;
                while ( y ) {
                    for ( int ii = 0; ii < ::nrows(retMat); ii++ ) {
                        double val = pX[ii + i * ::nrows(x)] * (*y);
                        pRetMat[ii + j * ::nrows(retMat)] += val;
                    }
                    j++;
                    ++y;
                }
            }
            break;
    }
    UNPROTECT(1);
    return retMat;
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

    SEXP getSum(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.sum(LOGICAL_VALUE(na_rm));
    }

    SEXP getMean(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.mean(LOGICAL_VALUE(na_rm));
    }

    SEXP getVar(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.var(LOGICAL_VALUE(na_rm));
    }

    SEXP getColSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colsums(LOGICAL_VALUE(na_rm));
    }

    SEXP getColMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colmeans(LOGICAL_VALUE(na_rm));
    }

    SEXP getColVar(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.colvar(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowSums(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowsums(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowMeans(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowmeans(LOGICAL_VALUE(na_rm));
    }

    SEXP getRowVar(SEXP x, SEXP na_rm) {
        Matter mMat(x);
        return mMat.rowvar(LOGICAL_VALUE(na_rm));
    }

    SEXP rightMultRMatrix(SEXP x, SEXP y) {
        Matter mMat(x);
        return mMat.rmult(y);
    }

    SEXP leftMultRMatrix(SEXP x, SEXP y) {
        Matter mMat(y);
        return mMat.lmult(x);
    }
}
