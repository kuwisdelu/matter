
#ifndef MATTER_H
#define MATTER_H

#include <R.h>
#include <Rdefines.h>

#include <cstdio>
#include <cstdlib>

#include "utils.h"

#define within_bounds(x, a, b) ((a) <= (x) && (x) < (b))
#define out_of_bounds(x, a, b) ((x) < (a) && (b) <= (x))

#define MATTER_VEC 1
#define MATTER_MATC 2
#define MATTER_MATR 3

#define NONE -99

typedef long index_type;
typedef double dbl_index_type;

//// Delayed operations on atoms
//-------------------------------

struct Scaled {
    SEXP scale;
    SEXP center;
};

typedef struct Scaled Scaled;

enum Ops_t { none, scalar, vector };

struct Ops {
    Ops_t center_t;
    double * center;
    Ops_t scale_t;
    double * scale; };

typedef struct Ops Ops;

Ops null_transform();

template<typename T>
T transform(T x, Ops o, int i) {
    switch(o.center_t) {
        case none:
            break;
        case scalar:
            x -= (*o.center);
            break;
        case vector:
            x -= o.center[i];
            break;
    }
    switch(o.scale_t) {
        case none:
            break;
        case scalar:
            x /= (*o.scale);
            break;
        case vector:
            x /= o.scale[i];
            break;
    }
    return x;
}

template<typename T>
T backtransform(T x, Ops o, int i) {
    switch(o.scale_t) {
        case none:
            break;
        case scalar:
            x *= (*o.scale);
            break;
        case vector:
            x *= o.scale[i];
            break;
    }
    switch(o.center_t) {
        case none:
            break;
        case scalar:
            x += (*o.center);
            break;
        case vector:
            x += o.center[i];
            break;
    }
    return x;
}

//// Low-level utility functions
//----------------------------------

template<typename T1, typename T2>
T2 coerce_cast(T1 x);

template<typename RType>
void fillNA(RType * ptr, size_t count, size_t skip = 1) {
    for ( size_t i = 0; i < count; i ++ ) {
        *ptr = DataNA<RType>();
        ptr += skip;
    }
}

//// Count # of consecutive indices after current one (for faster reads)
//----------------------------------------------------------------------

index_type num_consecutive(double * pindex, long i, long length);

//// DataSources class
//----------------

class DataSources {

    public:

        DataSources(SEXP x)
        {
            _paths = GET_SLOT(x, mkString("paths"));
            _filemode = GET_SLOT(x, mkString("filemode"));
            if ( LENGTH(_paths) == 0 )
                error("empty 'paths'");
            _length = LENGTH(_paths);
            _streams = (FILE **) Calloc(_length, FILE*);
            for ( int i = 0; i < _length; i++ )
                _streams[i] = NULL;
        }

        ~DataSources()
        {
            for ( int i = 0; i < _length; i++ )
                if ( _streams[i] != NULL )
                    fclose(_streams[i]);
            Free(_streams);
        }

        FILE * require(int source_id) {
            if ( source_id == NA_INTEGER )
                error("missing 'source_id'");
            if ( _streams[source_id] == NULL ) {
                const char * filename = CHARACTER_VALUE(STRING_ELT(_paths, source_id));
                _streams[source_id] = fopen(filename, CHARACTER_VALUE(_filemode));
                if ( _streams[source_id] == NULL )
                  error("could not open file '%s'", filename);
            }
            return _streams[source_id];
        }

    protected:

        SEXP _paths;
        SEXP _filemode;
        FILE ** _streams;
        int _length;

};


//// Atoms class
//-----------------------

class Atoms {

    public:

        Atoms(SEXP x, DataSources * s, Ops o) : _sources(s), _ops(o)
        {
            _length = INTEGER_VALUE(GET_SLOT(x, mkString("length")));
            _source_id = INTEGER(GET_SLOT(x, mkString("source_id")));
            _datamode = INTEGER(GET_SLOT(x, mkString("datamode")));
            _offset = REAL(GET_SLOT(x, mkString("offset")));
            _extent = REAL(GET_SLOT(x, mkString("extent")));
            _index_offset = REAL(GET_SLOT(x, mkString("index_offset")));
            _index_extent = REAL(GET_SLOT(x, mkString("index_extent")));
        }

        ~Atoms(){}

        int source_id(int i) {
            int retId = _source_id[i] - 1;
            if ( retId == NA_INTEGER )
                error("missing 'source_id'");
            return retId;
        }

        int datamode(int i) {
            return _datamode[i];
        }

        index_type offset(int i) {
            return static_cast<index_type>(_offset[i]);
        }

        index_type extent(int i) {
            return static_cast<index_type>(_extent[i]);
        }

        index_type index_offset(int i) {
            return static_cast<index_type>(_index_offset[i]);
        }

        index_type index_extent(int i) {
            return static_cast<index_type>(_index_extent[i]);
        }

        int length() {
            return _length;
        }

        index_type max_extent() {
            return(index_extent(length() - 1));
        }

        index_type byte_offset(int i, index_type offset) {
            index_type byte_offset, elt_offset;
            switch(datamode(i)) {
                case 1:
                    elt_offset = sizeof(short) * (offset - index_offset(i));
                    break;
                case 2:
                    elt_offset = sizeof(int) * (offset - index_offset(i));
                    break;
                case 3:
                    elt_offset = sizeof(long) * (offset - index_offset(i));
                    break;
                case 4:
                    elt_offset = sizeof(float) * (offset - index_offset(i));
                    break;
                case 5:
                    elt_offset = sizeof(double) * (offset - index_offset(i));
                    break;
                default:
                    error("unsupported datamode");
            }
            byte_offset = this->offset(i) + elt_offset;
            return byte_offset;
        }

        int find_atom(index_type offset) {
            for ( int retIdx = 0; retIdx < length(); retIdx++ )
                if ( within_bounds(offset, index_offset(retIdx), index_extent(retIdx)) )
                    return retIdx;
            error("subscript out of bounds");
        }

        template<typename CType, typename RType>
        index_type read_atom(RType * ptr, int which, index_type offset, index_type count, size_t skip = 1) {
            index_type numRead;
            FILE * stream = _sources->require(source_id(which));
            fseek(stream, byte_offset(which, offset), SEEK_SET);
            CType * tmp = (CType *) Calloc(count, CType);
            numRead = fread(tmp, sizeof(CType), count, stream);
            for ( index_type i = 0; i < numRead; i++ ) {
                *ptr = transform<RType>(coerce_cast<CType,RType>(tmp[i]), _ops, offset + i);
                ptr += skip;
            }
            Free(tmp);
            return numRead;
        }

        template<typename CType, typename RType>
        index_type write_atom(RType * ptr, int which, index_type offset, index_type count, size_t skip = 1) {
            index_type numWrote;
            FILE * stream = _sources->require(source_id(which));
            fseek(stream, byte_offset(which, offset), SEEK_SET);
            CType * tmp = (CType *) Calloc(count, CType);
            for ( index_type i = 0; i < count; i++ ) {
                tmp[i] = backtransform<RType>(coerce_cast<RType,CType>(*ptr), _ops, offset + i);
                ptr += skip;
            }
            numWrote = fwrite(tmp, sizeof(CType), count, stream);
            Free(tmp);
            return numWrote;
        }

        template<typename RType>
        index_type read(RType * ptr, index_type offset, index_type count, size_t skip = 1) {
            index_type toRead, numRead, totLength;
            toRead = count;
            numRead = 0;
            totLength = index_extent(length() - 1);
            if ( offset < 0 || offset + count > totLength )
                error("subscript out of bounds");
            while ( numRead < count && offset < totLength ) {
                int i = find_atom(offset);
                index_type n = toRead < extent(i) ? toRead : extent(i);
                switch(datamode(i)) {
                    case 1:
                        n = read_atom<short,RType>(ptr, i, offset, n, skip);
                        break;
                    case 2:
                        n = read_atom<int,RType>(ptr, i, offset, n, skip);
                        break;
                    case 3:
                        n = read_atom<long,RType>(ptr, i, offset, n, skip);
                        break;
                    case 4:
                        n = read_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case 5:
                        n = read_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        error("unsupported datamode");
                }
                toRead -= n;
                numRead += n;
                ptr += n;
                offset += n;
            }
            return numRead;
        }

        template<typename RType>
        index_type write(RType * ptr, index_type offset, index_type count, size_t skip = 1) {
            index_type toWrite, numWrote, totLength;
            toWrite = count;
            numWrote = 0;
            totLength = index_extent(length() - 1);
            if ( offset < 0 || offset + count > totLength )
                error("subscript out of bounds");
            while ( numWrote < count && offset < totLength ) {
                int i = find_atom(offset);
                index_type n = toWrite < extent(i) ? toWrite : extent(i);
                switch(datamode(i)) {
                    case 1:
                        n = write_atom<short,RType>(ptr, i, offset, n, skip);
                        break;
                    case 2:
                        n = write_atom<int,RType>(ptr, i, offset, n, skip);
                        break;
                    case 3:
                        n = write_atom<long,RType>(ptr, i, offset, n, skip);
                        break;
                    case 4:
                        n = write_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case 5:
                        n = write_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        error("unsupported datamode");
                }
                toWrite -= n;
                numWrote += n;
                ptr += n;
                offset += n;
            }
            return numWrote;
        }

        template<typename RType>
        index_type read_indices(RType * ptr, dbl_index_type * pindex, long length, size_t skip = 1) {
            index_type numRead;
            for ( long i = 0; i < length; i++ ) {
                if ( ISNA(pindex[i]) ) {
                    ptr[skip * i] = DataNA<RType>();
                    continue;
                }
                index_type nx = num_consecutive(pindex, i, length);
                if ( nx >= 0 ) {
                    index_type count = nx + 1;
                    index_type offset = static_cast<index_type>(pindex[i]);
                    numRead = read<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_type count = (-nx) + 1;
                    index_type offset = static_cast<index_type>(pindex[i + (-nx)]);
                    numRead = read<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
                }
                i += labs(nx);
            }
            return numRead;
        }

        template<typename RType>
        index_type write_indices(RType * ptr, dbl_index_type * pindex, long length, size_t skip = 1) {
            index_type numWrote;
            for ( long i = 0; i < length; i++ ) {
                if ( ISNA(pindex[i]) ) {
                    continue;
                }
                index_type nx = num_consecutive(pindex, i, length);
                if ( nx >= 0 ) {
                    index_type count = nx + 1;
                    index_type offset = static_cast<index_type>(pindex[i]);
                    numWrote = write<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_type count = (-nx) + 1;
                    index_type offset = static_cast<index_type>(pindex[i + (-nx)]);
                    numWrote = write<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
                }
                i += labs(nx);
            }
            return numWrote;
        }

    protected:

        int * _source_id;    // index from 1
        int * _datamode;   // 1 = short, 2 = int, 3 = index_type, 4 = float, 5 = double
        double * _offset;
        double * _extent;
        double * _index_offset; // index from 0
        double * _index_extent; // index from 0
        int _length;

        DataSources * _sources;
        Ops _ops;

};


//// Matter class
//----------------

class Matter
{

    public:

        Matter(SEXP x) : _sources(x)
        {
            _data = GET_SLOT(x, mkString("data"));
            _datamode = INTEGER_VALUE(GET_SLOT(x, mkString("datamode")));
            _chunksize = INTEGER_VALUE(GET_SLOT(x, mkString("chunksize")));
            _length = static_cast<index_type>(NUMERIC_VALUE(GET_SLOT(x, mkString("length"))));
            _dim = GET_SLOT(x, mkString("dim"));
            const char * S4class = CHARACTER_VALUE(GET_CLASS(x));
            if ( strcmp(S4class, "matter_vec") == 0 )
                _S4class = MATTER_VEC;
            else if ( strcmp(S4class, "matter_matc") == 0 )
                _S4class = MATTER_MATC;
            else if ( strcmp(S4class, "matter_matr") == 0 )
                _S4class = MATTER_MATR;
            const char * attr_center = "scaled:center";
            _scaled.center = GET_ATTR(x, install(attr_center));
            const char * attr_scale = "scaled:scale";
            _scaled.scale = GET_ATTR(x, install(attr_scale));
        }

        ~Matter(){}

        SEXP data() {
            return _data;
        }

        SEXP data(int i) {
            if ( i < 0 || LENGTH(_data) <= i )
                error("subscript out of bounds");
            return VECTOR_ELT(_data, i);
        }

        int data_n() {
            switch(S4class()) {
                case MATTER_MATC:
                    return ncols();
                case MATTER_MATR:
                    return nrows();
            }
            return 0;
        }

        int datamode() {
            return _datamode;
        }

        DataSources * sources() {
            return &_sources;
        }

        int chunksize() {
            return _chunksize;
        }

        index_type length() {
            return _length;
        }

        int dim(int i) {
            return INTEGER(_dim)[i];
        }

        int dim_n() {
            return LENGTH(_dim);
        }

        int nrows() {
            if ( dim_n() == 2 )
                return dim(0);
            else
                return 0;
        }

        int ncols() {
            if ( dim_n() == 2 )
                return dim(1);
            else
                return 0;
        }

        int S4class() {
            return _S4class;
        }

        Ops ops() {
            return null_transform();
        }

        Ops ops(int i) {
            Ops retOps = null_transform();
            switch(S4class()) {
                case MATTER_MATC:
                    if ( _scaled.center != R_NilValue )
                    {
                        retOps.center_t = scalar;
                        retOps.center = REAL(_scaled.center) + i;
                    }
                    if ( _scaled.scale != R_NilValue )
                    {
                        retOps.scale_t = scalar;
                        retOps.scale = REAL(_scaled.scale) + i;
                    }
                    break;
                case MATTER_MATR:
                    if ( _scaled.center != R_NilValue )
                    {
                        retOps.center_t = vector;
                        retOps.center = REAL(_scaled.center);
                    }
                    if ( _scaled.scale != R_NilValue )
                    {
                        retOps.scale_t = vector;
                        retOps.scale = REAL(_scaled.scale);
                    }
                    break;
            }
            return retOps;
        }

        template<typename RType>
        SEXP readVector();

        template<typename RType>
        void writeVector(SEXP value);

        template<typename RType>
        SEXP readVectorElements(SEXP i);

        template<typename RType>
        void writeVectorElements(SEXP i, SEXP value);

        template<typename RType>
        SEXP readMatrix();

        template<typename RType>
        void writeMatrix(SEXP value);

        template<typename RType>
        SEXP readMatrixRows(SEXP i);

        template<typename RType>
        void writeMatrixRows(SEXP i, SEXP value);

        template<typename RType>
        SEXP readMatrixCols(SEXP j);

        template<typename RType>
        void writeMatrixCols(SEXP j, SEXP value);

        template<typename RType>
        SEXP readMatrixElements(SEXP i, SEXP j);

        template<typename RType>
        void writeMatrixElements(SEXP i, SEXP j, SEXP value);

        template<typename RType>
        SEXP rmult(SEXP y);

        template<typename RType>
        SEXP lmult(SEXP x);

        SEXP sum(bool na_rm = false);

        SEXP mean(bool na_rm = false);

        SEXP var(bool na_rm = false);

        SEXP colsums(bool na_rm = false);

        SEXP rowsums(bool na_rm = false);

        SEXP colmeans(bool na_rm = false);

        SEXP rowmeans(bool na_rm = false);

        SEXP colvar(bool na_rm = false);

        SEXP rowvar(bool na_rm = false);

    protected:

        SEXP _data;     // EITHER "atoms" OR a *list* of "atoms"
        int _datamode;  // 1 = integer, 2 = numeric
        DataSources _sources;
        int _chunksize;
        index_type _length;
        SEXP _dim;
        int _S4class;      // 1 = vector, 2 = col-matrix, 3 = row-matrix
        Scaled _scaled;

};


//// MatterAccessor class
//-----------------------

template<typename RType>
class MatterAccessor
{

    public:

        MatterAccessor(Matter & x) : _matter(x)
        {
            switch(x.S4class()) {
                case 1:
                    _atoms = new Atoms(x.data(), x.sources(), x.ops());
                    _next = NONE;
                    break;
                default:
                    _atoms = new Atoms(x.data(0), x.sources(), x.ops(0));
                    _next = 1;
                    break;
            }
            init();
        }

        MatterAccessor(Matter & x, int i) : _matter(x)
        {
            _atoms = new Atoms(x.data(i), x.sources(), x.ops(i));
            _next = NONE;
            init();
        }

        int init() {
            _chunksize = _atoms->max_extent() < _matter.chunksize() ? 
                _atoms->max_extent() : _matter.chunksize();
            _current = 0;
            _lower = 0;
            _upper = _chunksize - 1;
            _buffer = (RType *) Calloc(_chunksize, RType);
            return next_chunk();
        }

        ~MatterAccessor()
        {
            delete _atoms;
            Free(_buffer);
        }

        int next_chunk() {
            if ( _current < _atoms->max_extent() )
            {
                int count;
                if ( _current + _chunksize > _atoms->max_extent() )
                    count = _atoms->max_extent() - _current;
                else
                    count = _chunksize;
                if ( count > 0 )
                {
                    _lower = _current;
                    _upper = _current + count - 1;
                    return _atoms->read<RType>(_buffer, _current, count);
                }    
            }
            else if ( 0 <= _next && _next < _matter.data_n() )
            {
                delete _atoms;
                _atoms = new Atoms(_matter.data(_next), _matter.sources(), _matter.ops(_next));
                _next++;
                return init();
            }
            return 0;
        }

        RType operator*() { 
            return _buffer[_current % _chunksize];
        }

        MatterAccessor<RType> & operator++() {
            _current++;
            if ( _current > _upper )
                next_chunk();
            return *this;
        }

        operator bool() {
            return (0 <= _current && _current < _atoms->max_extent() && 
                _lower <= _current && _current <= _upper);
        }

        bool operator !() {
            return !(0 <= _current && _current < _atoms->max_extent() && 
                _lower <= _current && _current <= _upper);
        }

    protected:
        Matter & _matter;
        Atoms * _atoms;
        int _next;
        int _chunksize;
        index_type _current;
        index_type _lower;
        index_type _upper;
        RType * _buffer;
};

double sum(MatterAccessor<double> & x, bool na_rm = false);

double mean(MatterAccessor<double> & x, bool na_rm = false);

double var(MatterAccessor<double> & x, bool na_rm = false);

//// Exported C functions
//-----------------------

extern "C" {

    SEXP getVector(SEXP x);

    void setVector(SEXP x, SEXP value);

    SEXP getVectorElements(SEXP x, SEXP i);

    void setVectorElements(SEXP x, SEXP i, SEXP value);

    SEXP getMatrix(SEXP x);

    void setMatrix(SEXP x, SEXP value);

    SEXP getMatrixRows(SEXP x, SEXP i);

    void setMatrixRows(SEXP x, SEXP i, SEXP value);

    SEXP getMatrixCols(SEXP x, SEXP j);

    void setMatrixCols(SEXP x, SEXP j, SEXP value);

    SEXP getMatrixElements(SEXP x, SEXP i, SEXP j);

    void setMatrixElements(SEXP x, SEXP i, SEXP j, SEXP value);

    SEXP getSum(SEXP x, SEXP na_rm);

    SEXP getMean(SEXP x, SEXP na_rm);

    SEXP getVar(SEXP x, SEXP na_rm);

    SEXP getColSums(SEXP x, SEXP na_rm);

    SEXP getColMeans(SEXP x, SEXP na_rm);

    SEXP getColVars(SEXP x, SEXP na_rm);

    SEXP getRowSums(SEXP x, SEXP na_rm);

    SEXP getRowMeans(SEXP x, SEXP na_rm);

    SEXP getRowVars(SEXP x, SEXP na_rm);

    SEXP rightMatrixMult(SEXP x, SEXP y);

    SEXP leftMatrixMult(SEXP x, SEXP y);

}

#endif

