
#ifndef MATTER
#define MATTER

#include <R.h>

#include <cstdio>
#include <cstdlib>

#include "utils.h"
#include "matterDefines.h"

#define within_bounds(x, a, b) ((a) <= (x) && (x) < (b))
#define out_of_bounds(x, a, b) ((x) < (a) && (b) <= (x))
#define is_equal_double(x, y) (fabs((x) - (y)) < 1E-9 || (ISNA(x) && ISNA(y)))

// Declare Atoms and Matter classes now since component classes need them
//----------------------------------

class Atoms;

class Matter;

//// Low-level utility functions
//----------------------------------

template<typename T>
T coerce_pow(T base, T exponent);

template<typename T>
T coerce_exp(T x);

template<typename T>
T coerce_log(T x);

template<typename T1, typename T2>
T2 coerce_cast(T1 x);

template<typename RType>
void fillNA(RType * ptr, size_t count, size_t skip = 1) {
    for ( size_t i = 0; i < count; i ++ ) {
        *ptr = DataNA<RType>();
        ptr += skip;
    }
}

//// Delta run length encoding 
//-----------------------------

index_t count_consecutive(double * pindex, long i, long length);

template<typename T>
T run_delta(T * values, int i, int n);

template<typename T>
int run_length(T * values, int i, int n, T delta);

template<typename T>
int count_runs(T * values, int n);

template<typename T>
SEXP makeDRLE(SEXP x, SEXP nruns);

template<typename RType, int SType>
class VectorOrDRLE {

    public:

        VectorOrDRLE(SEXP x)
        {
            if ( isS4(x) )
            {
                values = DataPtr<RType,SType>(GET_SLOT(x, install("values")));
                lengths = INTEGER(GET_SLOT(x, install("lengths")));
                deltas = DataPtr<RType,SType>(GET_SLOT(x, install("deltas")));
                nruns = LENGTH(GET_SLOT(x, install("values")));
                isDRLE = true;
            }
            else
            {
                values = DataPtr<RType,SType>(x);
                nruns = LENGTH(x);
                isDRLE = false;
            }
            ref_index = 0;
            ref_run = 0;
        }

        ~VectorOrDRLE(){}

        SEXP readVector();

        SEXP readVectorElements(SEXP i);

        int length();

        int find(RType value);

        RType operator[](int i);

    protected:

        RType * values;
        int * lengths;
        RType * deltas;
        int nruns;
        int ref_index;
        int ref_run;
        bool isDRLE;

};

template<>
int VectorOrDRLE<int,INTSXP> :: find(int value);

template<>
int VectorOrDRLE<double,REALSXP> :: find(double value);

//// DataSources class
//---------------------

class DataSources {

    public:

        DataSources(SEXP x)
        {
            _paths = GET_SLOT(x, install("paths"));
            _filemode = GET_SLOT(x, install("filemode"));
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

//// Delayed operations on atoms
//-------------------------------

union R_ANY {
    Rbyte * r;
    int * i;
    double * d;
    SEXP * s;
    Matter * m;
};

class Ops {

    public:

        Ops(SEXP x) {
            SEXP _x = GET_SLOT(x, install("ops"));
            if ( _x != R_NilValue ) {
                _length = LENGTH(_x);
                _lhs = Calloc(_length, SEXP);
                _rhs = Calloc(_length, SEXP);
                _op = Calloc(_length, int);
                _where = Calloc(_length, int);
                _type = Calloc(_length, SEXPTYPE);
                _arg = Calloc(_length, R_ANY);
                _arglengths = Calloc(_length, index_t);
                for ( int i = 0; i < _length; i++ ) {
                    SEXP _elt = VECTOR_ELT(_x, i);
                    _lhs[i] = VECTOR_ELT(_elt, 0);
                    _rhs[i] = VECTOR_ELT(_elt, 1);
                    _op[i] = INTEGER_VALUE(VECTOR_ELT(_elt, 2));
                    _where[i] = INTEGER_VALUE(VECTOR_ELT(_elt, 3));
                    if ( has_lhs(i) )
                        _type[i] = TYPEOF(lhs(i));
                    else if ( has_rhs(i) )
                        _type[i] = TYPEOF(rhs(i));
                    else
                        _type[i] = NILSXP;
                }
                init_args();
            }
            else
                _length = 0;
        }

        ~Ops() {
            if ( length() > 0 ) {
                finalize_args();
                Free(_lhs);
                Free(_rhs);
                Free(_op);
                Free(_where);
                Free(_type);
                Free(_arg);
                Free(_arglengths);
            }
        }

        void init_args();

        void finalize_args();

        int length() {
            return _length;
        }

        bool has_lhs(int i) {
            return lhs(i) != R_NilValue;
        }

        bool has_rhs(int i) {
            return rhs(i) != R_NilValue;
        }

        SEXP lhs(int i) {
            return _lhs[i];
        }

        SEXP rhs(int i) {
            return _rhs[i];
        }

        int op(int i) {
            return _op[i];
        }

        int where(int i) {
            return _where[i];
        }

        SEXPTYPE type(int i) {
            return _type[i];
        }

        index_t arglength(int i) {
            return _arglengths[i];
        }

        template<typename RType, int SType>
        RType * arg(int i);

        template<typename T1, typename T2>
        void add(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void sub(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void mul(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void div(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T>
        void exp(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void exp(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T>
        void log(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void log(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T>
        void do_ops(T * x, Atoms * atm, index_t offset, index_t count, size_t skip);

    protected:

        int _length;
        SEXP * _lhs;
        SEXP * _rhs;
        int * _op;
        int * _where;
        SEXPTYPE * _type;
        R_ANY * _arg;
        index_t * _arglengths;

};

//// Atoms class
//-----------------------

class Atoms {

    public:

        Atoms(SEXP x, DataSources & s, Ops & o) : _sources(s), _ops(o)
        {
            _natoms = INTEGER_VALUE(GET_SLOT(x, install("natoms")));
            _ngroups = INTEGER_VALUE(GET_SLOT(x, install("ngroups")));
            _group_id = new VectorOrDRLE<int,INTSXP>(GET_SLOT(x, install("group_id")));
            _source_id = new VectorOrDRLE<int,INTSXP>(GET_SLOT(x, install("source_id")));
            _datamode = new VectorOrDRLE<int,INTSXP>(GET_SLOT(x, install("datamode")));
            _offset = new VectorOrDRLE<double,REALSXP>(GET_SLOT(x, install("offset")));
            _extent = new VectorOrDRLE<double,REALSXP>(GET_SLOT(x, install("extent")));
            _index_offset = new VectorOrDRLE<double,REALSXP>(GET_SLOT(x, install("index_offset")));
            _index_extent = new VectorOrDRLE<double,REALSXP>(GET_SLOT(x, install("index_extent")));
            set_group(0);
        }

        ~Atoms()
        {
            delete _group_id;
            delete _source_id;
            delete _datamode;
            delete _offset;
            delete _extent;
            delete _index_offset;
            delete _index_extent;
        }

        int group() {
            return _group;
        }

        int group_offset() {
            return _group_offset;
        }

        int group_length() {
            return _group_length;
        }

        void set_group(int i) {
            if ( i > 0 && _group == i )
                return;
            if ( 0 <= i && i < length() )
            {
                _group = i;
                _group_offset = _group_id->find(i + 1);
                int j = _group_offset + 1;
                while ( j < _natoms && (*_group_id)[j] == _group + 1 )
                    j++;
                _group_length = j - _group_offset;
            }
            else
                error("subscript out of bounds");
        }

        int source_id(int i) {
            int retId = (*_source_id)[i + group_offset()] - 1;
            if ( retId == NA_INTEGER )
                error("missing 'source_id'");
            return retId;
        }

        int datamode(int i) {
            return (*_datamode)[i + group_offset()];
        }

        index_t offset(int i) {
            return static_cast<index_t>((*_offset)[i + group_offset()]);
        }

        index_t extent(int i) {
            return static_cast<index_t>((*_extent)[i + group_offset()]);
        }

        index_t index_offset(int i) {
            return static_cast<index_t>((*_index_offset)[i + group_offset()]);
        }

        index_t index_extent(int i) {
            return static_cast<index_t>((*_index_extent)[i + group_offset()]);
        }

        int length() {
            return _ngroups;
        }

        index_t max_extent() {
            return(index_extent(group_length() - 1));
        }

        index_t byte_offset(int i, index_t offset) {
            index_t byte_offset, elt_offset;
            switch(datamode(i)) {
                case C_CHAR:
                case C_UCHAR:
                    elt_offset = sizeof(char) * (offset - index_offset(i));
                    break;
                case C_SHORT:
                case C_USHORT:
                    elt_offset = sizeof(short) * (offset - index_offset(i));
                    break;
                case C_INT:
                case C_UINT:
                    elt_offset = sizeof(int) * (offset - index_offset(i));
                    break;
                case C_LONG:
                case C_ULONG:
                    elt_offset = sizeof(long) * (offset - index_offset(i));
                    break;
                case C_FLOAT:
                    elt_offset = sizeof(float) * (offset - index_offset(i));
                    break;
                case C_DOUBLE:
                    elt_offset = sizeof(double) * (offset - index_offset(i));
                    break;
                default:
                    error("unsupported datamode");
            }
            byte_offset = this->offset(i) + elt_offset;
            return byte_offset;
        }

        int find_atom(index_t offset) {
            for ( int retIdx = 0; retIdx < group_length(); retIdx++ )
                if ( within_bounds(offset, index_offset(retIdx), index_extent(retIdx)) )
                    return retIdx;
            error("subscript not found in any atom");
        }

        template<typename CType, typename RType>
        index_t read_atom(RType * ptr, int which, index_t offset, index_t count, size_t skip = 1) {
            index_t numRead;
            FILE * stream = _sources.require(source_id(which));
            fseek(stream, byte_offset(which, offset), SEEK_SET);
            CType * tmp = (CType *) Calloc(count, CType);
            RType * optr = ptr;
            numRead = fread(tmp, sizeof(CType), count, stream);
            for ( index_t i = 0; i < numRead; i++ ) {
                *ptr = coerce_cast<CType,RType>(tmp[i]);
                ptr += skip;
            }
            Free(tmp);
            _ops.do_ops<RType>(optr, this, offset, count, skip);
            return numRead;
        }

        template<typename CType, typename RType>
        index_t write_atom(RType * ptr, int which, index_t offset, index_t count, size_t skip = 1) {
            index_t numWrote;
            if ( _ops.length() > 0 )
                error("assignment not supported with delayed operations");
            FILE * stream = _sources.require(source_id(which));
            fseek(stream, byte_offset(which, offset), SEEK_SET);
            CType * tmp = (CType *) Calloc(count, CType);
            for ( index_t i = 0; i < count; i++ ) {
                tmp[i] = coerce_cast<RType,CType>(*ptr);
                ptr += skip;
            }
            numWrote = fwrite(tmp, sizeof(CType), count, stream);
            Free(tmp);
            return numWrote;
        }

        template<typename RType>
        index_t read(RType * ptr, index_t offset, index_t count, size_t skip = 1) {
            index_t toRead, numRead, totLength;
            toRead = count;
            numRead = 0;
            totLength = index_extent(group_length() - 1);
            if ( offset < 0 || offset + count > totLength )
                error("subscript out of bounds");
            while ( numRead < count && offset < totLength ) {
                int i = find_atom(offset);
                index_t n = toRead < extent(i) ? toRead : extent(i);
                switch(datamode(i)) {
                    case C_CHAR:
                        n = read_atom<char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UCHAR:
                        n = read_atom<unsigned char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_SHORT:
                        n = read_atom<short,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_USHORT:
                        n = read_atom<unsigned short,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_INT:
                        n = read_atom<int,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UINT:
                        n = read_atom<unsigned int,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_LONG:
                        n = read_atom<long,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_ULONG:
                        n = read_atom<unsigned long,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_FLOAT:
                        n = read_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_DOUBLE:
                        n = read_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        error("unsupported datamode");
                }
                toRead -= n;
                numRead += n;
                ptr += (n * skip);
                offset += n;
            }
            return numRead;
        }

        template<typename RType>
        index_t write(RType * ptr, index_t offset, index_t count, size_t skip = 1) {
            index_t toWrite, numWrote, totLength;
            toWrite = count;
            numWrote = 0;
            totLength = index_extent(group_length() - 1);
            if ( offset < 0 || offset + count > totLength )
                error("subscript out of bounds");
            while ( numWrote < count && offset < totLength ) {
                int i = find_atom(offset);
                index_t n = toWrite < extent(i) ? toWrite : extent(i);
                switch(datamode(i)) {
                    case C_CHAR:
                        n = write_atom<char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UCHAR:
                        n = write_atom<unsigned char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_SHORT:
                        n = write_atom<short,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_USHORT:
                        n = write_atom<unsigned short,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_INT:
                        n = write_atom<int,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UINT:
                        n = write_atom<unsigned int,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_LONG:
                        n = write_atom<long,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_ULONG:
                        n = write_atom<unsigned long,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_FLOAT:
                        n = write_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_DOUBLE:
                        n = write_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        error("unsupported datamode");
                }
                toWrite -= n;
                numWrote += n;
                ptr += (n * skip);
                offset += n;
            }
            return numWrote;
        }

        template<typename RType>
        index_t read_indices(RType * ptr, Rindex_t * pindex, long length, size_t skip = 1) {
            index_t numRead;
            for ( long i = 0; i < length; i++ ) {
                if ( ISNA(pindex[i]) ) {
                    ptr[skip * i] = DataNA<RType>();
                    continue;
                }
                index_t nx = count_consecutive(pindex, i, length);
                if ( nx >= 0 ) {
                    index_t count = nx + 1;
                    index_t offset = static_cast<index_t>(pindex[i]);
                    numRead = read<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_t count = (-nx) + 1;
                    index_t offset = static_cast<index_t>(pindex[i + (-nx)]);
                    numRead = read<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
                }
                i += labs(nx);
            }
            return numRead;
        }

        template<typename RType>
        index_t write_indices(RType * ptr, Rindex_t * pindex, long length, size_t skip = 1) {
            index_t numWrote;
            for ( long i = 0; i < length; i++ ) {
                if ( ISNA(pindex[i]) ) {
                    continue;
                }
                index_t nx = count_consecutive(pindex, i, length);
                if ( nx >= 0 ) {
                    index_t count = nx + 1;
                    index_t offset = static_cast<index_t>(pindex[i]);
                    numWrote = write<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_t count = (-nx) + 1;
                    index_t offset = static_cast<index_t>(pindex[i + (-nx)]);
                    numWrote = write<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
                }
                i += labs(nx);
            }
            return numWrote;
        }

    protected:

        int _natoms;
        int _ngroups;

        int _group;
        int _group_offset;
        int _group_length;

        VectorOrDRLE<int,INTSXP> * _group_id;     // index from 1
        VectorOrDRLE<int,INTSXP> * _source_id;    // index from 1
        VectorOrDRLE<int,INTSXP> * _datamode;
        VectorOrDRLE<double,REALSXP> * _offset;
        VectorOrDRLE<double,REALSXP> * _extent;
        VectorOrDRLE<double,REALSXP> * _index_offset; // index from 0
        VectorOrDRLE<double,REALSXP> * _index_extent; // index from 0

        DataSources & _sources;
        Ops & _ops;

};


//// Matter class
//----------------

class Matter
{

    public:

        Matter(SEXP x) : _sources(x), _ops(x)
        {
            _data = new Atoms(GET_SLOT(x, install("data")), _sources, _ops);
            _datamode = INTEGER_VALUE(GET_SLOT(x, install("datamode")));
            _chunksize = INTEGER_VALUE(GET_SLOT(x, install("chunksize")));
            _length = static_cast<index_t>(NUMERIC_VALUE(GET_SLOT(x, install("length"))));
            _dim = GET_SLOT(x, install("dim"));
            const char * classname = CHARACTER_VALUE(GET_CLASS(x));
            if ( strcmp(classname, "matter_vec") == 0 )
                _S4class = MATTER_VEC;
            else if ( strcmp(classname, "matter_matc") == 0 )
                _S4class = MATTER_MATC;
            else if ( strcmp(classname, "matter_matr") == 0 )
                _S4class = MATTER_MATR;
            else
                error("subclass not implemented yet");
            // const char * attr_center = "scaled:center";
            // _scaled.center = GET_ATTR(x, install(attr_center));
            // const char * attr_scale = "scaled:scale";
            // _scaled.scale = GET_ATTR(x, install(attr_scale));
        }

        ~Matter() {
            delete _data;
        }

        Atoms & data() {
            return (*_data);
        }

        int datamode() {
            return _datamode;
        }

        DataSources & sources() {
            return _sources;
        }

        int chunksize() {
            return _chunksize;
        }

        index_t length() {
            return _length;
        }

        int dim(int i) {
            return INTEGER(_dim)[i];
        }

        int dim_length() {
            return LENGTH(_dim);
        }

        int nrows() {
            if ( dim_length() == 2 )
                return dim(0);
            else
                return 0;
        }

        int ncols() {
            if ( dim_length() == 2 )
                return dim(1);
            else
                return 0;
        }

        int S4class() {
            return _S4class;
        }

        Ops & ops() {
            return _ops;
        }

        template<typename RType, int SType>
        SEXP readVector();

        template<typename RType, int SType>
        void writeVector(SEXP value);

        template<typename RType, int SType>
        SEXP readVectorElements(SEXP i);

        template<typename RType, int SType>
        void writeVectorElements(SEXP i, SEXP value);

        template<typename RType, int SType>
        SEXP readMatrix();

        template<typename RType, int SType>
        void writeMatrix(SEXP value);

        template<typename RType, int SType>
        SEXP readMatrixRows(SEXP i);

        template<typename RType, int SType>
        void writeMatrixRows(SEXP i, SEXP value);

        template<typename RType, int SType>
        SEXP readMatrixCols(SEXP j);

        template<typename RType, int SType>
        void writeMatrixCols(SEXP j, SEXP value);

        template<typename RType, int SType>
        SEXP readMatrixElements(SEXP i, SEXP j);

        template<typename RType, int SType>
        void writeMatrixElements(SEXP i, SEXP j, SEXP value);

        template<typename RType, int SType>
        SEXP rmult(SEXP y);

        template<typename RType, int SType>
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

        Atoms * _data;     // EITHER "atoms" OR a *list* of "atoms"
        int _datamode;  // 1 = integer, 2 = numeric
        DataSources _sources;
        Ops _ops;
        int _chunksize;
        index_t _length;
        SEXP _dim;
        int _S4class;      // 1 = vector, 2 = col-matrix, 3 = row-matrix
        // Scaled _scaled;

};


//// MatterIterator class
//-----------------------

template<typename RType>
class MatterIterator
{

    public:

        MatterIterator(Matter & x) : _x(x)
        {
            switch(x.S4class()) {
                case MATTER_VEC:
                    _next = NULL_INDEX;
                    break;
                case MATTER_MATC:
                    _next = 1;
                    break;
                case MATTER_MATR:
                    _next = 1;
                    break;
            }
            init();
        }

        MatterIterator(Matter & x, int i) : _x(x)
        {
            _x.data().set_group(i);
            _next = NULL_INDEX;
            init();
        }

        int init() {
            _buffersize = _x.data().max_extent() < _x.chunksize() ? 
                _x.data().max_extent() : _x.chunksize();
            _buffer = (RType *) Calloc(_buffersize, RType);
            _current = 0;
            _lower = 0;
            _upper = _buffersize - 1;
            return next_chunk();
        }

        ~MatterIterator()
        {
            Free(_buffer);
        }

        int next_chunk() {
            if ( _current < _x.data().max_extent() )
            {
                int count;
                if ( _current + _buffersize > _x.data().max_extent() )
                    count = _x.data().max_extent() - _current;
                else
                    count = _buffersize;
                if ( count > 0 )
                {
                    _lower = _current;
                    _upper = _current + count - 1;
                    return _x.data().template read<RType>(_buffer, _current, count);
                }    
            }
            else if ( 0 <= _next && _next < _x.data().length() )
            {
                _x.data().set_group(_next);
                _next++;
                return init();
            }
            return 0;
        }

        RType operator*() { 
            return _buffer[_current % _buffersize];
        }

        MatterIterator<RType> & operator++() {
            _current++;
            if ( _current > _upper )
                next_chunk();
            return *this;
        }

        operator bool() {
            return (0 <= _current && _current < _x.data().max_extent() && 
                _lower <= _current && _current <= _upper);
        }

        bool operator !() {
            return !(0 <= _current && _current < _x.data().max_extent() && 
                _lower <= _current && _current <= _upper);
        }

    protected:
        Matter & _x;
        int _next;
        int _buffersize;
        index_t _current;
        index_t _lower;
        index_t _upper;
        RType * _buffer;
};

double sum(MatterIterator<double> & x, bool na_rm = false);

double mean(MatterIterator<double> & x, bool na_rm = false);

double var(MatterIterator<double> & x, bool na_rm = false);

//// Exported C functions
//-----------------------

extern "C" {

    SEXP createAtoms(
        SEXP group_id,
        SEXP source_id,
        SEXP datamode,
        SEXP offset,
        SEXP extent
    );

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

    SEXP countRuns(SEXP x);

    SEXP createDRLE(SEXP x, SEXP nruns);

    SEXP getDRLE(SEXP x);

    SEXP getDRLEElements(SEXP x, SEXP i);

}

#endif

