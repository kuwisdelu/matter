
#ifndef MATTER
#define MATTER

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <climits>
#include <cmath>

#include "utils.h"
#include "matterDefines.h"

#include "search.h"

#include <ios>
#include <fstream>

using std::ios;
using std::fstream;

#define within_bounds(x, a, b) ((a) <= (x) && (x) < (b))
#define out_of_bounds(x, a, b) ((x) < (a) && (b) <= (x))
#define is_equal_double(x, y) (fabs((x) - (y)) < DBL_EPSILON || (ISNA(x) && ISNA(y)))

// Declare Atoms and Matter classes now since component classes need them
//----------------------------------

class Atoms;

class Matter;

//// Low-level utility functions
//----------------------------------

void set_matter_options();

int coerce_logical(Rbyte x);

int coerce_logical(int x);

int coerce_logical(double x);

template<typename T>
T coerce_pow(T base, T exponent);

template<typename T>
T coerce_exp(T x);

template<typename T1, typename T2>
T1 coerce_mod(T1 x, T2 y);

template<typename T1, typename T2>
T1 coerce_idiv(T1 x, T2 y);

template<typename T>
T coerce_log(T x);

template<typename T1, typename T2>
T2 coerce_cast(T1 x);

template<typename RType>
void fillNA(RType * ptr, size_t count, size_t skip = 1) {
    for ( size_t i = 0; i < count; i ++ ) {
        *ptr = NA<RType>();
        ptr += skip;
    }
}

//// Raw string encoding
//-----------------------

SEXP raw_to_char(Rbyte * x, int length);

SEXP raw_to_char(SEXP x);

//// Delta run length encoding 
//-----------------------------

template<typename T>
index_t count_consecutive(T * pindex, size_t i, size_t length)
{
    index_t n = 0;
    if ( isNA(pindex[i + 1]) )
        return n;
    if ( i < length - 1 && pindex[i + 1] > pindex[i] ) {
        while ( i < length - 1 && !isNA(pindex[i + 1]) && 
            static_cast<index_t>(pindex[i + 1] - pindex[i]) == 1 )
        {
            i++;
            n++;
        }
        return n;
    }
    else if ( i < length - 1 && pindex[i + 1] < pindex[i] ) {
        while ( i < length - 1 && !isNA(pindex[i + 1]) && 
            static_cast<index_t>(pindex[i + 1] - pindex[i]) == -1 )
        {
            i++;
            n--;
        }
        return n;
    }
    else
        return n;
}

template<typename T>
T run_delta(T * values, int i, int n);

template<typename T>
int run_length(T * values, int i, int n, T delta);

template<typename T>
int count_runs(T * values, int n, bool delta);

template<typename T>
SEXP makeDRLE(SEXP x, SEXP nruns, bool delta);

template<typename RType, int SType>
class VectorOrDRLE {

    public:

        VectorOrDRLE(SEXP x)
        {
            if ( Rf_isS4(x) )
            {
                values = DataPtr<RType>(R_do_slot(x, Rf_install("values")));
                lengths = INTEGER(R_do_slot(x, Rf_install("lengths")));
                deltas = DataPtr<RType>(R_do_slot(x, Rf_install("deltas")));
                nruns = LENGTH(R_do_slot(x, Rf_install("values")));
                isDRLE = true;
            }
            else
            {
                values = DataPtr<RType>(x);
                nruns = LENGTH(x);
                isDRLE = false;
            }
            ref_index = 0;
            ref_run = 0;
        }

        ~VectorOrDRLE(){}

        SEXP decode();

        SEXP decodeElements(SEXP i);

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
            _paths = R_do_slot(x, Rf_install("paths"));
            _filemode = Rf_asInteger(R_do_slot(x, Rf_install("filemode")));
            switch (_filemode) {
                case READ_ONLY:
                    _openmode = ios::in | ios::binary;
                    break;
                case WRITE_ONLY:
                case READ_WRITE:
                    _openmode = ios::in | ios::out | ios::binary;
                    break;
                default:
                    Rf_error("unrecognized 'filemode'");
            }
            if ( LENGTH(_paths) == 0 )
                Rf_error("empty 'paths'");
            _length = LENGTH(_paths);
            _current = 0;
            _streams = (fstream **) Calloc(_length, fstream*);
            for ( int i = 0; i < _length; i++ )
                _streams[i] = NULL;
        }

        ~DataSources()
        {
            for ( int i = 0; i < _length; i++ )
                if ( _streams[i] != NULL ) {
                    _streams[i]->close();
                    delete _streams[i];
                }
            Free(_streams);
        }

        fstream * select(int source_id)
        {
            if ( source_id == NA_INTEGER )
                Rf_error("missing 'source_id'");
            _current = source_id;
            if ( _streams[_current] == NULL ) {
                const char * filename = CHAR(Rf_asChar(STRING_ELT(_paths, _current)));
                _streams[_current] = new fstream();
                _streams[_current]->open(filename, _openmode);
                if ( !_streams[_current]->is_open() )
                  Rf_error("could not open file '%s'", filename);
            }
            return _streams[_current];
        }

        void rseek(int source_id, size_t offset = 0)
        {
            select(source_id)->seekg(offset, ios::beg);
        }

        void wseek(int source_id, size_t offset = 0)
        {
            select(source_id)->seekp(offset, ios::beg);
        }

        bool read(void * ptr, size_t size, size_t count)
        {
            if ( _filemode == WRITE_ONLY )
                Rf_error("'filemode' is write only");
            fstream * stream = _streams[_current];
            stream->read(reinterpret_cast<char*>(ptr), size * count);
            return !stream->fail();
        }

        bool write(void * ptr, size_t size, size_t count)
        {
            if ( _filemode == READ_ONLY )
                Rf_error("'filemode' is read only");
            fstream * stream = _streams[_current];
            stream->write(reinterpret_cast<char*>(ptr), size * count);
            return !stream->fail();
        }

    protected:

        SEXP _paths;
        int _filemode;
        ios::openmode _openmode;
        fstream ** _streams;
        int _current;
        int _length;

};

//// Delayed operations on atoms
//-------------------------------

union R_ANY {
    Rbyte * r;
    int * i;
    double * d;
    SEXP * s;
    Atoms * a;
    Matter * m;
};

struct R_OP_MODES {
    int * R_mode;     // the mode of the resulting R object
    bool is_list;  // lists have multiple R-level datamodes
};

class Ops {

    public:

        Ops(SEXP x) {
            SEXP _datamode = R_do_slot(x, Rf_install("datamode"));
            if ( LENGTH(_datamode) > 1 )
            {
                _modes.R_mode = INTEGER(_datamode);
                _modes.is_list = true;
            }
            else
            {
                _modes.R_mode = INTEGER(_datamode);
                _modes.is_list = false;
            }
            SEXP _x = R_do_slot(x, Rf_install("ops"));
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
                    _op[i] = Rf_asInteger(VECTOR_ELT(_elt, 2));
                    _where[i] = Rf_asInteger(VECTOR_ELT(_elt, 3));
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

        int result_type() {
            return _modes.R_mode[0];
        }

        int result_type(int i) {
            if ( _modes.is_list )
                return _modes.R_mode[i];
            else
                return _modes.R_mode[0];
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

        // Arith

        template<typename T1, typename T2>
        void add(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void sub(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void mul(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void div(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void mod(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void idiv(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T>
        void exp(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void exp(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T>
        void log(T * x, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void log(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        // Compare

        template<typename T1, typename T2>
        void eq(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void ne(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void gt(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void lt(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void ge(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void le(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        // Logic

        template<typename T1, typename T2>
        void AND(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        template<typename T1, typename T2>
        void OR(T1 * x, T2 * y, int i, Atoms * atm, index_t offset, index_t count, size_t skip);

        // Perform delayed operations

        template<typename T>
        void do_ops(T * x, Atoms * atm, index_t offset, index_t count, size_t skip = 1);

    protected:

        int _length;
        R_OP_MODES _modes;
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
            _natoms = Rf_asInteger(R_do_slot(x, Rf_install("natoms")));
            _ngroups = Rf_asInteger(R_do_slot(x, Rf_install("ngroups")));
            _group_id = new VectorOrDRLE<int,INTSXP>(R_do_slot(x, Rf_install("group_id")));
            _source_id = new VectorOrDRLE<int,INTSXP>(R_do_slot(x, Rf_install("source_id")));
            _datamode = new VectorOrDRLE<int,INTSXP>(R_do_slot(x, Rf_install("datamode")));
            _offset = new VectorOrDRLE<double,REALSXP>(R_do_slot(x, Rf_install("offset")));
            _extent = new VectorOrDRLE<double,REALSXP>(R_do_slot(x, Rf_install("extent")));
            _index_offset = new VectorOrDRLE<double,REALSXP>(R_do_slot(x, Rf_install("index_offset")));
            _index_extent = new VectorOrDRLE<double,REALSXP>(R_do_slot(x, Rf_install("index_extent")));
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
                Rf_error("subscript out of bounds");
        }

        int source_id(int i) {
            int retId = (*_source_id)[i + group_offset()] - 1;
            if ( retId == NA_INTEGER )
                Rf_error("missing 'source_id'");
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
                    elt_offset = sizeof(int16_t) * (offset - index_offset(i));
                    break;
                case C_INT:
                case C_UINT:
                    elt_offset = sizeof(int32_t) * (offset - index_offset(i));
                    break;
                case C_LONG:
                case C_ULONG:
                    elt_offset = sizeof(int64_t) * (offset - index_offset(i));
                    break;
                case C_FLOAT:
                    elt_offset = sizeof(float) * (offset - index_offset(i));
                    break;
                case C_DOUBLE:
                    elt_offset = sizeof(double) * (offset - index_offset(i));
                    break;
                default:
                    Rf_error("unsupported datamode");
            }
            byte_offset = this->offset(i) + elt_offset;
            return byte_offset;
        }

        int find_atom(index_t offset) {
            for ( int retIdx = 0; retIdx < group_length(); retIdx++ )
                if ( within_bounds(offset, index_offset(retIdx), index_extent(retIdx)) )
                    return retIdx;
            Rf_error("subscript not found in any atom");
        }

        template<typename CType, typename RType, typename IType>
        void apply_delayed_ops(RType * ptr, CType * buffer, index_t offset, index_t count, size_t skip) {
            if ( _ops.result_type(group()) == R_LOGICAL )
            {
                if ( _ops.length() == 0 )
                {
                    for ( index_t i = 0; i < count; i++ ) {
                        *ptr = coerce_logical(coerce_cast<CType,RType>(buffer[i]));
                        ptr += skip;
                    }
                }
                else
                {
                    IType * tmp = (IType *) Calloc(count, IType);
                    for ( index_t i = 0; i < count; i++ )
                        tmp[i] = coerce_cast<CType,IType>(buffer[i]);
                    _ops.do_ops<IType>(tmp, this, 0, count);
                    for ( index_t i = 0; i < count; i++ ) {
                        *ptr = static_cast<RType>(tmp[i]);
                        ptr += skip;
                    }
                    Free(tmp);
                }
            }
            else
            {
                RType * optr = ptr;
                for ( index_t i = 0; i < count; i++ ) {
                    *ptr = coerce_cast<CType,RType>(buffer[i]);
                    ptr += skip;
                }
                _ops.do_ops<RType>(optr, this, offset, count, skip);
            }
        }

        template<typename CType, typename RType>
        index_t read_atom(RType * ptr, int which, index_t offset, index_t count, size_t skip = 1) {
            bool stream_ok;
            CType * tmp = (CType *) Calloc(count, CType);
            _sources.rseek(source_id(which), byte_offset(which, offset));
            stream_ok = _sources.read(tmp, sizeof(CType), count);
            if ( !stream_ok )
                Rf_error("failed to read data elements");
            switch(datamode(which)) {
                case C_CHAR:
                case C_UCHAR:
                    apply_delayed_ops<CType,RType,Rbyte>(ptr, tmp, offset, count, skip);
                    break;
                case C_SHORT:
                case C_USHORT:
                case C_INT:
                case C_UINT:
                    apply_delayed_ops<CType,RType,int>(ptr, tmp, offset, count, skip);
                    break;
                case C_LONG:
                case C_ULONG:
                case C_FLOAT:
                case C_DOUBLE:
                    apply_delayed_ops<CType,RType,double>(ptr, tmp, offset, count, skip);
                    break;
            }
            Free(tmp);
            return count;
        }

        template<typename CType, typename RType>
        index_t write_atom(RType * ptr, int which, index_t offset, index_t count, size_t skip = 1) {
            bool stream_ok;
            if ( _ops.length() > 0 )
                Rf_error("assignment not supported with delayed operations");
            CType * tmp = (CType *) Calloc(count, CType);
            for ( index_t i = 0; i < count; i++ ) {
                tmp[i] = coerce_cast<RType,CType>(*ptr);
                ptr += skip;
            }
            _sources.wseek(source_id(which), byte_offset(which, offset));
            stream_ok = _sources.write(tmp, sizeof(CType), count);
            Free(tmp);
            if ( !stream_ok )
                Rf_error("failed to write data elements");
            return count;
        }

        template<typename RType>
        index_t read(RType * ptr, index_t offset, index_t count, size_t skip = 1) {
            index_t toRead, numRead, totLength;
            toRead = count;
            numRead = 0;
            totLength = index_extent(group_length() - 1);
            if ( offset < 0 || offset + count > totLength )
                Rf_error("subscript out of bounds");
            while ( numRead < count && offset < totLength ) {
                int i = find_atom(offset);
                index_t maxReadable = index_extent(i) - offset;
                index_t n = toRead <= maxReadable ? toRead : maxReadable;
                switch(datamode(i)) {
                    case C_CHAR:
                        n = read_atom<char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UCHAR:
                        n = read_atom<unsigned char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_SHORT:
                        n = read_atom<int16_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_USHORT:
                        n = read_atom<uint16_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_INT:
                        n = read_atom<int32_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UINT:
                        n = read_atom<uint32_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_LONG:
                        n = read_atom<int64_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_ULONG:
                        n = read_atom<uint64_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_FLOAT:
                        n = read_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_DOUBLE:
                        n = read_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        Rf_error("unsupported datamode");
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
                Rf_error("subscript out of bounds");
            while ( numWrote < count && offset < totLength ) {
                int i = find_atom(offset);
                index_t maxWritable = index_extent(i) - offset;
                index_t n = toWrite < maxWritable ? toWrite : maxWritable;
                switch(datamode(i)) {
                    case C_CHAR:
                        n = write_atom<char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UCHAR:
                        n = write_atom<unsigned char,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_SHORT:
                        n = write_atom<int16_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_USHORT:
                        n = write_atom<uint16_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_INT:
                        n = write_atom<int32_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_UINT:
                        n = write_atom<uint32_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_LONG:
                        n = write_atom<int64_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_ULONG:
                        n = write_atom<uint64_t,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_FLOAT:
                        n = write_atom<float,RType>(ptr, i, offset, n, skip);
                        break;
                    case C_DOUBLE:
                        n = write_atom<double,RType>(ptr, i, offset, n, skip);
                        break;
                    default:
                        Rf_error("unsupported datamode");
                }
                toWrite -= n;
                numWrote += n;
                ptr += (n * skip);
                offset += n;
            }
            return numWrote;
        }

        template<typename RType, typename IType>
        index_t read_indices(RType * ptr, IType * pindex, size_t length, size_t skip = 1, int zindex = 0) {
            index_t numRead = 0;
            for ( size_t i = 0; i < length; i++ ) {
                if ( isNA(pindex[i]) ) {
                    ptr[skip * i] = NA<RType>();
                    continue;
                }
                index_t nx = count_consecutive<IType>(pindex, i, length);
                if ( nx >= 0 ) {
                    index_t count = nx + 1;
                    index_t offset = static_cast<index_t>(pindex[i] - zindex);
                    numRead += read<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_t count = (-nx) + 1;
                    index_t offset = static_cast<index_t>(pindex[i + (-nx)] - zindex);
                    numRead += read<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
                }
                i += labs(nx);
            }
            return numRead;
        }

        template<typename RType, typename IType>
        index_t write_indices(RType * ptr, IType * pindex, size_t length, size_t skip = 1, int zindex = 0) {
            index_t numWrote = 0;
            for ( size_t i = 0; i < length; i++ ) {
                if ( isNA(pindex[i]) ) {
                    continue;
                }
                index_t nx = count_consecutive<IType>(pindex, i, length);
                if ( nx >= 0 ) {
                    index_t count = nx + 1;
                    index_t offset = static_cast<index_t>(pindex[i] - zindex);
                    numWrote += write<RType>(ptr + (skip * i), offset, count, skip);
                }
                else {
                    index_t count = (-nx) + 1;
                    index_t offset = static_cast<index_t>(pindex[i + (-nx)] - zindex);
                    numWrote += write<RType>(ptr + skip * (i + (-nx)), offset, count, -skip);
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
            _data = new Atoms(R_do_slot(x, Rf_install("data")), _sources, _ops);
            _datamode = INTEGER(R_do_slot(x, Rf_install("datamode")));
            _chunksize = Rf_asInteger(R_do_slot(x, Rf_install("chunksize")));
            _length = static_cast<index_t>(Rf_asReal(R_do_slot(x, Rf_install("length"))));
            _dim = R_do_slot(x, Rf_install("dim"));
            _dimnames = R_do_slot(x, Rf_install("dimnames"));
            _names = R_do_slot(x, Rf_install("names"));
            const char * classes[] = { "matter_vec", "matter_matc", "matter_matr",
                                        "matter_list", "matter_str", "" };
            int classmatch = R_check_class_etc(x, classes);
            switch (classmatch) {
                case 0:
                    _S4class = MATTER_VEC;
                    break;
                case 1:
                    _S4class = MATTER_MATC;
                    break;
                case 2:
                    _S4class = MATTER_MATR;
                    break;
                case 3:
                    _S4class = MATTER_LIST;
                    break;
                case 4:
                    _S4class = MATTER_STR;
                    break;
                default:
                    _S4class = MATTER_ANY;
                    break;
            }
            set_matter_options();
        }

        ~Matter() {
            delete _data;
        }

        Atoms & data() {
            return (*_data);
        }

        int datamode() {
            return _datamode[0];
        }

        int datamode(int i) {
            if ( S4class() == MATTER_LIST )
            {
                if ( 0 <= i && i < length() )
                    return _datamode[i];
                else
                    return R_LIST; // will return NULL
            }
            else
                return _datamode[0];
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

        SEXP dim() {
            return _dim;
        }

        int dim(int i) {
            return INTEGER(_dim)[i];
        }

        int dimlength() {
            return LENGTH(_dim);
        }

        SEXP dimnames() {
            return _dimnames;
        }

        SEXP names() {
            return _names;
        }

        int nrows() {
            if ( dimlength() >= 1 )
                return dim(0);
            else
                return 0;
        }

        int ncols() {
            if ( dimlength() >= 2 )
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
        SEXP readListElements(int i);

        template<typename RType, int SType>
        void writeListElements(int i, SEXP value);

        template<typename RType, int SType>
        SEXP readListElements(int i, SEXP j);

        template<typename RType, int SType>
        void writeListElements(int i, SEXP j, SEXP value);

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

        template<typename T, typename RType, int SType>
        SEXP rmult(SEXP y);

        template<typename T, typename RType, int SType>
        SEXP lmult(SEXP x);

        template<typename T>
        SEXP range(bool na_rm = false);

        template<typename T>
        SEXP prod(bool na_rm = false);

        template<typename T>
        SEXP sum(bool na_rm = false);

        template<typename T>
        SEXP mean(bool na_rm = false);

        template<typename T>
        SEXP var(bool na_rm = false);

        template<typename T>
        SEXP any(bool na_rm = false);

        template<typename T>
        SEXP all(bool na_rm = false);

        template<typename T>
        SEXP colsums(bool na_rm = false);

        template<typename T>
        SEXP rowsums(bool na_rm = false);

        template<typename T>
        SEXP colmeans(bool na_rm = false);

        template<typename T>
        SEXP rowmeans(bool na_rm = false);

        template<typename T>
        SEXP colvar(bool na_rm = false);

        template<typename T>
        SEXP rowvar(bool na_rm = false);

        SEXP which();

    protected:

        Atoms * _data;     // EITHER "atoms" OR a *list* of "atoms"
        int * _datamode;  // 1 = raw, 2 = logical, 3 = integer, 4 = numeric
        DataSources _sources;
        Ops _ops;
        int _chunksize;
        index_t _length;
        SEXP _dim;
        SEXP _dimnames;
        SEXP _names;
        int _S4class;      // 0 = any, 2 = col-matrix, 3 = row-matrix
};



//// MatterIterator class
//-----------------------

template<typename RType>
class MatterIterator
{

    public:

        MatterIterator(Matter & x) : _x(x)
        {
            x.data().set_group(0);
            if ( x.data().length() > 1 )
                _next = 1;
            else
                _next = NULL_INDEX;
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

template<typename T>
pair_double range(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
double prod(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
double sum(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
double mean(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
double var(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
int any(MatterIterator<T> & x, bool na_rm = false);

template<typename T>
int all(MatterIterator<T> & x, bool na_rm = false);


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

    SEXP getList(SEXP x);

    void setList(SEXP x, SEXP value);

    SEXP getListElements(SEXP x, SEXP i, SEXP j);

    void setListElements(SEXP x, SEXP i, SEXP j, SEXP value);

    SEXP getMatrix(SEXP x);

    void setMatrix(SEXP x, SEXP value);

    SEXP getMatrixRows(SEXP x, SEXP i);

    void setMatrixRows(SEXP x, SEXP i, SEXP value);

    SEXP getMatrixCols(SEXP x, SEXP j);

    void setMatrixCols(SEXP x, SEXP j, SEXP value);

    SEXP getMatrixElements(SEXP x, SEXP i, SEXP j);

    void setMatrixElements(SEXP x, SEXP i, SEXP j, SEXP value);

    SEXP getString(SEXP x);

    SEXP getStringElements(SEXP x, SEXP i);

    SEXP getRange(SEXP x, SEXP na_rm);

    SEXP getProd(SEXP x, SEXP na_rm);

    SEXP getSum(SEXP x, SEXP na_rm);

    SEXP getMean(SEXP x, SEXP na_rm);

    SEXP getVar(SEXP x, SEXP na_rm);

    SEXP getAny(SEXP x, SEXP na_rm);

    SEXP getAll(SEXP x, SEXP na_rm);

    SEXP getColSums(SEXP x, SEXP na_rm);

    SEXP getColMeans(SEXP x, SEXP na_rm);

    SEXP getColVars(SEXP x, SEXP na_rm);

    SEXP getRowSums(SEXP x, SEXP na_rm);

    SEXP getRowMeans(SEXP x, SEXP na_rm);

    SEXP getRowVars(SEXP x, SEXP na_rm);

    SEXP rightMatrixMult(SEXP x, SEXP y);

    SEXP leftMatrixMult(SEXP x, SEXP y);

    SEXP getWhich(SEXP x);

    SEXP countRuns(SEXP x, SEXP delta);

    SEXP createDRLE(SEXP x, SEXP nruns, SEXP delta);

    SEXP getDRLE(SEXP x);

    SEXP getDRLEElements(SEXP x, SEXP i);

}

#endif

