#ifndef ATOMS
#define ATOMS

#include <ios>
#include <fstream>

#include "Rutils.h"
#include "drle.h"

//// DataSources class
//---------------------

class DataSources2 {

	public:

		DataSources2(SEXP x)
		{
			SEXP sources = R_do_slot(x, Rf_install("source"));
			_paths = Rf_getAttrib(sources, R_LevelsSymbol);
			_length = LENGTH(_paths);
			_readonly = Rf_asLogical(R_do_slot(x, Rf_install("readonly")));
			if ( LENGTH(sources) < 0 || _length < 0 )
				Rf_error("no data sources found");
			if ( _readonly )
				_mode = std::ios::in | std::ios::binary;
			else
				_mode = std::ios::in | std::ios::out | std::ios::binary;
			init_streams();
		}

		~DataSources2() {
			exit_streams();
		}

		void init_streams()
		{
			if ( _streams == NULL ) {
				_streams = (std::fstream **) R_Calloc(_length, std::fstream*);
				for ( int i = 0; i < _length; i++ )
					_streams[i] = NULL;
			}
			_current = 0;
		}

		void exit_streams()
		{
			if ( _streams != NULL ) {
				for ( int i = 0; i < _length; i++ )
					if ( _streams[i] != NULL ) {
						_streams[i]->close();
						delete _streams[i];
						_streams[i] = NULL;
					}
			}
			_streams = NULL;
			R_Free(_streams);
		}

		bool readonly() {
			return _readonly;
		}

		SEXP path(int src) {
			return STRING_ELT(_paths, src);
		}

		int length(int src) {
			return _length;
		}

		std::fstream * select(int src)
		{
			if ( _streams[src] == NULL ) {
				const char * filename = CHAR(path(src));
				_streams[src] = new std::fstream();
				_streams[src]->open(filename, _mode);
				if ( !_streams[src]->is_open() ) {
					exit_streams();
					Rf_error("could not open file '%s'", filename);
				}
			}
			_current = src;
			return _streams[_current];
		}

		DataSources2 * rseek(int src, index_t off = 0)
		{
			select(src)->seekg(off, std::ios::beg);
			return this;
		}

		DataSources2 * wseek(int src, index_t off = 0)
		{
			select(src)->seekp(off, std::ios::beg);
			return this;
		}

		template<typename T>
		bool read(void * ptr, size_t size)
		{
			std::fstream * stream = _streams[_current];
			stream->read(reinterpret_cast<char*>(ptr), sizeof(T) * size);
			return !stream->fail();
		}

		template<typename T>
		bool write(void * ptr, size_t size)
		{
			if ( readonly() ) {
				exit_streams();
				Rf_error("storage mode is read-only");
			}
			std::fstream * stream = _streams[_current];
			stream->write(reinterpret_cast<char*>(ptr), sizeof(T) * size);
			return !stream->fail();
		}

	protected:

		SEXP _paths;
		bool _readonly;
		std::ios::openmode _mode;
		std::fstream ** _streams = NULL;
		int _current;
		int _length;

};

//// Atoms class
//-----------------------

class Atoms2 {

	public:

		Atoms2(SEXP x) : _io(x),
			_sources(R_do_slot(x, Rf_install("source"))),
			_types(R_do_slot(x, Rf_install("type"))),
			_offsets(R_do_slot(x, Rf_install("offset"))),
			_extents(R_do_slot(x, Rf_install("extent"))),
			_groups(R_do_slot(x, Rf_install("group"))),
			_pointers(R_do_slot(x, Rf_install("pointers"))) {}

		~Atoms2() {
			_io.exit_streams();
		}

		void self_destruct() {
			_io.exit_streams();
		}

		int natoms() {
			return _offsets.length();
		}

		int ngroups() {
			return _pointers.length() - 1;
		}

		Pair<int,index_t> find_atom(index_t i, int grp)
		{
			index_t j = 0;
			int atom = find_group(grp);
			Pair<int,index_t> info;
			while ( i >= 0 && group(atom) == grp && atom < natoms() )
			{
				size_t len = extent(atom);
				if ( j <= i && i < j + len ) {
					info = {atom, i - j};
					return info;
				}
				j += len;
				atom++;
			}
			self_destruct();
			Rf_error("subscript out of bounds");
		}

		int find_group(int grp) {
			return _pointers[grp];
		}

		int source(int atom) {
			return _sources[atom] - 1;
		}

		int type(int atom) {
			return _types[atom];
		}

		int group(int atom) {
			return _groups[atom];
		}

		index_t extent(int atom) {
			return static_cast<index_t>(_extents[atom]);
		}

		index_t offset(int atom) {
			return static_cast<index_t>(_offsets[atom]);
		}

		index_t offset(int atom, index_t pos)
		{
			index_t add_offset;
			switch(type(atom))
			{
				case C_CHAR:
				case C_UCHAR:
					add_offset = sizeof(char) * pos;
					break;
				case C_SHORT:
				case C_USHORT:
					add_offset = sizeof(int16_t) * pos;
					break;
				case C_INT:
				case C_UINT:
					add_offset = sizeof(int32_t) * pos;
					break;
				case C_LONG:
				case C_ULONG:
					add_offset = sizeof(int64_t) * pos;
					break;
				case C_FLOAT:
					add_offset = sizeof(float) * pos;
					break;
				case C_DOUBLE:
					add_offset = sizeof(double) * pos;
					break;
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			return offset(atom) + add_offset;
		}

		template<typename Tin, typename Tout>
		index_t read_atom(Tout * ptr, int atom, size_t pos, size_t size, int stride = 1)
		{
			if ( pos + size >= extent(atom) )
				size = extent(atom) - pos; // allow specifying over-size for convenience
			Tin * tmp = (Tin *) R_Calloc(size, Tin);
			bool success = _io.rseek(source(atom), offset(atom, pos))->read<Tin>(tmp, size);
			if ( !success ) {
				self_destruct();
				Rf_error("failed to read data elements");
			}
			for ( index_t i = 0; i < size; i++ )
				ptr[stride * i] = static_cast<Tout>(tmp[i]); // FIXME: change to coerce_cast()
			R_Free(tmp);
			return size;
		}

		template<typename Tin, typename Tout>
		index_t write_atom(Tin * ptr, int atom, size_t pos, size_t size, int stride = 1)
		{
			if ( pos + size >= extent(atom) )
				size = extent(atom) - pos;  // allow specifying over-size for convenience
			Tout * tmp = (Tout *) R_Calloc(size, Tout);
			for ( index_t i = 0; i < size; i++ )
				tmp[i] = static_cast<Tout>(ptr[stride * i]); // FIXME: change to coerce_cast()
			bool success = _io.wseek(source(atom), offset(atom, pos))->write<Tout>(tmp, size);
			if ( !success ) {
				self_destruct();
				Rf_error("failed to write data elements");
			}
			R_Free(tmp);
			return size;
		}

		template<typename T>
		index_t get_atom(T * ptr, int atom, size_t pos, size_t size, int stride = 1)
		{
			switch(type(atom)) {
				case C_CHAR:
					return read_atom<char,T>(ptr, atom, pos, size, stride);
				case C_UCHAR:
					return read_atom<unsigned char,T>(ptr, atom, pos, size, stride);
				case C_SHORT:
					return read_atom<int16_t,T>(ptr, atom, pos, size, stride);
				case C_USHORT:
					return read_atom<uint16_t,T>(ptr, atom, pos, size, stride);
				case C_INT:
					return read_atom<int32_t,T>(ptr, atom, pos, size, stride);
				case C_UINT:
					return read_atom<uint32_t,T>(ptr, atom, pos, size, stride);
				case C_LONG:
					return read_atom<int64_t,T>(ptr, atom, pos, size, stride);
				case C_ULONG:
					return read_atom<uint64_t,T>(ptr, atom, pos, size, stride);
				case C_FLOAT:
					return read_atom<float,T>(ptr, atom, pos, size, stride);
				case C_DOUBLE:
					return read_atom<double,T>(ptr, atom, pos, size, stride);
				default:
					Rf_error("unsupported data type");
			}
		}

		template<typename T>
		index_t set_atom(T * ptr, int atom, size_t pos, size_t size, int stride = 1)
		{
			switch(type(atom)) {
				case C_CHAR:
					return write_atom<T,char>(ptr, atom, pos, size, stride);
				case C_UCHAR:
					return write_atom<T,unsigned char>(ptr, atom, pos, size, stride);
				case C_SHORT:
					return write_atom<T,int16_t>(ptr, atom, pos, size, stride);
				case C_USHORT:
					return write_atom<T,uint16_t>(ptr, atom, pos, size, stride);
				case C_INT:
					return write_atom<T,int32_t>(ptr, atom, pos, size, stride);
				case C_UINT:
					return write_atom<T,uint32_t>(ptr, atom, pos, size, stride);
				case C_LONG:
					return write_atom<T,int64_t>(ptr, atom, pos, size, stride);
				case C_ULONG:
					return write_atom<T,uint64_t>(ptr, atom, pos, size, stride);
				case C_FLOAT:
					return write_atom<T,float>(ptr, atom, pos, size, stride);
				case C_DOUBLE:
					return write_atom<T,double>(ptr, atom, pos, size, stride);
				default:
					Rf_error("unsupported data type");
			}
		}

		template<typename T>
		index_t get_region(T * ptr, index_t i, size_t size, int grp = 0, int stride = 1)
		{
			Pair<int,index_t> info = find_atom(i, grp);
			int atom = info.first;
			index_t n, pos = info.second, num_read = 0, num_toread = size;
			while ( num_toread > 0 )
			{
				if ( atom >= natoms() || group(atom) != grp ) {
					self_destruct();
					Rf_error("subscript out of bounds");
				}
				n = get_atom<T>(ptr, atom, pos, num_toread, stride);
				num_read += n;
				num_toread -= n;
				pos = 0;
				atom++;
				ptr += (stride * n);
			}
			return num_read;
		}

		template<typename T>
		index_t set_region(T * ptr, index_t i, size_t size, int grp = 0, int stride = 1)
		{
			Pair<int,index_t> info = find_atom(i, grp);
			int atom = info.first;
			index_t n, pos = info.second, num_write = 0, num_towrite = size;
			while ( num_towrite > 0 )
			{
				if ( atom >= natoms() || group(atom) != grp ) {
					self_destruct();
					Rf_error("subscript out of bounds");
				}
				n = set_atom<T>(ptr, atom, pos, num_towrite, stride);
				num_write += n;
				num_towrite -= n;
				pos = 0;
				atom++;
				ptr += (stride * n);
			}
			return num_write;
		}

		template<typename Tind, typename Tval>
		index_t get_elements(Tval * ptr, Tind * pindx, size_t size,
			int grp = 0, int stride = 1, bool ind1 = false)
		{
			index_t n, i = 0, num_read = 0, num_toread = size;
			while ( num_toread > 0 )
			{
				Pair<R_xlen_t,Tind> run = compute_run<Tind>(pindx, 0, num_toread, true);
				size = run.first;
				if ( run.second >= 0 ) {
					i = (*pindx) - ind1;
					n = get_region<Tval>(ptr, i, size, grp, stride);
				}
				else {
					i = (*(pindx + size - 1)) - ind1;
					n = get_region<Tval>(ptr + size - 1, i, size, grp, -stride);
				}
				num_read += n;
				num_toread -= n;
				pindx += n;
				ptr += (stride * n);
			}
			return num_read;
		}

		template<typename Tind, typename Tval>
		index_t set_elements(Tval * ptr, Tind * pindx, size_t size,
			int grp = 0, int stride = 1, bool ind1 = false)
		{
			index_t n, i = 0, num_read = 0, num_toread = size;
			while ( num_toread > 0 )
			{
				i = (*pindx) - ind1;
				Pair<R_xlen_t,Tind> run = compute_run<Tind>(pindx, 0, num_toread, true);
				size = run.first;
				if ( run.second >= 0 ) {
					i = (*pindx) - ind1;
					n = set_region<Tval>(ptr, i, size, grp, stride);
				}
				else {
					i = (*(pindx + size - 1)) - ind1;
					n = set_region<Tval>(ptr + size - 1, i, size, grp, -stride);
				}
				num_read += n;
				num_toread -= n;
				pindx += n;
				ptr += (stride * n);
			}
			return num_read;
		}

		template<typename T>
		index_t get_elements(T * ptr, SEXP indx, int grp = 0, int stride = 1)
		{
			R_xlen_t len = XLENGTH(indx);
			switch(TYPEOF(indx)) {
				case INTSXP:
					return get_elements<int,T>(ptr, INTEGER(indx), len, grp, stride, true);
				case REALSXP:
					return get_elements<double,T>(ptr, REAL(indx), len, grp, stride, true);
				default:
					self_destruct();
					Rf_error("invalid index type");
			}
		}

		template<typename T>
		index_t set_elements(T * ptr, SEXP indx, int grp = 0, int stride = 1)
		{
			R_xlen_t len = XLENGTH(indx);
			switch(TYPEOF(indx)) {
				case INTSXP:
					return set_elements<int,T>(ptr, INTEGER(indx), len, grp, stride, true);
				case REALSXP:
					return set_elements<double,T>(ptr, REAL(indx), len, grp, stride, true);
				default:
					self_destruct();
					Rf_error("invalid index type");
			}
		}

	protected:

		DataSources2 _io;
		CompressedFactor _sources; // return as 0-based (internally 1-based)
		CompressedFactor _types; // int type codes for data types
		CompressedVector<double> _offsets; // byte offset in file
		CompressedVector<double> _extents; // number of elements
		CompressedVector<int> _groups; // 0-based organization
		CompressedVector<int> _pointers; // 0-based pointers to groups

};

#endif // ATOMS
