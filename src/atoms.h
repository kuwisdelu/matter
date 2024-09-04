#ifndef ATOMS
#define ATOMS

#include <ios>
#include <fstream>
#include <cstdint>

#include "matterDefines.h"
#include "resources.h"
#include "coerce.h"
#include "drle.h"

//// DataSources class
//---------------------

class DataSources {

	public:

		DataSources(SEXP x)
		{
			_readonly = Rf_asLogical(R_do_slot(x, Rf_install("readonly")));
			SEXP sources = R_do_slot(x, Rf_install("source"));
			if ( Rf_isS4(sources) )
				_names = R_do_slot(sources, Rf_install("levels"));
			else
				_names = Rf_getAttrib(sources, R_LevelsSymbol);
			_nsources = LENGTH(_names);
			if ( _nsources < 0 )
				Rf_error("no data sources found");
			init_sources();
		}

		~DataSources() {
			exit_sources();
		}

		void init_sources()
		{
			if ( _sources == NULL )
			{
				_sources = (SourceInterface **) R_Calloc(_nsources, SourceInterface*);
				for ( int i = 0; i < _nsources; i++ )
					_sources[i] = NULL;
			}
			_current = 0;
		}

		void exit_sources()
		{
			if ( _sources != NULL )
			{
				for ( int i = 0; i < _nsources; i++ ) 
					close(i);
			}
			_sources = NULL;
			Free(_sources);
		}

		SourceInterface * open(int src)
		{
			if ( _sources[src] == NULL )
			{
				Rprintf("opening src %d\n", src);
				const char * name = CHAR(STRING_ELT(_names, src));
				switch(parse_sourcetype(name)) {
					case SH_FILE:
						_sources[src] = new FileSource(name, readonly());
						if ( !_sources[src]->ok() ) {
							exit_sources();
							Rf_error("could not open file: '%s'", name);
						}
						break;
					case SH_MEMORY:
						Rprintf("opening shm src %d\n", src);
						_sources[src] = new SharedMemorySource(name, readonly());
						if ( !_sources[src]->ok() ) {
							exit_sources();
							Rf_error("could not map shared memory: '%s'", name);
						}
						break;
					default:
						break;
				}
			}
			_current = src;
			return _sources[_current];
		}

		void close(int src)
		{
			if ( _sources[src] != NULL )
			{
				Rprintf("closing src %d\n", src);
				switch(_sources[src]->sourcetype()) {
					case SH_FILE:
						static_cast<FileSource*>(_sources[src])->close();
						break;
					case SH_MEMORY:
						static_cast<SharedMemorySource*>(_sources[src])->close();
						break;
					default:
						break;
				}
				delete _sources[src];
				_sources[src] = NULL;
			}
		}

		template<typename T>
		T source(int src) {
			return static_cast<T>(open(src));
		}

		DataSources * rseek(int src, index_t off = 0)
		{
			Rprintf("rseek src %d -> %d\n", src, off);
			switch(open(src)->sourcetype()) {
				case SH_FILE:
					source<FileSource*>(src)->rseek(off);
					break;
				case SH_MEMORY:
					source<SharedMemorySource*>(src)->rseek(off);
					break;
				default:
					break;
			}
			return this;
		}

		DataSources * wseek(int src, index_t off = 0)
		{
			Rprintf("wseek src %d -> %d\n", src, off);
			switch(open(src)->sourcetype()) {
				case SH_FILE:
					source<FileSource*>(src)->wseek(off);
					break;
				case SH_MEMORY:
					source<SharedMemorySource*>(src)->wseek(off);
					break;
				default:
					break;
			}
			return this;
		}

		template<typename T>
		bool read(void * ptr, size_t size)
		{
			Rprintf("reading %d elts\n", size);
			switch(open(_current)->sourcetype()) {
				case SH_FILE:
					source<FileSource*>(_current)->read<T>(ptr, size);
					break;
				case SH_MEMORY:
					source<SharedMemorySource*>(_current)->read<T>(ptr, size);
					break;
				default:
					break;
			}
			return ok();
		}

		template<typename T>
		bool write(void * ptr, size_t size)
		{
			Rprintf("writing %d elts\n", size);
			if ( readonly() ) {
				exit_sources();
				Rf_error("storage mode is read-only");
			}
			switch(open(_current)->sourcetype()) {
				case SH_FILE:
					source<FileSource*>(_current)->write<T>(ptr, size);
					break;
				case SH_MEMORY:
					source<SharedMemorySource*>(_current)->write<T>(ptr, size);
					break;
				default:
					break;
			}
			return ok();
		}

		bool ok()
		{
			if ( _sources[_current] == NULL )
				return false;
			else
				return _sources[_current]->ok();
		}

		bool readonly() {
			return _readonly;
		}

	protected:

		SEXP _names;
		bool _readonly;
		int _nsources;
		SourceInterface ** _sources = NULL;
		int _current;

};

//// Struct for atom information
//-------------------------------

struct AtomInfo {
	int atom;
	index_t pos;
};

//// Atoms class
//----------------

class Atoms {

	public:

		Atoms(SEXP x) : _io(x),
			_sources(R_do_slot(x, Rf_install("source"))),
			_types(R_do_slot(x, Rf_install("type"))),
			_offsets(R_do_slot(x, Rf_install("offset"))),
			_extents(R_do_slot(x, Rf_install("extent"))),
			_groups(R_do_slot(x, Rf_install("group"))),
			_pointers(R_do_slot(x, Rf_install("pointers"))) {}

		~Atoms() {
			_io.exit_sources();
		}

		void self_destruct() {
			_io.exit_sources();
		}

		R_xlen_t natoms() {
			return _offsets.length();
		}

		R_xlen_t ngroups() {
			if ( is_flat() )
				return 1;
			else
				return _pointers.length() - 1;
		}

		R_xlen_t nelements() {
			R_xlen_t n = 0;
			for ( index_t atom = 0; atom < natoms(); atom++ )
				n += extent(atom);
			return n;
		}

		AtomInfo find_atom(index_t i, int grp = 0)
		{
			index_t j = 0;
			int atom = find_group(grp);
			AtomInfo ap;
			if ( i == 0 && extent(atom) == 0)
			{
				ap = {atom, 0};
				return ap;
			}
			while ( i >= 0 && group(atom) == grp && atom < natoms() )
			{
				index_t len = extent(atom);
				if ( j <= i && i < j + len )
				{
					ap = {atom, i - j};
					return ap;
				}
				j += len;
				atom++;
			}
			self_destruct();
			Rf_error("subscript out of bounds");
		}

		int find_group(int grp) {
			if ( is_flat() )
				return 0;
			else
				return _pointers[grp];
		}

		bool is_flat() {
			return _flatten;
		}

		Atoms * flatten(bool flat = true) {
			_flatten = flat;
			return this;
		}

		int source(int atom) {
			return _sources[atom] - 1;
		}

		int type(int atom) {
			return _types[atom];
		}

		int group(int atom) {
			if ( is_flat() )
				return 0;
			else
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
				case C_INT16:
				case C_UINT16:
					add_offset = sizeof(int16_t) * pos;
					break;
				case C_INT32:
				case C_UINT32:
					add_offset = sizeof(int32_t) * pos;
					break;
				case C_INT64:
				case C_UINT64:
					add_offset = sizeof(int64_t) * pos;
					break;
				case C_FLOAT32:
					add_offset = sizeof(float) * pos;
					break;
				case C_FLOAT64:
					add_offset = sizeof(double) * pos;
					break;
				default:
					self_destruct();
					Rf_error("unsupported data type");
			}
			return offset(atom) + add_offset;
		}

		template<typename Tin, typename Tout>
		size_t read_atom(Tout * ptr, int atom, index_t pos, size_t size, int stride = 1)
		{
			// check for user interrupt
			if ( pendingInterrupt() ) {
				self_destruct();
				Rf_error("user interrupt");
			}
			// allow specifying size > extent for convenience
			if ( pos + size >= extent(atom) )
				size = extent(atom) - pos;
			Tin * tmp = (Tin *) R_Calloc(size, Tin);
			index_t off = offset(atom, pos);
			bool success = _io.rseek(source(atom), off)->read<Tin>(tmp, size);
			if ( !success ) {
				Free(tmp);
				self_destruct();
				Rf_error("failed to read data elements");
			}
			for ( size_t i = 0; i < size; i++ )
				ptr[stride * i] = coerce_cast<Tout>(tmp[i]);
			Free(tmp);
			return size;
		}

		template<typename Tin, typename Tout>
		size_t write_atom(Tin * ptr, int atom, index_t pos, size_t size, int stride = 1)
		{
			Rprintf("in Atoms::write_atom() 5\n");
			// check for user interrupt
			if ( pendingInterrupt() ) {
				self_destruct();
				Rf_error("user interrupt");
			}
			// allow specifying size > extent for convenience
			if ( pos + size >= extent(atom) )
				size = extent(atom) - pos; 
			Tout * tmp = (Tout *) R_Calloc(size, Tout);
			for ( size_t i = 0; i < size; i++ )
				tmp[i] = coerce_cast<Tout>(ptr[stride * i]);
			index_t off = offset(atom, pos);
			bool success = _io.wseek(source(atom), off)->write<Tout>(tmp, size);
			if ( !success ) {
				Free(tmp);
				self_destruct();
				Rf_error("failed to write data elements");
			}
			Free(tmp);
			return size;
		}

		template<typename T>
		size_t get_atom(T * ptr, int atom, index_t pos, size_t size, int stride = 1)
		{
			switch(type(atom)) {
				case C_CHAR:
					return read_atom<char,T>(ptr, atom, pos, size, stride);
				case C_UCHAR:
					return read_atom<unsigned char,T>(ptr, atom, pos, size, stride);
				case C_INT16:
					return read_atom<int16_t,T>(ptr, atom, pos, size, stride);
				case C_UINT16:
					return read_atom<uint16_t,T>(ptr, atom, pos, size, stride);
				case C_INT32:
					return read_atom<int32_t,T>(ptr, atom, pos, size, stride);
				case C_UINT32:
					return read_atom<uint32_t,T>(ptr, atom, pos, size, stride);
				case C_INT64:
					return read_atom<int64_t,T>(ptr, atom, pos, size, stride);
				case C_UINT64:
					return read_atom<uint64_t,T>(ptr, atom, pos, size, stride);
				case C_FLOAT32:
					return read_atom<float,T>(ptr, atom, pos, size, stride);
				case C_FLOAT64:
					return read_atom<double,T>(ptr, atom, pos, size, stride);
				default:
					Rf_error("unsupported data type");
			}
		}

		template<typename T>
		size_t set_atom(T * ptr, int atom, index_t pos, size_t size, int stride = 1)
		{
			switch(type(atom)) {
				case C_CHAR:
					return write_atom<T,char>(ptr, atom, pos, size, stride);
				case C_UCHAR:
					return write_atom<T,unsigned char>(ptr, atom, pos, size, stride);
				case C_INT16:
					return write_atom<T,int16_t>(ptr, atom, pos, size, stride);
				case C_UINT16:
					return write_atom<T,uint16_t>(ptr, atom, pos, size, stride);
				case C_INT32:
					return write_atom<T,int32_t>(ptr, atom, pos, size, stride);
				case C_UINT32:
					return write_atom<T,uint32_t>(ptr, atom, pos, size, stride);
				case C_INT64:
					return write_atom<T,int64_t>(ptr, atom, pos, size, stride);
				case C_UINT64:
					return write_atom<T,uint64_t>(ptr, atom, pos, size, stride);
				case C_FLOAT32:
					return write_atom<T,float>(ptr, atom, pos, size, stride);
				case C_FLOAT64:
					return write_atom<T,double>(ptr, atom, pos, size, stride);
				default:
					Rf_error("unsupported data type");
			}
		}

		template<typename T>
		size_t get_region(T * ptr, index_t i, size_t size, int grp = 0, int stride = 1)
		{
			AtomInfo ap = find_atom(i, grp);
			int atom = ap.atom;
			index_t n, pos = ap.pos, num_read = 0, num_toread = size;
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
		size_t set_region(T * ptr, index_t i, size_t size, int grp = 0, int stride = 1)
		{
			Rprintf("in Atoms::set_region() 5\n");
			AtomInfo ap = find_atom(i, grp);
			int atom = ap.atom;
			index_t n, pos = ap.pos, num_write = 0, num_towrite = size;
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
		size_t get_elements(Tval * ptr, Tind * pindx, size_t size,
			int grp = 0, int stride = 1, bool ind1 = false)
		{
			index_t n, i = 0, num_read = 0, num_toread = size;
			while ( num_toread > 0 )
			{
				RunInfo<Tind> run = compute_run<Tind>(pindx, 0, num_toread, RUN_SEQ);
				size = run.length;
				if ( isNA(run.value) )
					n = fill<Tval>(ptr, size, NA<Tval>(), stride);
				else if ( run.delta >= 0 ) {
					i = (*pindx) - ind1;
					n = get_region<Tval>(ptr, i, size, grp, stride);
				}
				else {
					i = (*(pindx + size - 1)) - ind1;
					n = get_region<Tval>(ptr + stride * (size - 1), i, size, grp, -stride);
				}
				num_read += n;
				num_toread -= n;
				pindx += n;
				ptr += (stride * n);
			}
			return num_read;
		}

		template<typename Tind, typename Tval>
		size_t set_elements(Tval * ptr, Tind * pindx, size_t size,
			int grp = 0, int stride = 1, bool ind1 = false)
		{
			index_t n, i = 0, num_read = 0, num_toread = size;
			while ( num_toread > 0 )
			{
				RunInfo<Tind> run = compute_run<Tind>(pindx, 0, num_toread, RUN_SEQ);
				size = run.length;
				if ( isNA(run.value) ) {
					self_destruct();
					Rf_error("NAs not allowed in subscripted assignments");
				}
				else if ( run.delta >= 0 ) {
					i = (*pindx) - ind1;
					n = set_region<Tval>(ptr, i, size, grp, stride);
				}
				else {
					i = (*(pindx + size - 1)) - ind1;
					n = set_region<Tval>(ptr + stride * (size - 1), i, size, grp, -stride);
				}
				num_read += n;
				num_toread -= n;
				pindx += n;
				ptr += (stride * n);
			}
			return num_read;
		}

		template<typename T>
		size_t get_elements(T * ptr, SEXP indx, int grp = 0, int stride = 1)
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
		size_t set_elements(T * ptr, SEXP indx, int grp = 0, int stride = 1)
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

		int compute_span(index_t i, size_t size, int grp = 0)
		{
			// how many atoms does a region span?
			AtomInfo ap = find_atom(i, grp);
			int num_atoms = 0, atom = ap.atom;
			index_t n = size, pos = ap.pos;
			while ( n > 0 )
			{
				if ( atom >= natoms() || group(atom) != grp ) {
					self_destruct();
					Rf_error("subscript out of bounds");
				}
				n -= (extent(atom) - pos);
				pos = 0;
				atom++;
				num_atoms++;
			}
			return num_atoms;
		}

		int compute_block(int atom)
		{
			// how many atoms in a contiguous block of data?
			int num_atoms = 0;
			while ( TRUE )
			{
				num_atoms++;
				if ( atom + 1 >= natoms() )
					break;
				if ( source(atom) != source(atom + 1) )
					break;
				if ( type(atom) != type(atom + 1) )
					break;
				if ( offset(atom, extent(atom)) != offset(atom + 1) )
					break;
				atom++;
			}
			return num_atoms;
		}

		template<typename T>
		SEXP subset_index(T * pindx, size_t size, bool ind1 = false)
		{
			SEXP ans, nms, atomids, offsets, extents;
			index_t n, m, atom, pos;
			RunInfo<T> run;
			AtomInfo ap;
			int k = 0, num_atoms = 0;
			for ( int grp = 0; grp < ngroups(); grp++ )
			{
				size_t i = 0;
				while ( i < size )
				{
					run = compute_run<T>(pindx, i, size, RUN_SEQ);
					n = run.length;
					if ( run.delta == 1 )
						num_atoms += compute_span(pindx[i] - ind1, n, grp);
					else
						num_atoms += n;
					i += n;
				}
			}
			PROTECT(atomids = Rf_allocVector(INTSXP, num_atoms));
			PROTECT(offsets = Rf_allocVector(REALSXP, num_atoms));
			PROTECT(extents = Rf_allocVector(REALSXP, num_atoms));
			int * pids = INTEGER(atomids);
			double * poff = REAL(offsets);
			double * pext = REAL(extents);
			for ( int grp = 0; grp < ngroups(); grp++ )
			{
				size_t i = 0;
				while ( i < size )
				{
					run = compute_run<T>(pindx, i, size, RUN_SEQ);
					n = run.length;
					if ( run.delta == 1 )
					{
						int j = 0;
						while ( j < n )
						{
							if ( k >= num_atoms ) {
								self_destruct();
								Rf_error("subsetting atoms failed");
							}
							ap = find_atom(pindx[i + j] - ind1, grp);
							atom = ap.atom, pos = ap.pos;
							m = extent(atom) - pos;
							m = m < (n - j) ? m : (n - j);
							pids[k] = atom + 1;
							poff[k] = offset(atom, pos);
							pext[k] = m;
							j += m;
							k++;
						}
					}
					else
					{
						for ( int j = 0; j < n; j++ )
						{
							if ( k >= num_atoms ) {
								self_destruct();
								Rf_error("subsetting atoms failed");
							}
							ap = find_atom(pindx[i + j] - ind1, grp);
							atom = ap.atom, pos = ap.pos;
							pids[k] = atom + 1;
							poff[k] = offset(atom, pos);
							pext[k] = 1;
							k++;
						}
					}
					i += n;
				}
			}
			PROTECT(ans = Rf_allocVector(VECSXP, 3));
			PROTECT(nms = Rf_allocVector(STRSXP, 3));
			SET_VECTOR_ELT(ans, 0, atomids);
			SET_VECTOR_ELT(ans, 1, offsets);
			SET_VECTOR_ELT(ans, 2, extents);
			SET_STRING_ELT(nms, 0, Rf_mkChar("index"));
			SET_STRING_ELT(nms, 1, Rf_mkChar("offset"));
			SET_STRING_ELT(nms, 2, Rf_mkChar("extent"));
			Rf_setAttrib(ans, R_NamesSymbol, nms);
			UNPROTECT(5);
			return ans;
		}

		SEXP regroup_index(size_t ngroups)
		{
			SEXP ans, nms, atomids, offsets, extents, groups;
			index_t n, atom, pos;
			AtomInfo ap;
			if ( ngroups < 1 ) {
				self_destruct();
				Rf_error("number of groups is less than 1");
			}
			int k = 0, num_atoms = 0;
			R_xlen_t nelt = nelements();
			R_xlen_t groupsize = nelt / ngroups;
			if ( nelt % ngroups != 0 ) {
				self_destruct();
				Rf_error("number of elements is not a multiple of 'ngroups'");
			}
			for ( index_t i = 0; i < ngroups; i++ )
				num_atoms += flatten()->compute_span(i * groupsize, groupsize);
			PROTECT(atomids = Rf_allocVector(INTSXP, num_atoms));
			PROTECT(offsets = Rf_allocVector(REALSXP, num_atoms));
			PROTECT(extents = Rf_allocVector(REALSXP, num_atoms));
			PROTECT(groups = Rf_allocVector(INTSXP, num_atoms));
			int * pids = INTEGER(atomids);
			double * poff = REAL(offsets);
			double * pext = REAL(extents);
			int * pgrp = INTEGER(groups);
			for ( index_t i = 0; i < ngroups; i++ )
			{
				index_t j = 0;
				while ( j < groupsize )
				{
					if ( k >= num_atoms ) {
						self_destruct();
						Rf_error("regrouping atoms failed");
					}
					ap = flatten()->find_atom(i * groupsize + j);
					atom = ap.atom, pos = ap.pos;
					n = extent(atom) - pos;
					n = n < (groupsize - j) ? n : (groupsize - j);
					pids[k] = atom + 1;
					poff[k] = offset(atom, pos);
					pext[k] = n;
					pgrp[k] = i;
					j += n;
					k++;
				}
			}
			PROTECT(ans = Rf_allocVector(VECSXP, 4));
			PROTECT(nms = Rf_allocVector(STRSXP, 4));
			SET_VECTOR_ELT(ans, 0, atomids);
			SET_VECTOR_ELT(ans, 1, offsets);
			SET_VECTOR_ELT(ans, 2, extents);
			SET_VECTOR_ELT(ans, 3, groups);
			SET_STRING_ELT(nms, 0, Rf_mkChar("index"));
			SET_STRING_ELT(nms, 1, Rf_mkChar("offset"));
			SET_STRING_ELT(nms, 2, Rf_mkChar("extent"));
			SET_STRING_ELT(nms, 3, Rf_mkChar("groups"));
			Rf_setAttrib(ans, R_NamesSymbol, nms);
			UNPROTECT(6);
			return ans;
		}

		SEXP ungroup_index()
		{
			SEXP ans, nms, atomids, offsets, extents;
			int atom = 0, num_atoms = 0;
			while ( atom < natoms() )
			{
				atom += compute_block(atom);
				num_atoms++;
			}
			PROTECT(atomids = Rf_allocVector(INTSXP, num_atoms));
			PROTECT(offsets = Rf_allocVector(REALSXP, num_atoms));
			PROTECT(extents = Rf_allocVector(REALSXP, num_atoms));
			int * pids = INTEGER(atomids);
			double * poff = REAL(offsets);
			double * pext = REAL(extents);
			atom = 0;
			for ( int k = 0; k < num_atoms; k++ )
			{
				if ( atom >= natoms() ) {
					self_destruct();
					Rf_error("ungrouping atoms failed");
				}
				pids[k] = atom + 1;
				poff[k] = offset(atom);
				pext[k] = 0;
				int n = compute_block(atom);
				for ( int j = 0; j < n; j++ )
					pext[k] += extent(atom + j);
				atom += n;
			}
			PROTECT(ans = Rf_allocVector(VECSXP, 3));
			PROTECT(nms = Rf_allocVector(STRSXP, 3));
			SET_VECTOR_ELT(ans, 0, atomids);
			SET_VECTOR_ELT(ans, 1, offsets);
			SET_VECTOR_ELT(ans, 2, extents);
			SET_STRING_ELT(nms, 0, Rf_mkChar("index"));
			SET_STRING_ELT(nms, 1, Rf_mkChar("offset"));
			SET_STRING_ELT(nms, 2, Rf_mkChar("extent"));
			Rf_setAttrib(ans, R_NamesSymbol, nms);
			UNPROTECT(5);
			return ans;
		}

	protected:

		DataSources _io;
		CompressedFactor _sources; // return as 0-based (internally 1-based)
		CompressedFactor _types; // int type codes for data types
		CompressedVector<double> _offsets; // byte offset in file
		CompressedVector<double> _extents; // number of elements
		CompressedVector<int> _groups; // 0-based organization
		CompressedVector<int> _pointers; // 0-based pointers to groups
		bool _flatten = false;

};

#endif // ATOMS
