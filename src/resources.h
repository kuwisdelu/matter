#ifndef SHARED_RESOURCES
#define SHARED_RESOURCES

#define BOOST_NO_AUTO_PTR

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>

// needed due to system headers brought in by boost
// so that <R_ext/Boolean.h> gets used instead
#undef FALSE
#undef TRUE

#include "matterDefines.h"

namespace ipc = boost::interprocess;

//// FileSource class
//-------------------

class FileSource : public SourceInterface {

	public:

		FileSource(const char * name, bool readonly) : _file()
		{
			_sourcetype = SH_FILE;
			std::ios::openmode mode;
			if ( readonly )
				mode = std::ios::in | std::ios::binary;
			else
				mode = std::ios::in | std::ios::out | std::ios::binary;
			_file.open(name, mode);
			_ok = _file.good();
		}

		~FileSource() {
			close();
		}

		void close()
		{
			if ( _file.is_open() )
				_file.close();
		}

		void rseek(index_t off) {
			_file.seekg(off, std::ios::beg);
		}

		void wseek(index_t off) {
			_file.seekp(off, std::ios::beg);
		}

		template<typename T>
		void read(void * ptr, size_t size)
		{
			_file.read(reinterpret_cast<char*>(ptr), sizeof(T) * size);
			_ok = _file.good();
		}

		template<typename T>
		void write(void * ptr, size_t size)
		{
			_file.write(reinterpret_cast<char*>(ptr), sizeof(T) * size);
			_ok = _file.good();
		}

	protected:

		std::fstream _file;

};

//// SharedMemorySource class
//---------------------------

class SharedMemorySource : public SourceInterface {

	public:

		SharedMemorySource(const char * name, bool readonly) : _region()
		{
			_sourcetype = SH_MEMORY;
			ipc::mode_t mode;
			if ( readonly )
				mode = ipc::read_only;
			else
				mode = ipc::read_write;
			try {
				_shm = ipc::shared_memory_object(ipc::open_only, name, mode);
				_ok = true;
				remap();
			}
			catch(...) {
				_ok = false;
			}
			_off = 0;
		}

		~SharedMemorySource() {}

		void close() {}

		void rseek(index_t off) {
			_off = off;
		}

		void wseek(index_t off) {
			_off = off;
		}

		template<typename T>
		void read(void * ptr, size_t size)
		{
			index_t extent = _off + (sizeof(T) * size);
			if ( extent > _region.get_size() )
				resize(extent);
			if ( ok() ) {
				char * src = static_cast<char*>(_region.get_address()) + _off;
				std::memcpy(ptr, static_cast<void*>(src), sizeof(T) * size);
			}
		}

		template<typename T>
		void write(void * ptr, size_t size)
		{
			index_t extent = _off + (sizeof(T) * size);
			if ( extent > _region.get_size() )
				resize(extent);
			if ( ok() ) {
				char * dest = static_cast<char*>(_region.get_address()) + _off;
				std::memcpy(static_cast<void*>(dest), ptr, sizeof(T) * size);
			}
		}

		index_t shared_size()
		{
			ipc::offset_t size = 0;
			_ok = _shm.get_size(size);
			return static_cast<index_t>(size);
		}

		void resize(size_t size)
		{
			if ( shared_size() < size )
			{
				try {
					_shm.truncate(size);
					remap();
				}
				catch(...) {
					_ok = false;
				}
			}
		}

		void remap()
		{
			if ( shared_size() > 0 )
				_region = ipc::mapped_region(_shm, _shm.get_mode());
			else
				_region = ipc::mapped_region();
		}

	protected:

		ipc::shared_memory_object _shm;
		ipc::mapped_region _region;
		index_t _off;

};

inline bool create_shared_memory_obj(const char * name)
{
	try {
		ipc::shared_memory_object shm(ipc::create_only, name, ipc::read_only);
	}
	catch(...) {
		Rf_error("could not map shared memory: %s", name);
	}
	return true;
}

inline bool detect_shared_memory_obj(const char * name)
{
	try {
		ipc::shared_memory_object shm(ipc::open_only, name, ipc::read_only);
	}
	catch(...) {
		return false;
	}
	return true;
}

inline index_t sizeof_shared_memory_obj(const char * name)
{
	ipc::offset_t size = 0;
	try {
		ipc::shared_memory_object shm(ipc::open_only, name, ipc::read_only);
		shm.get_size(size);
	}
	catch(...) {
		Rf_error("could not map shared memory: %s", name);
	}
	return static_cast<index_t>(size);
}

inline bool remove_shared_memory_obj(const char * name)
{
	return ipc::shared_memory_object::remove(name);
}

#endif // SHARED_RESOURCES
