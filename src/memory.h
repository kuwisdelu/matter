#ifndef SHARED_MEMORY
#define SHARED_MEMORY

#define BOOST_NO_AUTO_PTR

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>

// needed due to system headers brought in by boost
// so that <R_ext/Boolean.h> gets used instead
#undef FALSE
#undef TRUE

#include "matterDefines.h"

namespace ipc = boost::interprocess;

//// SharedMemorySource class
//---------------------------

class SharedMemorySource : public SourceInterface {

	public:

		SharedMemorySource(const char * name, bool readonly)
		{
			_sourcetype = SH_MEMORY;
			ipc::mode_t mode;
			if ( readonly )
				mode = ipc::read_only;
			else
				mode = ipc::read_write;
			try
			{
				_shm = ipc::shared_memory_object(ipc::open_only, name, mode);
				_region = ipc::mapped_region(_shm, _shm.get_mode());
				_ptr = reinterpret_cast<char*>(_region.get_address());
				_ok = true;
			}
			catch(...)
			{
				_ptr = NULL;
				_ok = false;
			}
		}

		~SharedMemorySource() {}

		void close() {}

		void rseek(index_t off) {
			_ptr = reinterpret_cast<char*>(_region.get_address()) + off;
		}

		void wseek(index_t off) {
			_ptr = reinterpret_cast<char*>(_region.get_address()) + off;
		}

		template<typename T>
		void read(void * ptr, size_t size)
		{
			char * start = reinterpret_cast<char*>(_region.get_address());
			ipc::offset_t req_size = (_ptr + size) - start;
			if ( req_size > _region.get_size() )
				resize(req_size);
			if ( ok() )
				std::memcpy(ptr, reinterpret_cast<void*>(_ptr), size);
		}

		template<typename T>
		void write(void * ptr, size_t size)
		{
			char * start = reinterpret_cast<char*>(_region.get_address());
			ipc::offset_t req_size = (_ptr + size) - start;
			if ( req_size > _region.get_size() )
				resize(req_size);
			if ( ok() )
				std::memcpy(reinterpret_cast<void*>(_ptr), ptr, size);
		}

		void resize(size_t size)
		{
			try
			{
				_shm.truncate(size);
				_region = ipc::mapped_region(_shm, _shm.get_mode());
				_ptr = reinterpret_cast<char*>(_region.get_address());
				_ok = _region.get_size() >= size;
			}
			catch(...)
			{
				_ptr = NULL;
				_ok = false;
			}
		}

	protected:

		ipc::shared_memory_object _shm;
		ipc::mapped_region _region;
		char * _ptr;

};

inline bool create_shared_memory_obj(const char * name)
{
	try
	{
		ipc::shared_memory_object shm(ipc::create_only, name, ipc::read_only);
	}
	catch(...)
	{
		return false;
	}
	return true;
}

inline index_t sizeof_shared_memory_obj(const char * name)
{
	ipc::offset_t size = 0;
	try
	{
		ipc::shared_memory_object shm(ipc::open_only, name, ipc::read_only);
		shm.get_size(size);
	}
	catch(...)
	{
		return 0;
	}
	return static_cast<index_t>(size);
}

inline bool remove_shared_memory_obj(const char * name)
{
	return ipc::shared_memory_object::remove(name);
}

#endif // SHARED_MEMORY
