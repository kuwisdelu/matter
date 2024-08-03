#ifndef SHARED_MEMORY
#define SHARED_MEMORY

#define BOOST_NO_AUTO_PTR

#include <boost/interprocess/shared_memory_object.hpp>

#include "matterDefines.h"

namespace ipc = boost::interprocess;

//// SharedMemorySource class
//---------------------------

class SharedMemorySource : public SourceInterface {

	public:

		SharedMemorySource(const char * name, bool readonly)
		{
			// TODO
		}

		~SharedMemorySource() {
			// TODO
		}

		void close()
		{
			// TODO
		}

		void rseek(index_t off) {
			// TODO
		}

		void wseek(index_t off) {
			// TODO
		}

		template<typename T>
		void read(void * ptr, size_t size)
		{
			// TODO
		}

		template<typename T>
		void write(void * ptr, size_t size)
		{
			// TODO
		}

	protected:

		ipc::shared_memory_object _shm;

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
