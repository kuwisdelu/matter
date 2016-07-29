
#ifndef MATTER_ACCESSOR_H
#define MATTER_ACCESSOR_H

#include "matter.h"

//// WORK IN PROGRESS ////

//// MatterAccessor class
//-----------------------

template<typename RType>
class MatterAccessor
{

    public:

        MatterAccessor(Matter & x) : _data(x)
        {
            _current = 0;
            _min = 0;
            _max = x.buffer_size() - 1;
            _buffer = (RType *) Calloc(x.buffer_size(), RType);
        }

        ~MatterAccessor()
        {
            Free(_buffer);
        }

    protected:

        Matter & _data;
        index_type _current;
        index_type _min;
        index_type _max;
        RType * _buffer;
};

#endif
