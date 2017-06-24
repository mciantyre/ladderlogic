#ifndef LOGICMANAGER_H_
#define LOGICMANAGER_H_

#include "logic.h"

class LogicManager
{
public:

    static constexpr unsigned MAX_ALLOCATIONS = 100;

    template <class L, class... Args>
    static Logic& Make(Args&&... args )
    {
        L* logic = new L(args...);
        _logics[_allocated] = logic;
        ++_allocated;
        return *logic;
    }

    static void Reset()
    {
        for (unsigned i = 0; i < _allocated; ++i)
        {
            delete _logics[i];
        }
        _allocated = 0;
    }

private:
    static Logic* _logics[];
    static unsigned _allocated;
};

Logic* LogicManager::_logics[MAX_ALLOCATIONS] = {nullptr};
unsigned LogicManager::_allocated = 0;

#endif