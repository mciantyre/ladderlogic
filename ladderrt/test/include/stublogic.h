#ifndef STUBLOGIC_H_
#define STUBLOGIC_H_

#include "logic.h"

class StubLogic : public Logic
{
public:
    StubLogic(bool result) : _result(result) { /* empty */ }
    
    bool Evaluate() const override
    {
        return _result;
    }

private:
    bool _result;
};

#endif