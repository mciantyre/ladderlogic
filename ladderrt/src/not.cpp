#include "not.h"

Not::Not(Logic* logic)
    : _logic(logic)
{
    /* empty */
}

bool Not::Evaluate() const
{
    return !_logic->Evaluate();
}

Not::~Not()
{
    delete _logic;
}