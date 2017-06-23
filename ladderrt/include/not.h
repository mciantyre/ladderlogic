#ifndef NOT_H_
#define NOT_H_

#include "logic.h"

class Not : public Logic
{
public:
    Not(Logic& logic);
    bool Evaluate() const override;

private:
    Logic& _logic;
};
#endif