#ifndef INPUT_H_
#define INPUT_H_

#include "logic.h"

class Input : public Logic
{
public:
    explicit Input(int pin);
    bool Evaluate() const override;

private:
    int _pin;
};

#endif // INPUT_H_