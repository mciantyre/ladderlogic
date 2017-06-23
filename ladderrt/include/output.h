#ifndef OUTPUT_H_
#define OUTPUT_H_

#include "logic.h"

class Output : public Logic
{
public:
    explicit Output(int pin);
    bool Evaluate() const override;

private:
    int _pin;
};

#endif