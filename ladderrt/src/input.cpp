#include "input.h"

Input::Input(int pin)
    : _pin(pin)
{
    
}

bool Input::Evaluate() const
{
    return _pin != 0;
}