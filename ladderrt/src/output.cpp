#include "output.h"

Output::Output(int pin)
    : _pin(pin)
{

}

bool Output::Evaluate() const
{
    return _pin == 0;
}