#ifndef BINARYLOGIC_H_
#define BINARYLOGIC_H_

#include "logic.h"

template <typename T>
class BinaryLogicOperation : public Logic
{
public:
    BinaryLogicOperation(Logic& left, Logic& right)
        : _left(left)
        , _right(right)
    {
        /* empty */
    }

    bool Evaluate() const override
    {
        return T::Evaluate(_left, _right);
    }

protected:
    Logic& _left;
    Logic& _right;
};

struct Andable
{
    static bool Evaluate(const Logic& left, const Logic& right)
    {
        return left.Evaluate() && right.Evaluate();
    }
};

struct Orable
{
    static bool Evaluate(const Logic& left, const Logic& right)
    {
        return left.Evaluate() || right.Evaluate();
    }
};

using And   = BinaryLogicOperation<Andable>;
using Or    = BinaryLogicOperation<Orable>;

#endif // BINARYLOGIC_H_