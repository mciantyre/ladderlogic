#ifndef LOGIC_H_
#define LOGIC_H_

class Logic
{
public:
    
    virtual ~Logic() { /* empty */ }
    virtual bool Evaluate();
};

#endif // LOGIC_H_