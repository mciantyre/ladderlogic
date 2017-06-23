#ifndef LOGIC_H_
#define LOGIC_H_

class Logic
{
public:
    
    virtual ~Logic() { /* empty */ }

    /**
     * @brief Evaluate the logic statement.
     */
    virtual bool Evaluate() const = 0;
};

#endif // LOGIC_H_