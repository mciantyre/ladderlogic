#include "gtest/gtest.h"
#include "logicmanager.h"
#include "binarylogic.h"
#include "not.h"
#include "stublogic.h"

using namespace testing;

class LogicManagerTests : public Test
{
public:
    void SetUp() override
    {
        LogicManager::Reset();
    }
    
    void TearDown() override
    {
        LogicManager::Reset();
    }
};

TEST_F(LogicManagerTests, MakesNot)
{
    StubLogic logic(true);
    auto& n = LogicManager::Make<Not>(logic);
    ASSERT_FALSE(n.Evaluate());
}

TEST_F(LogicManagerTests, MakesAnd)
{
    StubLogic t(true), f(false);
    auto& a = LogicManager::Make<And>(t, f);
    ASSERT_FALSE(a.Evaluate());
}

TEST_F(LogicManagerTests, MakesRecursiveAnd)
{
    StubLogic t(true), tt(true), f(false);
    auto& a = LogicManager::Make<And>(t, tt);
    auto& b = LogicManager::Make<And>(a, f);
    ASSERT_TRUE(a.Evaluate());
    ASSERT_FALSE(b.Evaluate());
}

TEST_F(LogicManagerTests, MakesOr)
{
    StubLogic t(true), f(false);
    auto& o = LogicManager::Make<Or>(t, f);
    ASSERT_TRUE(o.Evaluate());
}

TEST_F(LogicManagerTests, MakesRecursiveOrs)
{
    StubLogic t(true), f(false);
    auto& o = LogicManager::Make<Or>(t, f);
    auto& no = LogicManager::Make<Not>(o);
    auto& oo = LogicManager::Make<Or>(no, f);
    ASSERT_TRUE(o.Evaluate());
    ASSERT_FALSE(no.Evaluate());
    ASSERT_FALSE(oo.Evaluate());
}