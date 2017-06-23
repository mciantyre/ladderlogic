#include "gtest/gtest.h"
#include "binarylogic.h"
#include "not.h"
#include "stublogic.h"

using namespace testing;

struct TruthTable
{
    bool left;
    bool right;
    bool expected;
};

static TruthTable AndTruthTable[] = {
    TruthTable{ true,   true,   true },
    TruthTable{ true,   false,  false },
    TruthTable{ false,  true,   false },
    TruthTable{ false,  false,  false }
};

static TruthTable OrTruthTable[] = {
    TruthTable{ true,   true,   true },
    TruthTable{ true,   false,  true },
    TruthTable{ false,  true,   true },
    TruthTable{ false,  false,  false }
};

class AndTest   : public TestWithParam<TruthTable> {};
class OrTest    : public TestWithParam<TruthTable> {};

TEST_P(AndTest, MatchesAndTruthTable)
{
    auto table = GetParam();
    StubLogic left(table.left);
    StubLogic right(table.right);
    And a(left, right);
    ASSERT_EQ(a.Evaluate(), table.expected);
}

TEST_P(OrTest, MatchesOrTruthTable)
{
    auto table = GetParam();
    StubLogic left(table.left);
    StubLogic right(table.right);
    Or o(left, right);
    ASSERT_EQ(o.Evaluate(), table.expected);
}

TEST(NotTest, TrueFalse)
{
    StubLogic l(true);
    Not n(l);
    ASSERT_FALSE(n.Evaluate());
}

TEST(NotTest, FalseTrue)
{
    StubLogic l(false);
    Not n(l);
    ASSERT_TRUE(n.Evaluate());
}

INSTANTIATE_TEST_CASE_P(LogicTest, AndTest, ValuesIn(AndTruthTable));
INSTANTIATE_TEST_CASE_P(LogicTest, OrTest, ValuesIn(OrTruthTable));

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}