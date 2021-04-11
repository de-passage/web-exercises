#include <gtest/gtest.h>

#include "./binary_search.hpp"

#include <sstream>

using namespace std;
using ss = stringstream;

TEST(Output, ShouldPrintCoordinatesCorrectly) {
  ss in;
  coordinates c{4, 2};
  in << c;
  ASSERT_EQ(in.str(), "4 2");
}
TEST(Coordinates, BasicEquality) {
  using c = coordinates;

  ASSERT_EQ((c{1, 1}), (c{1, 1}));
  ASSERT_EQ((c{9, 1}), (c{9, 1}));
  ASSERT_EQ((c{9, 42}), (c{9, 42}));

  ASSERT_NE((c{1, 1}), (c{2, 2}));
  ASSERT_NE((c{11, 1}), (c{11, 2}));
  ASSERT_NE((c{1, 42}), (c{2, 2}));
}