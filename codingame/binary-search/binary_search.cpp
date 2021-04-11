#include <gtest/gtest.h>

#include "./binary_search.hpp"

#include <sstream>

using namespace std;
using ss = stringstream;
using c = coordinates;

TEST(Output, ShouldPrintCoordinatesCorrectly) {
  ss in;
  coordinates c{4, 2};
  in << c;
  ASSERT_EQ(in.str(), "4 2");
}
TEST(Coordinates, BasicEquality) {
  ASSERT_EQ((c{1, 1}), (c{1, 1}));
  ASSERT_EQ((c{9, 1}), (c{9, 1}));
  ASSERT_EQ((c{9, 42}), (c{9, 42}));

  ASSERT_NE((c{1, 1}), (c{2, 2}));
  ASSERT_NE((c{11, 1}), (c{11, 2}));
  ASSERT_NE((c{1, 42}), (c{2, 2}));
}

TEST(Middle, ShouldComputeTheCorrectMiddlePoint) {
  ASSERT_EQ(middle({0, 0}, {4, 4}), (c{2, 2}));
  ASSERT_EQ(middle({0, 0}, {1, 1}), (c{0, 0}));
  ASSERT_EQ(middle({4, 4}, {4, 4}), (c{4, 4}));
  ASSERT_EQ(middle({1, 3}, {8, 5}), (c{4, 4}));
}