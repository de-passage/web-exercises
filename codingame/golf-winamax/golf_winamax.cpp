#include <gtest/gtest.h>
#include "./golf_winamax.hpp"

#include <sstream>

using ss = std::stringstream;

TEST(Field, ShouldParseTrivialExample) {
  ss input("1H");
  auto f = parse_field(input, 2, 1);

  ASSERT_TRUE(f.at(0, 0) == ball{1});
  ASSERT_TRUE(f.at(0, 1) == hole);
}

TEST(Field, ShouldParseMoreComplicatedExample) {
  ss input;
  input << "2.X" << std::endl;
  input << "..H" << std::endl;
  input << ".H1" << std::endl;

  auto f = parse_field(input, 3, 3);

  for (size_t i = 0; i < 3; ++i)
    for (size_t j = 0; j < 3; ++j) {
      auto c = f.at(i, j);
      if (i == 0 && j == 0) {
        ASSERT_EQ(c, ball{2});
      }
      else if (i == 2 && j == 2) {
        ASSERT_EQ(c, ball{1});
      }
      else if (i == 0 && j == 2) {
        ASSERT_EQ(c, water);
      }
      else if ((i == 1 && j == 2) || (i == 2 && j == 1)) {
        ASSERT_EQ(c, hole);
      }
      else {
        ASSERT_EQ(c, empty);
      }
    }
}

TEST(Cell, BasicEqualityResults) {
  ASSERT_EQ(cell{empty}, empty);
  ASSERT_EQ(cell{water}, water);
  ASSERT_EQ(cell{hole}, hole);
  ASSERT_EQ(cell{ball{2}}, ball{2});

  ASSERT_NE(cell{empty}, water);
  ASSERT_NE(cell{ball{1}}, hole);
  ASSERT_NE(cell{ball{1}}, ball{2});
  ASSERT_EQ(cell{water}, ball{1});
}