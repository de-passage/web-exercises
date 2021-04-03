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
  ASSERT_NE(cell{water}, ball{1});
}

TEST(AnswerCell, BasicEqualityResults) {
  ASSERT_EQ(answer_cell{empty}, empty);
  ASSERT_EQ(answer_cell{up}, up);
  ASSERT_EQ(answer_cell{down}, down);
  ASSERT_EQ(answer_cell{left}, left);
  ASSERT_EQ(answer_cell{right}, right);

  ASSERT_NE(answer_cell{left}, up);
  ASSERT_NE(answer_cell{up}, down);
  ASSERT_NE(answer_cell{right}, left);
  ASSERT_NE(answer_cell{down}, right);
  ASSERT_NE(answer_cell{empty}, up);
  ASSERT_NE(answer_cell{right}, empty);
}

TEST(Answer, SerializesProperly) {
  ss out;
  answer a(3, 3);
  a.at(0, 0) = down;
  a.at(1, 0) = down;
  a.at(2, 0) = right;
  a.at(2, 2) = up;

  out << a;
  ASSERT_EQ(out.str(), "v..\nv..\n>.^\n");
}

TEST(Answer, EqualiyShouldBehaveProperly) {
  ss in;
  in << "v.." << std::endl;
  in << "v.." << std::endl;
  in << ">.^" << std::endl;
  answer a1(3, 3), a2(3, 3);

  in >> a1;
  a2.at(0, 0) = down;
  a2.at(1, 0) = down;
  a2.at(2, 0) = right;
  a2.at(2, 2) = up;

  ASSERT_EQ(a1, a2);
}

TEST(Field, EqualityShouldBehaveProperly) {
  ss input;
  input << "2.X" << std::endl;
  input << "..H" << std::endl;
  input << ".H1" << std::endl;
  field f1(3, 3);
  input >> f1;
  field f2(3, 3);
  f2.at(0, 0) = ball{2};
  f2.at(0, 2) = water;
  f2.at(1, 2) = hole;
  f2.at(2, 1) = hole;
  f2.at(2, 2) = ball{1};

  ASSERT_EQ(f1, f2);
}