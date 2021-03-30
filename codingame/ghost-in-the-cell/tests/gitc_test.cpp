#include <gtest/gtest.h>

#include "ghost_in_the_cell.hpp"

using namespace gitc;

TEST(Types, ConvertToAndFromDouble) {
  strength test{10};
  ASSERT_EQ(test * 1.3, 13);
}

TEST(Types, AreInterconvertibleWhenSpecified) {
  duration d{4};
  production_capacity p{3};
  ASSERT_EQ(d * p, strength{12});
}