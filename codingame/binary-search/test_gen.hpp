#ifndef GUARD_DPSG_TEST_GEN_HPP
#define GUARD_DPSG_TEST_GEN_HPP

#include <random>
#include <stdexcept>
#include "./binary_search.hpp"

struct test {
  inline explicit test(size_t width,
                       size_t height,
                       coordinates solution,
                       coordinates position)
      : height{height}, width{width}, position{position}, solution{solution} {
    if (position.x >= width || position.y >= height) {
      throw std::out_of_range("Initial position outside the search space");
    }
    if (solution.x >= width || solution.y >= height) {
      throw std::out_of_range("Solution outside the search space");
    }
  }
  enum result { found, invalid, in_progress };

  inline temperature check(const coordinates& answer) {
    return temperature::unknown;
  }

 private:
  size_t height;
  size_t width;
  coordinates position;
  coordinates solution;
};

#endif  // GUARD_DPSG_TEST_GEN_HPP