#ifndef GUARD_DPSG_TEST_GEN_HPP
#define GUARD_DPSG_TEST_GEN_HPP

#include <random>
#include <stdexcept>
#include "./binary_search.hpp"

class test {
 public:
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
    if (answer.x >= width || answer.y >= height) {
      throw std::out_of_range("Answer outside the search space");
    }

    switch (relative_distance_from(answer, position, solution)) {
      case ordering::equal:
        return temperature::same;
      case ordering::lesser:
        return temperature::hot;
      case ordering::greater:
        return temperature::cold;
      default:
        return temperature::unknown;
    }
  }

  static test generate(size_t w, size_t h) {
    size_t height = _generate(1, h);
    size_t width = _generate(1, w);

    size_t tot = height * width;
    size_t sol = _generate(0, tot);
    size_t ini = _generate(0, tot - 1);
    if (ini >= sol) {
      ini += 1;
    }

    return test{
        width, height, {sol / width, sol % width}, {ini / width, ini % width}};
  }

 private:
  size_t height;
  size_t width;
  coordinates position;  // last recorded position
  coordinates solution;

  static std::minstd_rand _engine;

  static size_t _generate(size_t min, size_t max) {
    std::uniform_int_distribution<size_t> gen(min, max);
    return gen(_engine);
  }
};

std::minstd_rand test::_engine{};

#endif  // GUARD_DPSG_TEST_GEN_HPP