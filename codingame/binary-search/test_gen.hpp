#ifndef GUARD_DPSG_TEST_GEN_HPP
#define GUARD_DPSG_TEST_GEN_HPP

#include "./binary_search.hpp"

#include <cstring>
#include <random>
#include <sstream>
#include <stdexcept>

class test {
 public:
  constexpr static size_t default_max_tries = 1000;
  inline explicit test(size_t width,
                       size_t height,
                       coordinates solution,
                       coordinates position,
                       size_t allowed_tries = default_max_tries)
      : _height{height},
        _width{width},
        _position{position},
        _solution{solution},
        _allowed{allowed_tries} {
    if (position.x >= width || position.y >= height) {
      throw std::out_of_range("Initial position outside the search space");
    }
    if (solution.x >= width || solution.y >= height) {
      throw std::out_of_range("Solution outside the search space");
    }
  }
  enum result { found, invalid, in_progress };

  inline temperature check(const coordinates& answer) {
    if (_allowed-- == 0) {
      throw std::runtime_error("Available tries exceeded");
    }

    if (answer.x >= _width || answer.y >= _height) {
      throw std::out_of_range("Answer outside the search space");
    }

    auto last_position = std::exchange(_position, answer);
    if (_position == _solution) {
      return temperature::found;
    }

    switch (relative_distance_from(_solution, _position, last_position)) {
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

  static test generate(size_t w = 10000, size_t h = 10000) {
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

  size_t width() const { return _width; }
  size_t height() const { return _height; }

  coordinates current_position() const { return _position; }
  coordinates solution() const { return _solution; }

  size_t remaining_tries() const { return _allowed; }

 private:
  size_t _height;
  size_t _width;
  coordinates _position;  // last recorded position
  coordinates _solution;
  size_t _allowed;

  static std::minstd_rand _engine;

  static size_t _generate(size_t min, size_t max) {
    std::uniform_int_distribution<size_t> gen(min, max);
    return gen(_engine);
  }
};

std::minstd_rand test::_engine{};

class fake_referee : public std::streambuf {
 public:
  fake_referee(test t) : _test(std::move(t)) {
    _resetp();
    std::stringstream in;
    in << _test.width() << " " << _test.height() << std::endl;
    in << _test.remaining_tries() << std::endl;
    in << _test.current_position().x << " " << _test.current_position().y
       << std::endl;
    in << "UNKNOWN" << std::endl;

    const auto& str = in.str();
    for (size_t i = 0; i < sizeof(_get_area) && i < str.size(); ++i) {
      _get_area[i] = str[i];
    }
    _resetg(strlen(_get_area));
  }

  const test& underlying_test() const { return _test; }

 private:
  int_type underflow() override { return static_cast<int_type>('\n'); }

  int_type sync() override {
    std::stringstream in;
    in << _put_area;
    coordinates cs;
    in >> cs;

    std::stringstream out;
    out << _test.check(cs);

    _resetp();
    for (char& c : _put_area) {
      c = 0;
    }
    for (size_t i = 0; i < sizeof(_get_area); ++i) {
      _get_area[i] = 0;
    }

    out >> _get_area;
    _resetg(strlen(_get_area));

    return 0;
  }

  void _resetp() { setp(_getp(), _getp() + sizeof(_put_area)); }
  void _resetg(size_t n = sizeof(_get_area)) {
    setg(_getg(), _getg(), _getg() + n);
  }
  char_type* _getg() { return static_cast<char_type*>(_get_area); }
  char_type* _getp() { return static_cast<char_type*>(_put_area); }

  char _put_area[128]{};
  char _get_area[128]{};

  test _test;
};

#endif  // GUARD_DPSG_TEST_GEN_HPP