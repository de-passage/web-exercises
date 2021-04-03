#ifndef GUARD_DPSG_GOLF_WINAMAX_HPP
#define GUARD_DPSG_GOLF_WINAMAX_HPP
#include <algorithm>
#include <memory>
#include <vector>

struct water_t {
} water;
struct empty_t {
} empty;
struct ball {
  int value;
};
struct hole_t {
} hole;

struct cell {
  enum class type { empty, water, ball, hole };
  constexpr cell() noexcept : cell(empty) {}
  constexpr cell(water_t) noexcept : _type{type::water} {}
  constexpr cell(empty_t) noexcept : _type{type::empty} {}
  constexpr cell(hole_t) noexcept : _type{type::hole} {}
  constexpr cell(ball b) noexcept : _type{type::ball}, _ball{b} {}

  constexpr type get_type() const noexcept { return _type; }
  constexpr ball ball_count() const noexcept {
    assert(_type == type::ball);
    return _ball;
  }

  friend constexpr bool operator==(const cell& c, water_t) noexcept {
    return c.get_type() == type::water;
  }

  friend constexpr bool operator==(const cell& c, hole_t) noexcept {
    return c.get_type() == type::hole;
  }

  friend constexpr bool operator==(const cell& c, empty_t) noexcept {
    return c.get_type() == type::empty;
  }

  friend constexpr bool operator==(const cell& c, ball b) noexcept {
    return c.get_type() == type::ball && c.ball_count().value == b.value;
  }

  template <class T>
  friend constexpr bool operator!=(const cell& c, T t) noexcept {
    return !(c == t);
  }

 private:
  type _type;
  ball _ball{};
};

struct field {
  field(size_t width, size_t height)
      : _width{width},
        _height{height},
        _cells(std::make_unique<cell[]>(width * height)) {}

  cell& at(size_t x, size_t y) noexcept { return _cells[x * _width + y]; }
  const cell& at(size_t x, size_t y) const noexcept {
    return const_cast<field*>(this)->at(x, y);
  }

  constexpr size_t height() const noexcept { return _height; }
  constexpr size_t width() const noexcept { return _width; }

 private:
  size_t _width;
  size_t _height;
  std::unique_ptr<cell[]> _cells;
};

field parse_field(std::istream& in, size_t width, size_t height) {
  field result(width, height);
  for (size_t i = 0; i < height; ++i) {
    for (size_t j = 0; j < width; ++j) {
      char c;
      in >> c;
      if (c >= '0' && c <= '9') {
        result.at(i, j) = ball{c - '0'};
      }
      else if (c == 'H') {
        result.at(i, j) = hole;
      }
      else if (c == 'X') {
        result.at(i, j) = water;
      }
    }
    in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }
  return result;
}

#endif  // GUARD_DPSG_GOLF_WINAMAX_HPP