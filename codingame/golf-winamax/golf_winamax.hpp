#ifndef GUARD_DPSG_GOLF_WINAMAX_HPP
#define GUARD_DPSG_GOLF_WINAMAX_HPP
#include <algorithm>
#include <memory>
#include <vector>

struct water_t {
  friend std::ostream& operator<<(std::ostream& out, water_t) {
    return out << 'X';
  }
} water;
struct empty_t {
  friend std::ostream& operator<<(std::ostream& out, empty_t) {
    return out << '.';
  }
} empty;
struct ball {
  int value;
  friend std::ostream& operator<<(std::ostream& out, ball b) {
    return out << (static_cast<char>(b.value) + '0');
  }
};
struct hole_t {
  friend std::ostream& operator<<(std::ostream& out, hole_t) {
    return out << 'H';
  }
} hole;

struct path {
  enum { up, down, left, right } value;
  friend std::ostream& operator<<(std::ostream& out, path p) {
    switch (p.value) {
      case up:
        return out << '^';
      case down:
        return out << 'v';
      case right:
        return out << '>';
      case left:
        return out << '<';
    }
  }
} constexpr up{path::up}, down{path::down}, left{path::left},
    right{path::right};

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

  friend constexpr bool operator==(const cell& left,
                                   const cell& right) noexcept {
    if (left._type != right._type)
      return false;
    if (left._type == type::ball)
      return left == right._ball;
    return true;
  }

  template <class T>
  friend constexpr bool operator!=(const cell& c, T t) noexcept {
    return !(c == t);
  }

  friend std::istream& operator>>(std::istream& in, cell& cell) noexcept {
    char c;
    in >> c;
    if (c >= '0' && c <= '9') {
      cell = {ball{c - '0'}};
    }
    else if (c == 'X') {
      cell = {water};
    }
    else if (c == 'H') {
      cell = {hole};
    }
    else {
      cell = {empty};
    }
    return in;
  }

  friend std::ostream& operator<<(std::ostream& out,
                                  const cell& cell) noexcept {
    switch (cell._type) {
      case type::water:
        return out << water;
      case type::hole:
        return out << hole;
      case type::ball:
        return out << cell._ball;
      case type::empty:
        return out << empty;
    }
  }

 private:
  type _type;
  ball _ball{};
};

template <class T>
struct two_d_array {
  using value_type = T;
  two_d_array(size_t width, size_t height)
      : _width{width},
        _height{height},
        _cells(std::make_unique<value_type[]>(width * height)) {}

  value_type& at(size_t x, size_t y) noexcept { return _cells[x * _width + y]; }
  const value_type& at(size_t x, size_t y) const noexcept {
    return const_cast<two_d_array*>(this)->at(x, y);
  }

  constexpr size_t height() const noexcept { return _height; }
  constexpr size_t width() const noexcept { return _width; }

 private:
  struct iterator_impl {};

 public:
  friend std::ostream& operator<<(std::ostream& out, const two_d_array& array) {
    for (size_t i = 0; i < array.height(); ++i) {
      for (size_t j = 0; j < array.width(); ++j) {
        out << array.at(i, j);
      }
      out << '\n';
    }
    return out;
  }

  friend std::istream& operator>>(std::istream& in, two_d_array& array) {
    for (size_t i = 0; i < array._height; ++i) {
      for (size_t j = 0; j < array._width; ++j) {
        value_type c;
        in >> c;
        array.at(i, j) = std::move(c);
      }
      in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
    return in;
  }

  friend bool operator==(const two_d_array& left,
                         const two_d_array& right) noexcept {
    if (left._height != right._height || left._width != right._width)
      return false;

    for (size_t i = 0; i < left._height; ++i) {
      for (size_t j = 0; j < left._width; ++j) {
        if (left.at(i, j) != right.at(i, j)) {
          return false;
        }
      }
    }
    return true;
  }

  friend bool operator!=(const two_d_array& left,
                         const two_d_array& right) noexcept {
    return !(left == right);
  }

 private:
  size_t _width;
  size_t _height;
  std::unique_ptr<T[]> _cells;
};

using field = two_d_array<cell>;

field parse_field(std::istream& in, size_t width, size_t height) {
  field result(width, height);
  in >> result;
  return result;
}

struct answer_cell {
  enum class type { path, empty };
  constexpr answer_cell() noexcept : _type{type::empty} {}
  constexpr answer_cell(path p) noexcept : _type{type::path}, _path{p} {}
  constexpr answer_cell(empty_t) noexcept : _type{type::empty} {}

  friend constexpr bool operator==(answer_cell left, empty_t) noexcept {
    return left._type == type::empty;
  }

  friend constexpr bool operator==(answer_cell left, path right) noexcept {
    return left._type == type::path && left._path.value == right.value;
  }

  friend constexpr bool operator==(answer_cell left,
                                   answer_cell right) noexcept {
    return left._type == right._type &&
           (left._type == type::path ? left._path.value == right._path.value
                                     : true);
  }

  template <class T>
  friend constexpr bool operator!=(answer_cell left, T right) noexcept {
    return !(left == right);
  }

  friend std::ostream& operator<<(std::ostream& out, answer_cell cell) {
    if (cell._type == type::empty) {
      out << empty;
    }
    else {
      out << cell._path;
    }
    return out;
  }

  friend std::istream& operator>>(std::istream& in, answer_cell& cell) {
    char c;
    in >> c;
    if (c == '^') {
      cell = {up};
    }
    else if (c == '>') {
      cell = {right};
    }
    else if (c == '<') {
      cell = {left};
    }
    else if (c == 'v') {
      cell = {down};
    }
    else {
      cell = {empty};
    }
    return in;
  }

 private:
  type _type;
  path _path{};
};

using answer = two_d_array<answer_cell>;

#endif  // GUARD_DPSG_GOLF_WINAMAX_HPP