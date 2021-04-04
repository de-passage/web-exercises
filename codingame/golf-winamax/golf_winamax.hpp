#ifndef GUARD_DPSG_GOLF_WINAMAX_HPP
#define GUARD_DPSG_GOLF_WINAMAX_HPP

#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <vector>

struct water_t {
  friend std::ostream& operator<<(std::ostream& out, water_t) {
    return out << 'X';
  }
} constexpr water{};
struct empty_t {
  friend std::ostream& operator<<(std::ostream& out, empty_t) {
    return out << '.';
  }
} constexpr empty{};
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
} constexpr hole{};

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

  friend std::istream& operator>>(std::istream& in, cell& cell) {
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

  friend std::ostream& operator<<(std::ostream& out, const cell& cell) {
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
  using const_value_type = std::add_const_t<value_type>;
  using pointer = value_type*;
  using const_pointer = const_value_type*;

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
  template <class V>
  struct iterator_impl {
    using value_type = V;
    using pointer = value_type*;
    using reference = value_type&;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    constexpr iterator_impl() noexcept = default;
    constexpr iterator_impl(const iterator_impl&) noexcept = default;
    constexpr iterator_impl(iterator_impl&&) noexcept = default;
    constexpr iterator_impl& operator=(const iterator_impl&) noexcept = default;
    constexpr iterator_impl& operator=(iterator_impl&&) noexcept = default;
    ~iterator_impl() = default;

   private:
    constexpr iterator_impl(pointer ptr) noexcept : _ptr{ptr} {}
    friend two_d_array;

    pointer _ptr;

   public:
    constexpr iterator_impl& operator++() noexcept {
      ++_ptr;
      return *this;
    }
    constexpr iterator_impl operator++(int) noexcept {
      return iterator_impl{_ptr++};
    }

    constexpr iterator_impl& operator--() noexcept {
      --_ptr;
      return *this;
    }
    constexpr iterator_impl operator--(int) noexcept {
      return iterator_impl{_ptr--};
    }

    constexpr iterator_impl& operator+=(difference_type d) noexcept {
      _ptr += d;
      return *this;
    }

    constexpr iterator_impl& operator-=(difference_type d) noexcept {
      _ptr -= d;
      return *this;
    }

    constexpr reference operator*() const noexcept { return *_ptr; }
    constexpr pointer operator->() const noexcept { return _ptr; }

    friend constexpr bool operator==(iterator_impl left,
                                     iterator_impl right) noexcept {
      return left._ptr == right._ptr;
    }

    friend constexpr bool operator!=(iterator_impl left,
                                     iterator_impl right) noexcept {
      return left._ptr != right._ptr;
    }
  };

  template <class V>
  struct indexed_iterator_impl {
    using value_type = V;
    using pointer = value_type*;
    using reference = value_type&;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    constexpr indexed_iterator_impl() noexcept = default;
    constexpr indexed_iterator_impl(const indexed_iterator_impl&) noexcept =
        default;
    constexpr indexed_iterator_impl(indexed_iterator_impl&&) noexcept = default;
    constexpr indexed_iterator_impl& operator=(
        const indexed_iterator_impl&) noexcept = default;
    constexpr indexed_iterator_impl& operator=(
        indexed_iterator_impl&&) noexcept = default;
    ~indexed_iterator_impl() = default;

   private:
    constexpr indexed_iterator_impl(pointer ptr,
                                    size_t width,
                                    difference_type offset = 0) noexcept
        : _ptr{ptr}, _offset{offset}, _width{width} {}
    friend two_d_array;

    pointer _ptr{};
    difference_type _offset{};
    size_t _width{};

   public:
    constexpr indexed_iterator_impl& operator++() noexcept {
      ++_offset;
      return *this;
    }
    constexpr indexed_iterator_impl operator++(int) noexcept {
      return indexed_iterator_impl{_ptr, _width, _offset++};
    }

    constexpr indexed_iterator_impl& operator--() noexcept {
      --_offset;
      return *this;
    }
    constexpr indexed_iterator_impl operator--(int) noexcept {
      return indexed_iterator_impl{_ptr, _width, _offset--};
    }

    constexpr indexed_iterator_impl& operator+=(difference_type d) noexcept {
      _offset += d;
      return *this;
    }

    constexpr indexed_iterator_impl& operator-=(difference_type d) noexcept {
      _offset -= d;
      return *this;
    }

    constexpr reference operator*() const noexcept { return *(_ptr + _offset); }
    constexpr pointer operator->() const noexcept { return _ptr + _offset; }

    friend constexpr bool operator==(
        const indexed_iterator_impl& left,
        const indexed_iterator_impl& right) noexcept {
      return left._ptr == right._ptr && left._offset == right._offset;
    }

    friend constexpr bool operator!=(
        const indexed_iterator_impl& left,
        const indexed_iterator_impl& right) noexcept {
      return !(left == right);
    }

    constexpr size_t x() const noexcept { return _offset / _width; }
    constexpr size_t y() const noexcept { return _offset % _width; }
  };

 public:
  using iterator = iterator_impl<value_type>;
  using const_iterator = iterator_impl<std::add_const_t<value_type>>;

  constexpr iterator begin() {
    return iterator_impl<value_type>(static_cast<pointer>(_cells.get()));
  }
  constexpr const_iterator begin() const {
    return iterator_impl<const_value_type>(_cells.get());
  }
  constexpr const_iterator cbegin() const {
    return iterator_impl<const_value_type>(
        static_cast<const_pointer>(_cells.get()));
  }

  constexpr iterator end() {
    return iterator_impl<value_type>(
        static_cast<pointer>(_cells.get() + (_height * _width)));
  }
  constexpr const_iterator end() const {
    return iterator_impl<const_value_type>(
        static_cast<const_pointer>(_cells.get() + (_height * _width)));
  }
  constexpr const_iterator cend() const {
    return iterator_impl<const_value_type>(
        static_cast<const_pointer>(_cells.get() + (_height * _width)));
  }

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

    for (auto beg1 = left.begin(), beg2 = right.begin(), e1 = left.end();
         beg1 != e1;
         ++beg1, ++beg2) {
      if (*beg1 != *beg2) {
        return false;
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

field parse_field(std::istream& in) {
  size_t width, height;
  in >> width >> height;
  in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
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

answer solve(const field& field) {
  answer result{field.width(), field.height()};
  // for (ball_sorted_by_strikes ball : field) {
  //   hs = available holes sorted by manathan distance find_shortest_path(
  //       ball, nearest_holes);
  //   if
  //     not found backtrack
  // }
  return result;
}

#endif  // GUARD_DPSG_GOLF_WINAMAX_HPP