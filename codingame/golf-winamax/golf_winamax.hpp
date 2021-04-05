#ifndef GUARD_DPSG_GOLF_WINAMAX_HPP
#define GUARD_DPSG_GOLF_WINAMAX_HPP

#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <stack>
#include <tuple>
#include <unordered_set>
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
    return out << b.value;
  }

  ball& operator--() {
    --value;
    return *this;
  }

  friend constexpr ball operator-(ball b, int v) noexcept {
    return ball{b.value - v};
  }

  friend constexpr bool operator==(ball l, ball r) noexcept {
    return l.value == r.value;
  }
  friend constexpr bool operator!=(ball l, ball r) noexcept {
    return l.value != r.value;
  }
  friend constexpr bool operator<(ball l, ball r) noexcept {
    return l.value < r.value;
  }
};
struct hole_t {
  friend std::ostream& operator<<(std::ostream& out, hole_t) {
    return out << 'H';
  }
} constexpr hole{};

struct direction {
  enum { up, down, left, right } value;
  friend std::ostream& operator<<(std::ostream& out, direction p) {
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
  friend constexpr bool operator==(direction l, direction r) noexcept {
    return l.value == r.value;
  }
  friend constexpr bool operator<(direction l, direction r) noexcept {
    return l.value < r.value;
  }
  friend constexpr bool operator!=(direction l, direction r) noexcept {
    return l.value != r.value;
  }
} constexpr up{direction::up}, down{direction::down}, left{direction::left},
    right{direction::right};

namespace std {
template <>
class hash<::direction> {
  size_t operator()(::direction d) const noexcept {
    return hash<int>{}(d.value);
  }
};
template <>
class hash<::ball> {
  size_t operator()(::ball b) const noexcept { return hash<int>{}(b.value); }
};
}  // namespace std

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
    if (left._type != right._type) {
      return false;
    }
    if (left._type == type::ball) {
      return left == right._ball;
    }
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

    constexpr size_t x() const noexcept {
      return static_cast<size_t>(_offset) / _width;
    }
    constexpr size_t y() const noexcept {
      return static_cast<size_t>(_offset) % _width;
    }
  };

 public:
  using iterator = iterator_impl<value_type>;
  using const_iterator = iterator_impl<const_value_type>;
  using indexed_iterator = indexed_iterator_impl<value_type>;
  using indexed_const_iterator = indexed_iterator_impl<const_value_type>;

  constexpr iterator begin() {
    return iterator(static_cast<pointer>(_cells.get()));
  }
  constexpr const_iterator begin() const {
    return const_iterator(_cells.get());
  }
  constexpr const_iterator cbegin() const {
    return const_iterator(static_cast<const_pointer>(_cells.get()));
  }

  constexpr iterator end() {
    return iterator(static_cast<pointer>(_cells.get() + (_height * _width)));
  }
  constexpr const_iterator end() const {
    return const_iterator(
        static_cast<const_pointer>(_cells.get() + (_height * _width)));
  }
  constexpr const_iterator cend() const {
    return const_iterator(
        static_cast<const_pointer>(_cells.get() + (_height * _width)));
  }

  constexpr indexed_const_iterator indexed_begin() const {
    return indexed_const_iterator{_cells.get(), _width};
  }

  constexpr indexed_const_iterator indexed_end() const {
    return indexed_const_iterator{
        _cells.get(),
        _width,
        static_cast<typename indexed_const_iterator::difference_type>(size())};
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

  size_t size() const noexcept { return _width * _height; }

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
  enum class type { direction, empty };
  constexpr answer_cell() noexcept = default;
  constexpr answer_cell(direction p) noexcept
      : _type{type::direction}, _direction{p} {}
  constexpr answer_cell(empty_t) noexcept : _type{type::empty} {}

  friend constexpr bool operator==(answer_cell left, empty_t) noexcept {
    return left._type == type::empty;
  }

  friend constexpr bool operator==(answer_cell left, direction right) noexcept {
    return left._type == type::direction &&
           left._direction.value == right.value;
  }

  friend constexpr bool operator==(answer_cell left,
                                   answer_cell right) noexcept {
    return left._type == right._type &&
           (left._type == type::direction
                ? left._direction.value == right._direction.value
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
      out << cell._direction;
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
  type _type{type::empty};
  direction _direction{};
};

using answer = two_d_array<answer_cell>;

template <class T, class R = std::make_signed_t<T>>
constexpr R to_signed(T in) noexcept {
  return static_cast<R>(in);
}

template <class T, class R = std::make_unsigned_t<T>>
constexpr R to_unsigned(T in) noexcept {
  return static_cast<R>(in);
}

template <class T, class R = std::make_unsigned_t<T>>
constexpr R distance(const T& left, const T& right) noexcept {
  return to_unsigned(abs(to_signed(left) - to_signed(right)));
}

template <class T, class U>
constexpr size_t manathan_distance(const T& left, const U& right) noexcept {
  return distance(left.x, right.x) + distance(left.y, right.y);
}

using coordinates = std::pair<int, int>;
using path_point = std::pair<direction, ball>;
using path = std::vector<path_point>;
using path_list = std::vector<path>;
using hole_list = std::vector<coordinates>;
using coordinates_extractor = const int& (*)(const coordinates&);
constexpr coordinates_extractor x = std::get<0>;
constexpr coordinates_extractor y = std::get<1>;

coordinates advance(const coordinates& coord, direction dir, int length) {
  switch (dir.value) {
    case direction::left:
      return coordinates{coord.first, coord.second - length};
    case direction::right:
      return coordinates{coord.first, coord.second + length};
    case direction::up:
      return coordinates{coord.first - length, coord.second};
    case direction::down:
      return coordinates{coord.first + length, coord.second};
  }
  return coordinates{};
}

template <class T>
decltype(auto) at(T& t, const coordinates& c) {
  return t.at(to_unsigned(c.first), to_unsigned(c.second));
}

bool intersects(const coordinates& c, const path& path, const coordinates& o) {
  coordinates begin = o;
  coordinates end;
  for (const auto& p : path) {
    end = advance(begin, p.first, p.second.value);
    if (p.first == down && y(c) == y(end) && x(c) <= x(end) &&
        x(c) >= x(begin)) {
      return true;
    }
    if (p.first == up && y(c) == y(end) && x(c) >= x(end) && x(c) <= x(begin)) {
      return true;
    }
    if (p.first == right && x(c) == x(end) && y(c) <= y(end) &&
        y(c) >= y(begin)) {
      return true;
    }
    if (p.first == left && x(c) == x(end) && y(c) >= y(end) &&
        y(c) <= y(begin)) {
      return true;
    }

    begin = end;
  }
  return false;
}

// Find every possible valid path from origin to destination with ball_data,
// taking both field and answ as constraints
//
// Exhaustive dfs through the graph of possibilities to accumulate the options
// in a list of point + direction.
path_list find_path(const coordinates& origin,
                    const coordinates& destination,
                    ball ball_data,
                    const field& field,
                    answer& answ) {
  using point_to_explore = std::tuple<coordinates, ball, path>;
  std::stack<point_to_explore> to_explore;

  // we start from the origin, with the original amount of strikes
  to_explore.push(point_to_explore(origin, ball_data, path{}));
  path_list result;

  while (!to_explore.empty()) {
    point_to_explore current = std::move(to_explore.top());
    to_explore.pop();

    // for each point, we'll compute the 4 possible strikes and push the valid
    // ones back onto the stack
    for (auto d : {up, down, left, right}) {
      coordinates c = std::get<0>(current);
      ball b = std::get<1>(current);
      path& p = std::get<2>(current);
      for (int i = 0; i < b.value; ++i) {
        c = advance(c, d, 1);
        if (c.first < 0 || to_unsigned(c.first) >= field.height() ||
            c.second < 0 || to_unsigned(c.second) >= field.width() ||
            at(answ, c) != empty ||
            (c != destination && at(field, c) == hole) ||
            intersects(c, p, origin)) {
          // falling in here means this path is invalid, we can skip to the next
          // direction
          goto continue_outer_loop;  // tribute to A. Alexandreiscu, sue me
        }
      }

      // We reached destination, save this path as a solution
      if (c == destination) {
        auto copy = p;
        copy.push_back(std::make_pair(d, b));
        result.push_back(std::move(copy));
      }
      // The strike is valid
      // This was not the last strike and we didn't fall into the water
      else if (b != ball{1} && at(field, c) != water) {
        auto copy = p;
        copy.push_back(std::make_pair(d, b));
        to_explore.push(point_to_explore(c, b - 1, std::move(copy)));
      }
      // otherwise, the strike was invalid and we can drop it
    continue_outer_loop:;
    }
  }

  return result;
}

answer solve(const field& field) {
  answer result{field.width(), field.height()};
  struct ball_data {
    size_t x;
    size_t y;
    ball n;
  };
  struct hole_data {
    size_t x;
    size_t y;
  };
  std::vector<ball_data> balls;
  std::vector<hole_data> holes;

  for (auto it = field.indexed_begin(), end = field.indexed_end(); it != end;
       ++it) {
    switch (it->get_type()) {
      case cell::type::ball:
        balls.push_back(ball_data{it.x(), it.y(), it->ball_count()});
        break;
      case cell::type::hole:
        holes.push_back(hole_data{it.x(), it.y()});
        break;
      default:
        break;
    }
  }

  std::sort(balls.begin(),
            balls.end(),
            [](const ball_data& left, const ball_data& right) {
              return left.n.value < right.n.value;
            });

  for (auto ball_it = balls.begin(), ball_end = balls.end();
       ball_it != ball_end;) {
    auto& ball = *ball_it;
    sort(holes.begin(),
         holes.end(),
         [&ball](const hole_data& left, const hole_data& right) {
           return manathan_distance(left, ball) <
                  manathan_distance(right, ball);
         });

    for (auto hole_it = holes.begin(), hole_end = holes.end();
         hole_it != hole_end;) {
      auto& hole = *hole_it;

      // find all paths from ball to hole. if none try next hole
      // if found, exit loop and remove hole from available holes
      // need to consider all possible paths?
      ++hole_it;
    }
    // if all holes tested and no solution, backtrack.
    //     -> add back the hole used to the pool
    //        then from the next hole from that one in the list, repeat the
    //        process if all holes where exhausted, go back to the previous ball
    //        and backtrack that one
    // Else next ball
    ++ball_it;
  }

  return result;
}

#endif  // GUARD_DPSG_GOLF_WINAMAX_HPP