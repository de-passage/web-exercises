#ifndef GUARD_DPSG_BINARY_SEARCH_HPP
#define GUARD_DPSG_BINARY_SEARCH_HPP
#include <iostream>

struct coordinates {
  size_t x;
  size_t y;

  friend bool operator==(const coordinates& left,
                         const coordinates& right) noexcept {
    return left.x == right.x && left.y == right.y;
  }
  friend bool operator!=(const coordinates& left,
                         const coordinates& right) noexcept {
    return !(left == right);
  }
};
inline std::ostream& operator<<(std::ostream& out, const coordinates& coords) {
  return out << coords.x << " " << coords.y;
}

inline coordinates middle(const coordinates& top_left,
                          const coordinates& bottom_right) {
  return coordinates{(top_left.x + bottom_right.x) / 2,
                     (top_left.y + bottom_right.y) / 2};
}

enum class ordering : unsigned char {
  equal = 1,
  greater = 2,
  lesser = 4,
  greater_equal = equal | greater,
  lesser_equal = equal | lesser
};

std::ostream& operator<<(std::ostream& out, ordering ord) {
  switch (ord) {
    case ordering::equal:
      return out << "equal";
    case ordering::greater:
      return out << "greater";
    case ordering::lesser:
      return out << "lesser";
    default:
      return out;
  }
}

inline size_t distance_squared(const coordinates& left,
                               const coordinates& right) {
  auto x = left.x - right.x;
  auto y = left.y - right.y;
  return x * x + y * y;
}

inline ordering relative_distance_from(const coordinates& origin,
                                       const coordinates& left,
                                       const coordinates& right) {
  auto d1 = distance_squared(origin, left);
  auto d2 = distance_squared(origin, right);

  if (d1 == d2) {
    return ordering::equal;
  }
  else if (d1 < d2) {
    return ordering::lesser;
  }
  else {
    return ordering::greater;
  }
}

enum class temperature { hot, cold, same, unknown };
inline std::ostream& operator<<(std::ostream& out, temperature t) {
  switch (t) {
    case temperature::hot:
      return out << "WARMER";
    case temperature::same:
      return out << "SAME";
    case temperature::cold:
      return out << "COLDER";
    default:
      return out << "UNKNOWN";
  }
}

inline std::istream& operator>>(std::istream& in, temperature& tmp) {
  std::string dir;
  in >> dir;
  if (dir == "COLDER") {
    tmp = temperature::cold;
  }
  else if (dir == "WARMER") {
    tmp = temperature::hot;
  }
  else if (dir == "SAME") {
    tmp = temperature::same;
  }
  else {
    tmp = temperature::unknown;
  }
  return in;
}

inline temperature get_temperature(std::istream& in) {
  temperature tmp;
  in >> tmp;
  in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  return tmp;
}

inline coordinates divide(const coordinates& top_left,
                          const coordinates& bottom_right) {
  size_t width = std::max(top_left.x, bottom_right.x) -
                 std::min(top_left.x, bottom_right.x);
  size_t height = std::max(top_left.y, bottom_right.y) -
                  std::min(top_left.y, bottom_right.y);

  if (width >= height) {
    return {(top_left.x + bottom_right.x) / 2, bottom_right.y};
  }
  return {bottom_right.x, (top_left.y + bottom_right.y) / 2};
}

inline coordinates search(const coordinates& current,
                          const coordinates& top_left,
                          const coordinates& bottom_right) {
  if (current.x < top_left.x || current.y < top_left.y ||
      current.x >= bottom_right.x || current.y >= bottom_right.y) {
    // we're out of the search space == last one was cold, we need to go inside,
    // in the middle of one of the halves

    return middle(top_left, divide(top_left, bottom_right));
  }
  // we're inside the search space, we need to jump to the other side of the
  // dividing line

  auto middle_point = middle(top_left, bottom_right);

  coordinates result = {bottom_right.x + top_left.x - current.x,
                        top_left.y + bottom_right.y - current.y};
  if (middle_point.x % 2 == 1 && result.x == middle_point.x + 1) {
    // we're on the middle vertical line and we haven't explored the whole space
    if (result.x > top_left.x) {
      --result.x;
    }
    else if (result.x < bottom_right.x) {
      ++result.x;
    }
  }
  if (middle_point.y % 2 == 1 && result.y == middle_point.y + 1) {
    // we're on the middle horizontal line
    if (result.y > top_left.y) {
      --result.y;
    }
    else if (result.y < bottom_right.y) {
      ++result.y;
    }
  }

  return result;
}

#endif  // GUARD_DPSG_BINARY_SEARCH_HPP