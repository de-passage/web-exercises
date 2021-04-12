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

#endif  // GUARD_DPSG_BINARY_SEARCH_HPP