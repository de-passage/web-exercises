#ifndef GUARD_DPSG_CVSZ_TYPES_HPP
#define GUARD_DPSG_CVSZ_TYPES_HPP

#include <unordered_map>

namespace cvsz {
struct coordinates {
  int x;
  int y;
};
struct positionable {
  coordinates position;

  int x() const { return position.x; }
  int y() const { return position.y; }
};
struct zombie : positionable {
  constexpr zombie(const coordinates& pos, const coordinates& target) noexcept
      : positionable{pos}, target{target} {}
  coordinates target;
};
struct human : positionable {
  constexpr human(const coordinates& pos) noexcept : positionable{pos} {}
  constexpr human(int x, int y) noexcept : positionable{{x, y}} {}
};
struct ash : positionable {};

struct id {
  int value;
  friend bool operator==(id left, id right) {
    return left.value == right.value;
  }
  friend bool operator<(id left, id right) { return left.value < right.value; }
};

template <class T>
using id_indexed_map = std::unordered_map<id, T>;
using human_container = id_indexed_map<human>;
using zombie_container = id_indexed_map<zombie>;

}  // namespace cvsz

#endif  // GUARD_DPSG_CVSZ_TYPES_HPP