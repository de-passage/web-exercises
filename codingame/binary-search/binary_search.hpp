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
std::ostream& operator<<(std::ostream& out, const coordinates& coords) {
  return out << coords.x << " " << coords.y;
}

coordinates middle(const coordinates& top_left,
                   const coordinates& bottom_right) {
  return coordinates{top_left.x + bottom_right.x / 2,
                     (top_left.y + bottom_right.y) / 2};
}