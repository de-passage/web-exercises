#ifndef GUARD_DPSG_BINARY_SEARCH_HPP
#define GUARD_DPSG_BINARY_SEARCH_HPP
#include <cassert>
#include <iostream>
#include <sstream>

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

// Top left corner included, bottom right excluded
struct box {
  box() = default;
  box(size_t min_x, size_t min_y, size_t max_x, size_t max_y)
      : top_left{min_x, min_y}, bottom_right{max_x, max_y} {
    if (left() >= right()) {
      std::stringstream fmt;
      fmt << "Invalid dimensions, left(" << left() << ") >= right(" << right()
          << ")";
      throw std::invalid_argument(fmt.str());
    }
    if (top() >= bottom()) {
      std::stringstream fmt;
      fmt << "Invalid dimensions, top(" << top() << ") >= bottom(" << bottom()
          << ")";
      throw std::invalid_argument(fmt.str());
    }
  }

  size_t& top() { return top_left.y; }
  size_t& bottom() { return bottom_right.y; }
  size_t& left() { return top_left.x; }
  size_t& right() { return bottom_right.x; }

  size_t top() const { return top_left.y; }
  size_t bottom() const { return bottom_right.y; }
  size_t left() const { return top_left.x; }
  size_t right() const { return bottom_right.x; }

  size_t height() const { return bottom() - top(); }

  size_t width() const { return right() - left(); }

  box left_half() const { return {left(), top(), vertical_center(), bottom()}; }

  box right_half() const {
    return {vertical_center(), top(), right(), bottom()};
  }

  box bottom_half() const {
    return {left(), horizontal_center(), right(), bottom()};
  }

  box top_half() const { return {left(), top(), right(), horizontal_center()}; }

  size_t vertical_center() const { return (left() + right()) / 2; }

  size_t horizontal_center() const { return (top() + bottom()) / 2; }

  bool contains(const coordinates& c) const {
    return c.x >= left() && c.x < right() && c.y >= top() && c.y < bottom();
  }

  std::pair<coordinates, box> symmetric_point_boxed(
      const coordinates& c) const {
    if (!contains(c)) {
      throw std::out_of_range(
          "Trying to find the symmetry of a point outside the box");
    }

    if (width() < height()) {
      coordinates new_c{c.x, top() + bottom() - c.y - 1};
      if (top_half().contains(new_c)) {
        return std::make_pair(new_c, top_half());
      }
      return std::make_pair(new_c, bottom_half());
    }

    coordinates new_c{left() + right() - c.x - 1, c.y};
    if (left_half().contains(new_c)) {
      return std::make_pair(new_c, left_half());
    }
    return std::make_pair(new_c, right_half());
  }

  box best_half() const {
    if (width() < height()) {
      return bottom_half();
    }
    return right_half();
  }

  box other_half(box other) const {
    if (width() < other.width()) {
      throw std::invalid_argument("Received box is too wide");
    }
    if (height() < other.height()) {
      throw std::invalid_argument("Received box is too tall");
    }
    if (!((top() == other.top() && bottom() == other.bottom()) ||
          (left() == other.left() && right() == other.right()))) {
      std::stringstream fmt;
      fmt << "Received box {" << other.left() << "," << other.top() << ","
          << other.right() << "," << other.bottom()
          << "} doesn't have a common dimension with calling object {" << left()
          << "," << top() << "," << right() << "," << bottom() << "}";
      throw std::invalid_argument(fmt.str());
    }

    if (other.right() < right()) {
      other.left() = other.right();
      other.right() = right();
    }
    else if (other.left() > left()) {
      other.right() = other.left();
      other.left() = left();
    }

    if (other.top() > top()) {
      other.bottom() = other.top();
      other.top() = top();
    }
    else if (other.bottom() < bottom()) {
      other.top() = other.bottom();
      other.bottom() = bottom();
    }

    return other;
  }

  coordinates center() const {
    return {vertical_center(), horizontal_center()};
  }

  box vertical_line_at(size_t x) const {
    box result{*this};
    result.left() = x;
    result.right() = x + 1;
    return result;
  }

  box horizontal_line_at(size_t y) const {
    box result{*this};
    result.top() = y;
    result.bottom() = y + 1;
    return result;
  }

  friend bool operator==(const box& left, const box& right) {
    return left.top_left == right.top_left &&
           left.bottom_right == right.bottom_right;
  }

  friend bool operator!=(const box& left, const box& right) {
    return !(left == right);
  }

 private:
  coordinates top_left;
  coordinates bottom_right;
};

inline std::ostream& operator<<(std::ostream& out, const coordinates& coords) {
  return out << coords.x << " " << coords.y;
}
inline std::istream& operator>>(std::istream& in, coordinates& coords) {
  return in >> coords.x >> coords.y;
}
inline std::ostream& operator<<(std::ostream& out, const box& box) {
  return out << "{" << box.left() << ", " << box.top() << ", " << box.right()
             << ", " << box.bottom() << "}";
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
  if (d1 < d2) {
    return ordering::lesser;
  }
  return ordering::greater;
}

enum class temperature {
  hot,
  cold,
  same,
  unknown,
  found /* stopping point in unit tests */
};
inline std::ostream& operator<<(std::ostream& out, temperature t) {
  switch (t) {
    case temperature::hot:
      return out << "WARMER";
    case temperature::same:
      return out << "SAME";
    case temperature::cold:
      return out << "COLDER";
    case temperature::found:
      return out << "FOUND";
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
  else if (dir == "FOUND") {
    tmp = temperature::found;
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

void avoid_center(coordinates& current,
                  const box& reference,
                  const coordinates& center) {
  if (current.x == center.x && reference.width() % 2 == 1) {
    if (current.x > reference.left()) {
      current.x--;
    }
    else if (current.x < reference.right() - 1) {
      current.x++;
    }
  }
  if (current.y == center.y && reference.height() % 2 == 1) {
    if (current.y > reference.top()) {
      current.y--;
    }
    else if (current.y < reference.bottom() - 1) {
      current.y++;
    }
  }
}
void avoid_center(coordinates& current, const box& reference) {
  avoid_center(current, reference, reference.center());
}

coordinates search_by_rectangles(std::istream& in, std::ostream& out) {
  size_t h{};
  size_t w{};
  size_t n{};

  in >> w >> h >> n;

  box search_space{0, 0, w, h};

  box attempt{search_space};

  coordinates current{};

  in >> current;
  coordinates last{current};

  temperature temp = temperature::unknown;
  bool searching = false;

  while (search_space.width() > 1 || search_space.height() > 1) {
    in >> temp;

    if (temp == temperature::found) {
      return current;
    }

    // NEED TO FIX: if we're in the middle of a row/column, need to move aside
    // also need to double check logic for SAME result

    if (!searching) {
      avoid_center(current, search_space);
      auto p = search_space.symmetric_point_boxed(current);
      last = current;
      current = p.first;
      attempt = p.second;
      searching = true;
    }
    else {
      if (temp == temperature::hot) {
        // we're on the right spot, let's assume that the box is ok
        search_space = attempt;

        // we now want to find the new middle, on the symmetry
        avoid_center(current, search_space);
        auto p = search_space.symmetric_point_boxed(current);
        last = current;
        current = p.first;
        attempt = p.second;

        searching = true;
      }
      else if (temp == temperature::cold) {
        search_space = search_space.other_half(attempt);
        attempt = search_space.best_half();
        auto c = attempt.center();
        last = current;
        current = c;
        avoid_center(current, attempt, c);
        searching = false;
      }
      else if (temp == temperature::same) {
        if (search_space.width() == 1 || search_space.height() == 1) {
          current = search_space.center();
        }
        else {
          if (current.x == last.x) {
            search_space =
                search_space.vertical_line_at((current.y + last.y) / 2);
          }
          else {
            search_space =
                search_space.horizontal_line_at((current.x + last.x) / 2);
          }

          attempt = search_space;
          auto c = attempt.center();
          last = current;
          current = c;
          avoid_center(current, attempt, c);
          searching = false;
        }
      }

      if (last == current) {
        throw std::runtime_error(
            "Jumping on the same point, this shouldn't happen");
      }
      out << current << std::endl;
    }
  }

  return {search_space.left(), search_space.top()};
}

#endif  // GUARD_DPSG_BINARY_SEARCH_HPP