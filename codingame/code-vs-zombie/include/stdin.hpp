#ifndef GUARD_DPSG_CVSZ_STDIN_HPP
#define GUARD_DPSG_CVSZ_STDIN_HPP

#include "./types.hpp"

#include <iostream>

namespace cvsz {
typename human_container::value_type parse_human(std::istream& in) {
  int hid, x, y;
  in >> hid >> x >> y;
  in.ignore();
  return {id{hid}, human{x, y}};
}

typename zombie_container::value_type parse_zombie(std::istream& in) {
  int zid, x, y, xn, yn;
  (in >> zid >> x >> y >> xn >> yn).ignore();
  return {id{zid}, zombie{{x, y}, {xn, yn}}};
}
}  // namespace cvsz

#endif  // GUARD_DPSG_CVSZ_STDIN_HPP