#ifndef GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP
#define GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP

#include "./decisions.hpp"

#include <iostream>

namespace gitc {

std::ostream& operator<<(std::ostream& out, const decision& d) {
  return d.dispatch(
      [&out]() -> decltype(auto) { return out << "WAIT"; },
      [&out](const move& m) -> decltype(auto) {
        return out << "MOVE " << m.origin.value << " " << m.destination.value
                   << " " << m.cyborgs.value;
      },
      [&out](const increment_production& p) -> decltype(auto) {
        return out << "INC " << p.target.value;
      },
      [&out](const launch_bomb& b) -> decltype(auto) {
        return out << "BOMB " << b.origin.value << " " << b.destination.value;
      });
}

std::ostream& operator<<(std::ostream& out, const decision_list& decisions) {
  if (decisions.size() > 0) {
    for (size_t i = 0; i < decisions.size(); ++i) {
      auto& d = decisions[i];
      out << d;
      if (i != decisions.size() - 1)
        out << ';';
    }
  }
  else {
    out << wait;
  }
  return out;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP