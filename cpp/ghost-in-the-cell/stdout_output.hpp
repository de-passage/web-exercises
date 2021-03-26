#ifndef GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP
#define GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP

#include "./decisions.hpp"

#include <iostream>

namespace gitc {

std::ostream& operator<<(std::ostream& out, const decision& d) {
  return d.dispatch([&out]() -> decltype(auto) { return out << "WAIT"; },
                    [&out](const move& m) -> decltype(auto) {
                      return out << "MOVE " << m.origin.id() << " "
                                 << m.destination.id() << " "
                                 << m.cyborgs.value;
                    });
}

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_STDOUT_OUTPUT_HPP