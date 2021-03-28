#ifndef GUARD_DPSG_GITC_DECISIONS_HPP
#define GUARD_DPSG_GITC_DECISIONS_HPP

#include "./types.hpp"

namespace gitc {

enum class decision_type { wait, move };
struct wait_t {
} constexpr wait;
struct move {
  strength cyborgs;
  factory origin;
  factory destination;
};

class decision {
 public:
  decision(wait_t) : _type(decision_type::wait), _wait{} {}
  decision(const move& m) : _type{decision_type::move}, _move{m} {}

  template <class W, class M>
  decltype(auto) dispatch(const W& with_wait, const M& with_move) const {
    if (_type == decision_type::wait) {
      return with_wait();
    }
    return with_move(_move);
  }

 private:
  decision_type _type;
  union {
    wait_t _wait;
    move _move;
  };
};

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_DECISIONS_HPP