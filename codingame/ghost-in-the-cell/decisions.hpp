#ifndef GUARD_DPSG_GITC_DECISIONS_HPP
#define GUARD_DPSG_GITC_DECISIONS_HPP

#include "./types.hpp"

namespace gitc {

enum class decision_type { wait, move, bomb, inc };
struct wait_t {
} constexpr wait;
struct move {
  strength cyborgs;
  factory origin;
  factory destination;
};

struct increment_production {
  factory target;
};

struct bomb {};

class decision {
 public:
  decision(wait_t) : _type(decision_type::wait), _wait{} {}
  decision(const move& m) : _type{decision_type::move}, _move{m} {}
  decision(increment_production p) : _type(decision_type::inc), _inc{p} {}

  template <class W, class M, class I>
  decltype(auto) dispatch(const W& with_wait,
                          const M& with_move,
                          const I& with_inc) const {
    if (_type == decision_type::wait) {
      return with_wait();
    }
    else if (_type == decision_type::inc) {
      return with_inc(_inc);
    }
    return with_move(_move);
  }

 private:
  decision_type _type;
  union {
    wait_t _wait;
    move _move;
    bomb _bomb;
    increment_production _inc;
  };
};

using decision_list = std::vector<decision>;

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_DECISIONS_HPP