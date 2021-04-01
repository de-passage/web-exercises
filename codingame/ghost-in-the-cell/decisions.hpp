#ifndef GUARD_DPSG_GITC_DECISIONS_HPP
#define GUARD_DPSG_GITC_DECISIONS_HPP

#include "./types.hpp"

namespace gitc {

enum class decision_type { wait, move, bomb, inc };
struct wait_t {
} constexpr wait;
struct move {
  strength cyborgs;
  factory_id origin;
  factory_id destination;
};

struct increment_production {
  factory_id target;
};

struct launch_bomb {
  factory_id origin;
  factory_id destination;
};

class decision {
 public:
  decision(wait_t) : _type(decision_type::wait), _wait{} {}
  decision(const move& m) : _type{decision_type::move}, _move{m} {}
  decision(increment_production p) : _type(decision_type::inc), _inc{p} {}
  decision(const launch_bomb& b) : _type(decision_type::bomb), _bomb{b} {}

  template <class W, class M, class I, class B>
  decltype(auto) dispatch(const W& with_wait,
                          const M& with_move,
                          const I& with_inc,
                          const B& with_bomb) const {
    switch (_type) {
      case decision_type::bomb:
        return with_bomb(_bomb);
      case decision_type::inc:
        return with_inc(_inc);
      case decision_type::move:
        return with_move(_move);
      default:
        return with_wait();
    }
  }

 private:
  decision_type _type;
  union {
    wait_t _wait;
    move _move;
    launch_bomb _bomb;
    increment_production _inc;
  };
};

using decision_list = std::vector<decision>;

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_DECISIONS_HPP