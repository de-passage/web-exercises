#ifndef GUARD_DPSG_GITC_ENTITY_HPP
#define GUARD_DPSG_GITC_ENTITY_HPP

#include <unordered_map>

#include "./types.hpp"

namespace gitc {
struct entity {
  constexpr entity(troop_info info) noexcept
      : _type{entity_type::troop}, _troops{info} {}
  constexpr entity(factory_info info) noexcept
      : _type{entity_type::factory}, _factory{info} {}

  template <class T, class F>
  decltype(auto) either(T&& troop_func, F&& factory_func) const& {
    if (_type == entity_type::factory) {
      return factory_func(_factory);
    }
    return troop_func(_troops);
  }

 private:
  entity_type _type;
  union {
    troop_info _troops;
    factory_info _factory;
  };
};

using entity_container = std::unordered_map<id_t, entity>;
}  // namespace gitc

#endif  // GUARD_DPSG_GITC_ENTITY_HPP