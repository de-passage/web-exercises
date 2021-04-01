#ifndef GUARD_DPSG_GTIC_DECIDE_HPP
#define GUARD_DPSG_GTIC_DECIDE_HPP

#include "./types.hpp"

#include "./decisions.hpp"
#include "./graph.hpp"

#include "./cache.hpp"

#include <numeric>
#include <queue>
#include <utility>

namespace gitc {

decision_list decide(const graph& map,
                     const factory_container& factories,
                     const troop_container& troops,
                     const bomb_container& bombs) {
  decision_list decisions;
  cache cache{build_cache(map, factories, troops)};

  for (auto& factory : factories) {
    if (owner(factory) == owner_type::me) {
      auto soldiers = available_soldiers(factory, cache);

      //  std::cerr << "considering factory: " << id(factory).value <<
      //  std::endl;
      using factory_strategic_value =
          std::tuple<strategic_value, factory_id, strength>;
      constexpr int factory_idx = 1;
      constexpr int strength_idx = 2;
      std::priority_queue<factory_strategic_value> queue;

      for (auto& target : factories) {
        if (id(target) == id(factory))
          continue;

        strategic_value r_strat_value =
            relative_strategic_value(id(target), id(factory), map, cache);

        strength str =
            owner(target) == owner_type::me
                ? std::max(strength{0}, available_defence(factory, cache))
                : strength_required(
                      target, map.distance(id(target), id(factory)), cache);

        queue.emplace(r_strat_value, id(target), str);
      }

      while (soldiers > 0 && queue.size() > 0) {
        auto target = std::move(queue.top());
        queue.pop();

        strength req_soldiers = std::get<strength_idx>(target);
        strategic_value strat_value =
            cache.strategic_value_for(std::get<factory_idx>(target));
        strength opti_soldiers{req_soldiers * (1 + (strat_value / 100)).value};
        opti_soldiers = std::max(opti_soldiers, soldiers);

        //  std::cerr << " soldiers: " << soldiers.value
        //            << " target_id: " << std::get<1>(target).value
        //            << " req soldiers: " << req_soldiers.value
        //            << " opti soldiers: " << opti_soldiers.value
        //            << " strat: " << std::get<0>(target).value
        //            << std::endl;
        if (soldiers >= req_soldiers && req_soldiers > 0) {
          auto to_send = std::min(req_soldiers, opti_soldiers);
          soldiers -= to_send;
          decisions.emplace_back(
              move{to_send, id(factory), std::get<factory_idx>(target)});
        }
      }
    }
  }

  return decisions;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GTIC_DECIDE_HPP