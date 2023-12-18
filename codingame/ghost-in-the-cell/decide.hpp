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
int available_bombs = 2;

decision_list decide(const graph& map,
                     const factory_container& factories,
                     const troop_container& troops,
                     const bomb_container& bombs) {
  decision_list decisions;
  cache cache{build_cache(map, factories, troops)};

  for (auto& factory : factories) {
    if (owner(factory) == owner_type::me) {
      auto soldiers = available_soldiers(factory, cache);

      // std::cerr << "considering factory: " << id(factory).value << std::endl;
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

      if (cyborgs(factory) >= 10 && production(factory) < 3) {
        decisions.push_back(increment_production{id(factory)});
        soldiers -= 10;
      }

      while (soldiers > 0 && queue.size() > 0) {
        auto target = std::move(queue.top());
        queue.pop();

        strength req_soldiers = std::get<strength_idx>(target);
        strategic_value strat_value =
            cache.strategic_value_for(std::get<factory_idx>(target));
        strength opti_soldiers{req_soldiers * (1 + (strat_value / 100)).value};
        opti_soldiers = std::max(opti_soldiers, soldiers);

        // std::cerr << " soldiers: " << soldiers.value
        //           << " target_id: " << std::get<1>(target).value
        //           << " req soldiers: " << req_soldiers.value
        //           << " opti soldiers: " << opti_soldiers.value
        //           << " strat: " << std::get<0>(target).value << std::endl;
        if (req_soldiers > 0) {
          auto to_send =
              std::max(std::min(req_soldiers, opti_soldiers), soldiers);
          soldiers -= to_send;
          // std::cerr << "NEW ORDER: soldiers{" << to_send.value << "} from "
          //           << id(factory).value << " to "
          //           << std::get<factory_idx>(target).value << std::endl;
          decisions.emplace_back(
              move{to_send, id(factory), std::get<factory_idx>(target)});
        }
      }
    }
  }

  if (available_bombs > 0)
    for (auto& factory : factories) {
      if (owner(factory) == owner_type::opponent && production(factory) == 3) {
        auto bomb_underway = std::count_if(
            bombs.begin(), bombs.end(), [&factory](const bomb_with_id& bomb) {
              return id(factory) == target(bomb);
            });
        if (bomb_underway == 0) {
          auto closest = std::accumulate(
              factories.begin(),
              factories.end(),
              id(factory),
              [&map, &factory](factory_id best_id,
                               const factory_with_id& potential) {
                duration unlimited{std::numeric_limits<int>::max()};
                if (owner(potential) != owner_type::me)
                  return best_id;
                auto best_dist = best_id == id(factory)
                                     ? unlimited
                                     : map.distance(best_id, id(potential));
                auto d = id(potential) == id(factory)
                             ? unlimited
                             : map.distance(id(potential), id(factory));
                if (d < best_dist) {
                  return id(potential);
                }
                return best_id;
              });
          if (closest != id(factory)) {
            decisions.push_back(launch_bomb{closest, id(factory)});
            --available_bombs;
            if (available_bombs == 0)
              break;
          }
        }
      }
    }

  return decisions;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GTIC_DECIDE_HPP