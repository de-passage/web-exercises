#ifndef GUARD_DPSG_GTIC_DECIDE_HPP
#define GUARD_DPSG_GTIC_DECIDE_HPP

#include "./decisions.hpp"
#include "./graph.hpp"
#include "./types.hpp"

#include <algorithm>
#include <numeric>
#include <queue>
#include <utility>

namespace gitc {

double strategic_value(entity_id fact_id,
                       const graph& map,
                       const factory_container& factories,
                       const troop_container& troops) {
  const auto& info = *factories.find(fact_id);
  double point_of_interest_proximity = 0;
  double enemy_proximity = 0;
  double friendly_proximity = 0;
  for (auto& fact : factories) {
    if (fact_id == id(fact))
      continue;
    auto dist = map.distance(to_factory(fact_id), to_factory(id(fact)));
    point_of_interest_proximity += production(fact) / dist;
    if (owner(fact) == owner_type::me) {
      friendly_proximity += cyborgs(fact) / dist;
    }
    else if (owner(fact) == owner_type::opponent) {
      enemy_proximity += cyborgs(fact) / dist;
    }
  }

  double coef = owner(info) == owner_type::me ? 0.1
                : owner(info) == owner_type::neutral
                    ? 1
                    : (friendly_proximity - cyborgs(info).value);
  double base = production(info).value * coef;
  return base *
         (enemy_proximity + friendly_proximity + point_of_interest_proximity);
}

const double MAX_SENT = 0.9;
strength available_soldiers(const entity_id& origin,
                            const factory_info& info,
                            const troop_container& troops) {
  int coming_in =
      std::accumulate(troops.begin(),
                      troops.end(),
                      0,
                      [origin](int acc, const auto& pair) -> int {
                        if (pair.second.distance.target == to_factory(origin)) {
                          int str = cyborgs(pair).value;
                          int own = static_cast<int>(owner(pair));
                          return acc + (own * str);
                        }
                        return acc;
                      });
  return (info.cyborgs.value + coming_in) * 0.9;
}

bool has_enough_soldiers(const weight& distance,
                         const strength& av_soldiers,
                         const factory_info& destination) {
  strength op_soldiers = destination.cyborgs;

  return av_soldiers.value >
             op_soldiers.value +
                 (abs(static_cast<int>(owner(destination))) *
                  destination.production.value * distance.value) &&
         av_soldiers.value > 0;
}

decision decide(const graph& map,
                const factory_container& factories,
                const troop_container& troops) {
  std::vector<std::pair<entity_id, strength>> controlled;
  for (auto& fact : factories) {
    if (owner(fact) == owner_type::me) {
      controlled.emplace_back(
          id(fact), available_soldiers(fact.first, fact.second, troops));
    }
  }

  std::priority_queue<std::pair<double, entity_id>> queue;
  for (auto& fact : factories) {
    queue.emplace(strategic_value(id(fact), map, factories, troops), id(fact));
  }
  auto& best = queue.top();

  int closest_factory = -1;
  strength soldiers;

  for (auto& fact : controlled) {
    auto distance = map.distance(to_factory(best.second), to_factory(id(fact)));
    if (has_enough_soldiers(
            distance, fact.second, factories.find(best.second)->second) &&
        soldiers.value < (fact.second / distance)) {
      closest_factory = id(fact).id;
      soldiers = fact.second;
    }
  }

  if (closest_factory != -1 && closest_factory != best.second.id) {
    return move{soldiers,
                factory{static_cast<size_t>(closest_factory)},
                to_factory(best.second)};
  }

  return wait;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GTIC_DECIDE_HPP