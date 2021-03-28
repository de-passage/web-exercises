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
                       const factory_container& factories) {
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
      friendly_proximity += cyborgs(fact) / dist * 2;
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

double relative_strategic_value(entity_id target_id,
                                entity_id rel_point_id,
                                const graph& map,
                                const factory_container& factories) {
  double base = strategic_value(target_id, map, factories);
  weight distance =
      map.distance(to_factory(target_id), to_factory(rel_point_id));
  return base / distance.value;
}

strength incoming_soldiers(const entity_id& origin,
                           const troop_container& troops) {
  return std::accumulate(
      troops.begin(),
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
}

const double MAX_SENT = 0.9;
strength available_soldiers(const entity_id& origin,
                            const factory_info& info,
                            const troop_container& troops) {
  auto coming_in = incoming_soldiers(origin, troops);
  return strength{static_cast<int>(
      static_cast<double>(info.cyborgs.value + coming_in.value) * MAX_SENT)};
}

strength available_defence(const entity_id& fact,
                           const factory_info& info,
                           const troop_container& troops) {
  strength op_soldiers =
      info.cyborgs.value + incoming_soldiers(fact, troops).value;
  auto prod = info.production;
  return op_soldiers.value + prod.value;
}

strength strength_required(const entity_id& fact,
                           const factory_info& info,
                           weight distance,
                           const troop_container& troops) {
  strength op_soldiers =
      info.cyborgs.value + -(incoming_soldiers(fact, troops).value);
  auto prod = abs(static_cast<int>(owner(info)) * info.production.value *
                  distance.value);
  return prod + op_soldiers.value + 1;
}

decision_list decide(const graph& map,
                     const factory_container& factories,
                     const troop_container& troops) {
  decision_list decisions;
  std::unordered_map<entity_id, double> strategic_values;
  for (auto& fact : factories) {
    strategic_values.emplace(fact.first,
                             strategic_value(fact.first, map, factories));
  }

  for (auto& fact : factories) {
    if (owner(fact) == owner_type::me) {
      auto soldiers = available_soldiers(fact.first, fact.second, troops);

      //  std::cerr << "considering factory: " << id(fact).id << std::endl;
      std::priority_queue<std::tuple<double, entity_id, strength>> queue;
      for (auto& target : factories) {
        if (id(target) == id(fact))
          continue;

        double r_strat_value =
            relative_strategic_value(id(target), id(fact), map, factories);

        strength str =
            owner(target) == owner_type::me
                ? std::max(
                      0, available_defence(id(fact), fact.second, troops).value)
                : strength_required(id(target),
                                    target.second,
                                    map.distance(to_factory(id(target)),
                                                 to_factory(id(fact))),
                                    troops)
                      .value;

        queue.emplace(r_strat_value, id(target), str);
      }

      while (soldiers.value > 0 && queue.size() > 0) {
        auto target = std::move(queue.top());
        queue.pop();

        strength req_soldiers = std::get<2>(target).value;
        double strat_value = strategic_values.find(std::get<1>(target))->second;
        strength opti_soldiers =
            std::max(static_cast<int>(static_cast<double>(req_soldiers.value) *
                                      (1 + (strat_value / 100))),
                     soldiers.value);

        //  std::cerr << " soldiers: " << soldiers.value
        //            << " target_id: " << std::get<1>(target).id
        //            << " req soldiers: " << req_soldiers.value
        //            << " opti soldiers: " << opti_soldiers.value
        //            << " strat: " << std::get<0>(target)
        //            << std::endl;
        if (soldiers.value >= req_soldiers.value && req_soldiers.value > 0) {
          int to_send = std::min(req_soldiers.value, opti_soldiers.value);
          soldiers.value -= to_send;
          decisions.emplace_back(move{strength{to_send},
                                      to_factory(id(fact)),
                                      to_factory(std::get<1>(target))});
        }
      }
    }
  }

  return decisions;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GTIC_DECIDE_HPP