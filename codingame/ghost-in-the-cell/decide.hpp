#ifndef GUARD_DPSG_GTIC_DECIDE_HPP
#define GUARD_DPSG_GTIC_DECIDE_HPP

#include "./types.hpp"

#include "./decisions.hpp"
#include "./graph.hpp"

#include <numeric>
#include <queue>
#include <utility>

namespace gitc {
double strategic_value(factory_id factory_id,
                       const graph& map,
                       const factory_container& factories) {
  const auto& factory = *factories.find(factory_id);
  double point_of_interest_proximity = 0;
  double enemy_proximity = 0;
  double friendly_proximity = 0;

  for (auto& other_factory : factories) {
    if (factory_id == id(other_factory))
      continue;
    auto dist = map.distance(factory_id, id(other_factory));
    point_of_interest_proximity += production(other_factory) / dist;
    if (owner(other_factory) == owner_type::me) {
      friendly_proximity += cyborgs(other_factory) / dist * 2;
    }
    else if (owner(other_factory) == owner_type::opponent) {
      enemy_proximity += cyborgs(other_factory) / dist;
    }
  }

  double coef = owner(factory) == owner_type::me ? 0.1
                : owner(factory) == owner_type::neutral
                    ? 1
                    : (friendly_proximity - cyborgs(factory).value);
  double base = production(factory).value * coef;
  return base *
         (enemy_proximity + friendly_proximity + point_of_interest_proximity);
}

double relative_strategic_value(factory_id target_id,
                                factory_id rel_point_id,
                                const graph& map,
                                const factory_container& factories) {
  double base = strategic_value(target_id, map, factories);
  duration distance = map.distance(target_id, rel_point_id);
  return base / distance.value;
}

strength incoming_soldiers(const factory_id& origin,
                           const troop_container& troops) {
  return std::accumulate(troops.begin(),
                         troops.end(),
                         strength{0},
                         [origin](strength acc, const auto& troop) -> strength {
                           if (target(troop) == origin) {
                             int own = static_cast<int>(owner(troop));
                             return acc + (own * cyborgs(troop));
                           }
                           return acc;
                         });
}

const double MAX_SENT = 0.9;
strength available_soldiers(const factory_with_id& origin,
                            const troop_container& troops) {
  auto coming_in = incoming_soldiers(id(origin), troops);
  return (cyborgs(origin) + coming_in) * MAX_SENT;
}

strength available_defence(const factory_with_id& factory,
                           const troop_container& troops) {
  strength op_soldiers =
      cyborgs(factory) + incoming_soldiers(id(factory), troops);
  return op_soldiers;
}

strength strength_required(const factory_with_id& factory,
                           duration distance,
                           const troop_container& troops) {
  strength op_soldiers =
      cyborgs(factory) - incoming_soldiers(id(factory), troops);
  strength prod =
      static_cast<int>(owner(factory)) * production(factory) * distance;
  prod = prod >= 0 ? prod : -prod;
  return prod + op_soldiers + 1;
}

decision_list decide(const graph& map,
                     const factory_container& factories,
                     const troop_container& troops) {
  decision_list decisions;
  std::unordered_map<factory_id, double> strategic_values;
  for (auto& factory : factories) {
    strategic_values.emplace(id(factory),
                             strategic_value(id(factory), map, factories));
  }

  for (auto& factory : factories) {
    if (owner(factory) == owner_type::me) {
      auto soldiers = available_soldiers(factory, troops);

      //  std::cerr << "considering factory: " << id(factory).id << std::endl;
      using factory_strategic_value = std::tuple<double, factory_id, strength>;
      constexpr int factory_idx = 1;
      constexpr int strength_idx = 2;
      std::priority_queue<factory_strategic_value> queue;

      for (auto& target : factories) {
        if (id(target) == id(factory))
          continue;

        double r_strat_value =
            relative_strategic_value(id(target), id(factory), map, factories);

        strength str =
            owner(target) == owner_type::me
                ? std::max(strength{0}, available_defence(factory, troops))
                : strength_required(
                      target, map.distance(id(target), id(factory)), troops);

        queue.emplace(r_strat_value, id(target), str);
      }

      while (soldiers > 0 && queue.size() > 0) {
        auto target = std::move(queue.top());
        queue.pop();

        strength req_soldiers = std::get<strength_idx>(target);
        double strat_value =
            strategic_values.find(std::get<factory_idx>(target))->second;
        strength opti_soldiers{req_soldiers * (1 + (strat_value / 100))};
        opti_soldiers = std::max(opti_soldiers, soldiers);

        //  std::cerr << " soldiers: " << soldiers.value
        //            << " target_id: " << std::get<1>(target).id
        //            << " req soldiers: " << req_soldiers.value
        //            << " opti soldiers: " << opti_soldiers.value
        //            << " strat: " << std::get<0>(target)
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