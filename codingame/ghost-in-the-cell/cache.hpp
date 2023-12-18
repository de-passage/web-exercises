#ifndef GUARD_DPSG_GTIC_CACHE_HPP
#define GUARD_DPSG_GTIC_CACHE_HPP

#include "./graph.hpp"
#include "./types.hpp"

#include <stdexcept>
#include <unordered_map>

namespace gitc {

struct cache {
  struct factory_strategic_data {
    strategic_value value;
    strength incoming_soldiers;
  };

  using factory_data_container =
      std::unordered_map<factory_id, factory_strategic_data>;

  factory_data_container factory_data;

  factory_strategic_data data_for(factory_id id) const {
    auto it = factory_data.find(id);
    if (it != factory_data.end()) {
      return it->second;
    }
    throw std::runtime_error("trying to access non-existant factory data");
  }

  strategic_value strategic_value_for(factory_id id) const {
    return data_for(id).value;
  }

  strength incoming_soldiers_for(factory_id id) const {
    return data_for(id).incoming_soldiers;
  }
};

strategic_value relative_strategic_value(factory_id target_id,
                                         factory_id rel_point_id,
                                         const graph& map,
                                         const cache& cache) {
  strategic_value base = cache.strategic_value_for(target_id);
  duration distance = map.distance(target_id, rel_point_id);
  return base / distance.value;
}

strength incoming_soldiers(const factory_id& origin,
                           const troop_container& troops) {
  strength acc{0};
  for (const auto& troop : troops) {
    if (target(troop) == origin) {
      int own = static_cast<int>(owner(troop));
      return acc + (own * cyborgs(troop));
    }
  }
  return acc;
}

const double MAX_SENT = 0.9;
strength available_soldiers(const factory_with_id& origin, const cache& cache) {
  auto coming_in = cache.incoming_soldiers_for(id(origin));
  return (cyborgs(origin) + coming_in) * MAX_SENT;
}

strength available_defence(const factory_with_id& factory, const cache& cache) {
  strength op_soldiers =
      cyborgs(factory) + cache.incoming_soldiers_for(id(factory));
  return op_soldiers;
}

strength strength_required(const factory_with_id& factory,
                           duration distance,
                           const cache& cache) {
  strength op_soldiers =
      cyborgs(factory) - cache.incoming_soldiers_for(id(factory));
  strength prod =
      static_cast<int>(owner(factory)) * production(factory) * distance;
  prod = prod >= 0 ? prod : -prod;
  return prod + op_soldiers + 1;
}

strategic_value compute_strategic_value(factory_id factory_id,
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
  return strategic_value{base * (enemy_proximity + friendly_proximity +
                                 point_of_interest_proximity)};
}

cache build_cache(const graph& map,
                  const factory_container& factories,
                  const troop_container& troops) {
  cache cache;

  for (const auto& factory : factories) {
    strategic_value sv = compute_strategic_value(id(factory), map, factories);
    strength is = incoming_soldiers(id(factory), troops);

    cache.factory_data.emplace(id(factory),
                               cache::factory_strategic_data{sv, is});
  }

  return cache;
}
}  // namespace gitc

#endif  // GUARD_DPSG_GTIC_CACHE_HPP