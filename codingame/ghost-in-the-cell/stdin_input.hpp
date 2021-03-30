#ifndef GUARD_DPSG_GITC_STDIN_INPUT_HPP
#define GUARD_DPSG_GITC_STDIN_INPUT_HPP

#include <iostream>
#include <string>

#include "./entity.hpp"
#include "./graph.hpp"
#include "./types.hpp"

namespace gitc {

template <class K, class V>
void upsert(std::unordered_map<K, V>& container, std::pair<K, V>&& to_add) {
  auto it = container.find(to_add.first);
  if (it != container.end()) {
    it->second = std::move(to_add.second);
  }
  else {
    container.insert(std::move(to_add));
  }
};

graph parse_map() {
  size_t factory_count;  // the number of factories
  std::cin >> factory_count;
  std::cin.ignore();

  int link_count;  // the number of links between factories
  std::cin >> link_count;
  std::cin.ignore();
  graph map(factory_count);

  for (int i = 0; i < link_count; i++) {
    id_t factory1;
    id_t factory2;
    int distance;
    std::cin >> factory1 >> factory2 >> distance;
    std::cin.ignore();
    factory_id f1{factory1};
    factory_id f2{factory2};
    duration dist{distance};
    map.add_edge(f1, f2, dist);
  }

  return map;
}

factory_info parse_factory_info(std::istream& in) {
  int owner;
  strength cyborgs;
  strength production;
  int discard;
  in >> owner >> cyborgs.value >> production.value >> discard >> discard;
  std::cin.ignore();
  return factory_info{static_cast<owner_type>(owner), cyborgs, production};
}

troop_info parse_troop_info(std::istream& in) {
  int owner;
  id_t origin;
  strength cyborgs;
  id_t destination;
  duration distance;
  in >> owner >> origin >> destination >> cyborgs.value >> distance.value;
  std::cin.ignore();
  return troop_info{static_cast<owner_type>(owner),
                    factory_id{origin},
                    cyborgs,
                    factory_distance{factory_id{destination}, distance}};
}

void parse_entity(std::istream& in,
                  troop_container& troops,
                  factory_container& factories) {
  id_t id;
  std::string type;
  in >> id >> type;
  if (type == "FACTORY") {
    upsert(factories, std::make_pair(factory_id{id}, parse_factory_info(in)));
  }
  else if (type == "TROOP") {
    upsert(troops, std::make_pair(troop_id{id}, parse_troop_info(in)));
  }
}

void parse_and_update_entities(troop_container& troops,
                               factory_container& factories) {
  int entity_count;  // the number of entities (e.g. factories and troops)
  std::cin >> entity_count;
  std::cin.ignore();
  for (int i = 0; i < entity_count; i++) {
    parse_entity(std::cin, troops, factories);
  }
}

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_STDIN_INPUT_HPP