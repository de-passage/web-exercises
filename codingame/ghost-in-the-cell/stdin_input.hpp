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

void discard_remaining(std::istream& in) {
  in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

graph parse_map(std::istream& in) {
  size_t factory_count;  // the number of factories
  in >> factory_count;
  in.ignore();

  int link_count;  // the number of links between factories
  in >> link_count;
  in.ignore();
  graph map(factory_count);

  for (int i = 0; i < link_count; i++) {
    id_t factory1;
    id_t factory2;
    int distance;
    in >> factory1 >> factory2 >> distance;
    discard_remaining(in);
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
  production_capacity prod;
  duration inactivity;
  in >> owner >> cyborgs.value >> prod.value >> inactivity.value;
  discard_remaining(in);
  return factory_info{
      static_cast<owner_type>(owner), cyborgs, prod, inactivity};
}

troop_info parse_troop_info(std::istream& in) {
  int owner;
  id_t origin;
  strength cyborgs;
  id_t destination;
  duration distance;
  in >> owner >> origin >> destination >> cyborgs.value >> distance.value;
  discard_remaining(in);
  return troop_info{static_cast<owner_type>(owner),
                    factory_id{origin},
                    cyborgs,
                    distance,
                    factory_id{destination}};
}

bomb_info parse_bomb_info(std::istream& in) {
  int owner;
  id_t origin;
  id_t destination;
  duration distance;
  in >> owner >> origin >> destination >> distance.value;
  discard_remaining(in);
  return bomb_info{static_cast<owner_type>(owner),
                   factory_id{origin},
                   factory_id{destination},
                   distance};
}

void parse_entity(std::istream& in,
                  troop_container& troops,
                  factory_container& factories,
                  bomb_container& bombs) {
  id_t id;
  std::string type;
  in >> id >> type;
  if (type == "FACTORY") {
    upsert(factories, std::make_pair(factory_id{id}, parse_factory_info(in)));
  }
  else if (type == "TROOP") {
    upsert(troops, std::make_pair(troop_id{id}, parse_troop_info(in)));
  }
  else if (type == "BOMB") {
    upsert(bombs, std::make_pair(bomb_id{id}, parse_bomb_info(in)));
  }
}

void parse_and_update_entities(std::istream& in,
                               troop_container& troops,
                               factory_container& factories,
                               bomb_container& bombs) {
  int entity_count;  // the number of entities (e.g. factories and troops)
  in >> entity_count;
  in.ignore();
  for (int i = 0; i < entity_count; i++) {
    parse_entity(in, troops, factories, bombs);
  }
}

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_STDIN_INPUT_HPP