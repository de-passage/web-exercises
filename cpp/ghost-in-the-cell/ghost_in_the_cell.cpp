#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

enum class owner_type : int { me = 1, opponent = -1, neutral = 0 };
enum class entity_type : int { troop, factory };

struct strength {
  constexpr strength() noexcept = default;
  constexpr strength(int value) noexcept : value{value} {}
  int value{};
};

struct factory_info {
  owner_type owner;
  strength cyborgs;
  strength production;
};

struct factory {
  constexpr explicit factory(size_t id) : _id{id} {}

  size_t id() const { return _id; }
  friend bool operator==(factory left, factory right) noexcept {
    return left._id == right._id;
  }

 private:
  size_t _id;
};

struct weight {
  constexpr weight() noexcept = default;
  constexpr weight(int d) : value{d} {}
  int value{};
};

struct factory_distance {
  constexpr factory_distance(factory f, weight d) : target{f}, distance{d} {}
  factory target;
  weight distance;
};

struct troop_info {
  owner_type owner;
  factory origin;
  strength cyborgs;
  factory_distance distance;
};

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

struct entity_id {
  int id;
  friend bool operator==(entity_id left, entity_id right) noexcept {
    return left.id == right.id;
  }
};
namespace std {
template <>
struct hash<factory> {
  auto operator()(factory f) const { return hash<size_t>()(f.id()); }
};
template <>
struct hash<entity_id> {
  auto operator()(entity_id id) const { return hash<int>()(id.id); }
};
}  // namespace std

namespace detail {
struct graph_container {
  weight* _weights;
  size_t _count;
};
}  // namespace detail

class graph : private detail::graph_container {
  using base = detail::graph_container;
  using base::_count;
  using base::_weights;

 public:
  explicit graph(size_t node_count)
      : base{new weight[node_count * node_count], node_count} {
    for (size_t i = 0; i < _count; ++i) {
      for (size_t j = 0; j < _count; ++j) {
        add_edge(factory{i}, factory{j},
                 weight{i == j ? 0 : numeric_limits<int>::max()});
      }
    }
  }

  graph(const graph& to_copy)
      : base{new weight[to_copy.node_count() * to_copy.node_count()],
             to_copy.node_count()} {
    for (size_t i = 0; i < _count; ++i) {
      for (size_t j = 0; j < _count; ++j) {
        factory left{i}, right{j};
        add_edge(left, right, to_copy.distance(left, right));
      }
    }
  }

  graph(graph&& to_copy)
      : base{exchange(to_copy._weights, nullptr), exchange(to_copy._count, 0)} {
  }

  graph& operator=(const graph& g) {
    graph{g}.swap(*this);
    return *this;
  }

  graph& operator=(graph&& g) {
    graph{move(g)}.swap(*this);
    return *this;
  }

  ~graph() { delete _weights; }

  friend void swap(graph& left, graph& right) { left.swap(right); }

  void add_edge(factory left, factory right, weight distance) {
    this->distance(left, right) = distance;
    this->distance(right, left) = distance;
  }

  weight distance(factory left, factory right) const {
    return const_cast<graph*>(this)->distance(left, right);
  }

  struct node_range : private detail::graph_container {
   private:
    using base = detail::graph_container;
    using base::_count;
    using base::_weights;
    node_range(weight* w, size_t count) : base{w, count} {}
    friend class graph;
  };

  node_range nodes() const { return node_range{_weights, _count}; }

  size_t node_count() const { return _count; }

 private:
  weight& distance(factory left, factory right) {
    return _weights[left.id() * _count + right.id()];
  }

  void swap(graph& g) {
    using std::swap;
    swap(_count, g._count);
    swap(_weights, g._weights);
  }
};

using entity_container = unordered_map<entity_id, entity>;

void upsert(entity_container& container, pair<entity_id, entity>&& to_add) {
  auto it = container.find(to_add.first);
  if (it != container.end()) {
    it->second = move(to_add.second);
  } else {
    container.insert(move(to_add));
  }
}

graph parse_map() {
  size_t factory_count;  // the number of factories
  cin >> factory_count;
  cin.ignore();

  int link_count;  // the number of links between factories
  cin >> link_count;
  cin.ignore();
  graph map(factory_count);

  for (int i = 0; i < link_count; i++) {
    size_t factory1;
    size_t factory2;
    int distance;
    cin >> factory1 >> factory2 >> distance;
    cin.ignore();
    factory f1{factory1};
    factory f2{factory2};
    weight dist{distance};
    map.add_edge(f1, f2, dist);
  }

  return map;
}

factory_info parse_factory_info(istream& in) {
  int owner;
  strength cyborgs;
  strength production;
  int discard;
  in >> owner >> cyborgs.value >> production.value >> discard >> discard;
  cin.ignore();
  return factory_info{static_cast<owner_type>(owner), cyborgs, production};
}

troop_info parse_troop_info(istream& in) {
  int owner;
  size_t origin;
  strength cyborgs;
  size_t destination;
  weight distance;
  in >> owner >> origin >> destination >> cyborgs.value >> distance.value;
  cin.ignore();
  return troop_info{static_cast<owner_type>(owner), factory{origin}, cyborgs,
                    factory_distance{factory{destination}, distance}};
}

pair<entity_id, entity> parse_entity(istream& in) {
  entity_id id;
  string type;
  in >> id.id >> type;
  if (type == "FACTORY") {
    return make_pair(id, parse_factory_info(in));
  } else if (type == "TROOP") {
    return make_pair(id, parse_troop_info(in));
  }

  throw runtime_error("Invalid entity type: " + move(type));
}

void parse_and_update_entities(entity_container& container) {
  int entity_count;  // the number of entities (e.g. factories and troops)
  cin >> entity_count;
  cin.ignore();
  for (int i = 0; i < entity_count; i++) {
    upsert(container, parse_entity(cin));
  }
}

int main() {
  graph map{parse_map()};
  entity_container entities;

  // game loop
  while (1) {
    parse_and_update_entities(entities);
    // Any valid action, such as "WAIT" or "MOVE source destination cyborgs"
    cout << "WAIT" << endl;
  }
}