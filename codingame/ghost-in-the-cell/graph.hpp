#ifndef GUARD_DPSG_GITC_GRAPH_HPP
#define GUARD_DPSG_GITC_GRAPH_HPP

#include "./types.hpp"

namespace gitc {

namespace detail {
struct graph_container {
  duration* _durations;
  size_t _count;
};
}  // namespace detail

class graph : private detail::graph_container {
  using base = detail::graph_container;
  using base::_count;
  using base::_durations;

 public:
  explicit graph(size_t node_count)
      : base{new duration[node_count * node_count], node_count} {
    for (size_t i = 0; i < _count; ++i) {
      for (size_t j = 0; j < _count; ++j) {
        add_edge(factory_id{static_cast<id_t>(i)},
                 factory_id{static_cast<id_t>(j)},
                 duration{i == j ? 0 : std::numeric_limits<int>::max()});
      }
    }
  }

  graph(const graph& to_copy)
      : base{new duration[to_copy.node_count() * to_copy.node_count()],
             to_copy.node_count()} {
    for (size_t i = 0; i < _count; ++i) {
      for (size_t j = 0; j < _count; ++j) {
        factory_id left{static_cast<id_t>(i)}, right{static_cast<id_t>(j)};
        add_edge(left, right, to_copy.distance(left, right));
      }
    }
  }

  graph(graph&& to_copy)
      : base{std::exchange(to_copy._durations, nullptr),
             std::exchange(to_copy._count, 0)} {}

  graph& operator=(const graph& g) {
    graph{g}.swap(*this);
    return *this;
  }

  graph& operator=(graph&& g) {
    graph{std::move(g)}.swap(*this);
    return *this;
  }

  ~graph() { delete _durations; }

  friend void swap(graph& left, graph& right) { left.swap(right); }

  void add_edge(factory_id left, factory_id right, duration distance) {
    _distance(left, right) = distance;
    _distance(right, left) = distance;
  }

  duration distance(factory_id left, factory_id right) const {
    return const_cast<graph*>(this)->_distance(left, right);
  }

  struct node_range : private detail::graph_container {
   private:
    using base = detail::graph_container;
    using base::_count;
    using base::_durations;
    node_range(duration* w, size_t count) : base{w, count} {}
    friend class graph;
  };

  node_range nodes() const { return node_range{_durations, _count}; }

  size_t node_count() const { return _count; }

 private:
  duration& _distance(factory_id left, factory_id right) {
    return _durations[left.value * static_cast<id_t>(_count) + right.value];
  }

  void swap(graph& g) {
    using std::swap;
    swap(_count, g._count);
    swap(_durations, g._durations);
  }
};

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_GRAPH_HPP