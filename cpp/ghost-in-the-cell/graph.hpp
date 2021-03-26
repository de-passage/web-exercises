#ifndef GUARD_DPSG_GITC_GRAPH_HPP
#define GUARD_DPSG_GITC_GRAPH_HPP

#include <numeric>
#include <utility>

namespace gitc {

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
                 weight{i == j ? 0 : std::numeric_limits<int>::max()});
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
      : base{std::exchange(to_copy._weights, nullptr),
             std::exchange(to_copy._count, 0)} {}

  graph& operator=(const graph& g) {
    graph{g}.swap(*this);
    return *this;
  }

  graph& operator=(graph&& g) {
    graph{std::move(g)}.swap(*this);
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

}  // namespace gitc

#endif  // GUARD_DPSG_GITC_GRAPH_HPP