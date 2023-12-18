#include <gtest/gtest.h>

#include <graph.hpp>

using gitc::graph;
using f = gitc::factory_id;
using w = gitc::duration;

graph make_graph(int n) {
  assert(n > 0);
  graph g{static_cast<size_t>(n)};
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j)
      g.add_edge(f{i}, f{j}, w{static_cast<int>(i + j)});
  }
  return g;
}

#define CHECK_EDGES(g)                                     \
  for (size_t i = 0; i < g.node_count() - 1; ++i) {        \
    for (size_t j = i + 1; j < g.node_count(); ++j) {      \
      ASSERT_EQ(g.distance(f{static_cast<gitc::id_t>(i)},  \
                           f{static_cast<gitc::id_t>(j)}), \
                w{static_cast<int>(i + j)});               \
      ASSERT_EQ(g.distance(f{static_cast<gitc::id_t>(j)},  \
                           f{static_cast<gitc::id_t>(i)}), \
                w{static_cast<int>(i + j)});               \
    }                                                      \
  }

TEST(Graph, ShouldConstruct) {
  graph g{2};
  g.add_edge(f{0}, f{1}, w{3});
  ASSERT_EQ(g.node_count(), 2);
  ASSERT_EQ(g.distance(f{0}, f{0}), w{0});
  ASSERT_EQ(g.distance(f{1}, f{1}), w{0});
  ASSERT_EQ(g.distance(f{0}, f{1}), w{3});
  ASSERT_EQ(g.distance(f{1}, f{0}), w{3});
}

TEST(Graph, ShouldMove) {
  const int size = 8;
  graph g1{make_graph(size)};
  ASSERT_EQ(g1.node_count(), size);
  CHECK_EDGES(g1)

  graph g2{std::move(g1)};
  ASSERT_EQ(g2.node_count(), size);
  ASSERT_EQ(g1.node_count(), 0);
  CHECK_EDGES(g2);

  g1 = std::move(g2);
  ASSERT_EQ(g1.node_count(), size);
  ASSERT_EQ(g2.node_count(), 0);
  CHECK_EDGES(g1);
}

TEST(Graph, ShouldCopy) {
  const int size = 8;
  graph g1{make_graph(size)};
  ASSERT_EQ(g1.node_count(), size);
  CHECK_EDGES(g1)

  graph g2{g1};
  ASSERT_EQ(g2.node_count(), size);
  ASSERT_EQ(g1.node_count(), size);
  CHECK_EDGES(g1);
  CHECK_EDGES(g2);

  graph g3{1};
  ASSERT_EQ(g3.node_count(), 1);
  CHECK_EDGES(g3)
  g3 = g2;
  ASSERT_EQ(g3.node_count(), size);
  ASSERT_EQ(g2.node_count(), size);
  CHECK_EDGES(g3);
  CHECK_EDGES(g2);
}

TEST(Graph, ShouldSwap) {
  const int s1 = 8;
  const int s2 = 5;
  graph g1{make_graph(s1)};
  graph g2{make_graph(s2)};
  ASSERT_EQ(g1.node_count(), s1);
  ASSERT_EQ(g2.node_count(), s2);

  using std::swap;
  swap(g1, g2);
  ASSERT_EQ(g1.node_count(), s2);
  ASSERT_EQ(g2.node_count(), s1);
  CHECK_EDGES(g1);
  CHECK_EDGES(g2);
}