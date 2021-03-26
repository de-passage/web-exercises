#include <gtest/gtest.h>

#include <graph.hpp>

using f = gitc::factory;
using w = gitc::weight;

TEST(Graph, ShouldConstruct) {
  gitc::graph g{2};
  g.add_edge(f{0}, f{1}, w{3});
  ASSERT_EQ(g.node_count(), 2);
  ASSERT_EQ(g.distance(f{0}, f{0}), w{0});
  ASSERT_EQ(g.distance(f{1}, f{1}), w{0});
  ASSERT_EQ(g.distance(f{0}, f{1}), w{3});
  ASSERT_EQ(g.distance(f{1}, f{0}), w{3});
}