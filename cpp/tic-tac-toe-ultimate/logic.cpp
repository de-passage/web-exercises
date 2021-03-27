#include <gtest/gtest.h>

#include "./tic_tac_toe.hpp"

TEST(Logic, BestMoveShouldReturn) {
  game g(player::me);
  ASSERT_EQ(best_move(g, player::me), (action{1, 1}));
}

TEST(Logic, ShouldTakeCenter) {
  game g(player::opponent);
  g.play_at(0, 0);
  ASSERT_EQ(best_move(g, player::me), (action{1, 1}));
}

TEST(Logic, ShouldAvoidImmediateLoss) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(2, 0);
  g.play_at(1, 0);
  g.play_at(2, 1);
  ASSERT_EQ(best_move(g, player::me), (action{2, 2}));
}

TEST(Logic, ShouldWinImmediately) {
  game g(player::me);
  g.play_at(0, 2);
  g.play_at(1, 1);
  g.play_at(1, 2);
  g.play_at(2, 0);
  ASSERT_EQ(best_move(g, player::me), (action{2, 2}));
}