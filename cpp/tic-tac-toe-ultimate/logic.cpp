#include <gtest/gtest.h>

#include "./tic_tac_toe.hpp"

TEST(Utility, PlayerNegationWorks) {
  ASSERT_EQ(~player::me, player::opponent);
  ASSERT_EQ(~player::opponent, player::me);
  ASSERT_EQ(~player::none, player::none);
}

TEST(Game, PlayingShouldGiveHandToNextPlayer) {
  game g(player::me);
  ASSERT_EQ(g.current_player(), player::me);
  g.play_at(0, 0);
  ASSERT_EQ(g.current_player(), player::opponent);
  g.play_at(0, 1);
  ASSERT_EQ(g.current_player(), player::me);
  g.play_at(0, 2);
  ASSERT_EQ(g.current_player(), player::opponent);
}

TEST(Game, InvalidPlayShouldntAlterState) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(0, 0);
  ASSERT_EQ(g.current_player(), player::opponent);
  ASSERT_EQ(g.valid_moves().size(), 8);
}

TEST(Game, VictoryShouldReturnTheVictorsID) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(1, 0);
  g.play_at(0, 1);
  g.play_at(1, 1);
  auto value = g.play_at(0, 2);
  value.dispatch([](invalid_t) { FAIL(); },
                 [](draw_t) { FAIL(); },
                 [](won victory) { ASSERT_EQ(victory.winner, player::me); },
                 [](ongoing) { FAIL(); });
}

TEST(Logic, ScoreSimulationShouldReturnLessThan0ForImmediateDefeat) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(1, 0);
  g.play_at(0, 1);
  g.play_at(1, 1);
  ASSERT_LT(simulate_score(action{2, 0}, g), 0);
}

TEST(Logic, ScoreSimulationShouldReturnMoreThan0ForImmediateVictory) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(1, 0);
  g.play_at(0, 1);
  g.play_at(1, 1);
  ASSERT_GT(simulate_score(action{0, 2}, g), 0);
}

TEST(Logic, BestMoveShouldReturn) {
  game g(player::me);
  ASSERT_EQ(best_move(g), (action{1, 1}));
}

TEST(Logic, ShouldTakeCenter) {
  game g(player::opponent);
  g.play_at(0, 0);
  ASSERT_EQ(best_move(g), (action{1, 1}));
}

TEST(Logic, ShouldPlayCorrectlyWithOnlyThreeChoices) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(1, 1);
  g.play_at(0, 1);
  g.play_at(0, 2);
  g.play_at(2, 0);
  g.play_at(1, 0);
  ASSERT_EQ(best_move(g), (action{1, 2}));
  g.play_at(2, 2);
  ASSERT_EQ(best_move(g), (action{1, 2}));
}

TEST(Logic, ShouldPlayCorrectlyWithOnlyFourChoices) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(1, 1);
  g.play_at(0, 1);
  g.play_at(0, 2);
  g.play_at(2, 0);
  ASSERT_EQ(best_move(g), (action{1, 0}));
}

TEST(Logic, ShouldAvoidImmediateLoss) {
  game g(player::me);
  g.play_at(0, 0);
  g.play_at(2, 0);
  g.play_at(1, 0);
  g.play_at(2, 1);
  ASSERT_EQ(best_move(g), (action{2, 2}));
}

TEST(Logic, ShouldWinImmediately) {
  game g(player::me);
  g.play_at(0, 2);
  g.play_at(1, 1);
  g.play_at(1, 2);
  g.play_at(2, 0);
  ASSERT_EQ(best_move(g), (action{2, 2}));
}