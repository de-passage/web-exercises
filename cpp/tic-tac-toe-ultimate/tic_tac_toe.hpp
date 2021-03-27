#ifndef GUARD_DPSG_TTT_ULTIMATE_HPP
#define GUARD_DPSG_TTT_ULTIMATE_HPP

#include <algorithm>
#include <utility>
#include <vector>

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
enum class player : int { me = 1, opponent = -1, none = 0 };
player& operator+=(player& left, player right) {
  left = static_cast<player>(static_cast<int>(left) + static_cast<int>(right));
  return left;
}
player operator+(player left, player right) {
  return left += right;
}
player operator/(player left, int right) {
  return static_cast<player>(static_cast<int>(left) / right);
}
using action = std::pair<size_t, size_t>;
using action_list = std::vector<action>;

struct won {
  player winner;
};
struct ongoing {
  player next;
};
struct invalid_t {
} invalid;
struct empty_t {};
struct draw_t {
} draw;
class play_status {
  enum { invalid, won, draw, ongoing };

 public:
  play_status(draw_t) : _none{}, _status{draw} {}
  play_status(struct won winner) : _won{winner}, _status{won} {}
  play_status(invalid_t) : _none{}, _status{invalid} {}
  play_status(struct ongoing next) : _ongoing{next}, _status{ongoing} {}

  template <class I, class D, class W, class O>
  decltype(auto) dispatch(I&& w_invalid, D&& w_draw, W&& w_won, O&& w_ongoing) {
    switch (_status) {
      case won:
        return w_won(_won);
      case ongoing:
        return w_ongoing(_ongoing);
      case draw:
        return w_draw(::draw);
      default:
        return w_invalid(::invalid);
    }
  }

 private:
  union {
    empty_t _none;
    struct won _won;
    struct ongoing _ongoing;
  };
  int _status;
};

class board {
 public:
  player& at(size_t i, size_t j) { return _board[i][j]; }
  player at(size_t i, size_t j) const { return _board[i][j]; }
  player& at(const action& a) { return at(a.first, a.second); }
  player at(const action& a) const { return at(a.first, a.second); }

  player winner() const {
    player sumd1 = player::none;
    player sumd2 = player::none;
    for (size_t i = 0; i < 3; ++i) {
      player suml = player::none;
      player sumc = player::none;
      for (size_t j = 0; j < 3; ++j) {
        suml += at(i, j);
        sumc += at(j, i);
      }
      if (abs(static_cast<int>(suml)) == 3) {
        return player(static_cast<int>(suml) / 3);
      }
      if (abs(static_cast<int>(sumc)) == 3) {
        return player(static_cast<int>(sumc) / 3);
      }
      sumd1 += at(i, i);
      sumd2 += at(i, 2 - i);
    }
    if (abs(static_cast<int>(sumd1)) == 3) {
      return player(static_cast<int>(sumd1) / 3);
    }
    if (abs(static_cast<int>(sumd2)) == 3) {
      return player(static_cast<int>(sumd2) / 3);
    }
    return player::none;
  }

 private:
  player _board[3][3]{{player::none}};
};

class game {
 public:
  game(player current) : _current_player(current) {}

  play_status play_at(const action& a) { return play_at(a.first, a.second); }

  play_status play_at(size_t x, size_t y) {
    player& cell = _board.at(x, y);
    if (cell != player::none) {
      return invalid;
    }

    cell = _current_player;

    if (_board.winner() == player::none) {
      for (size_t i = 0; i < 3; ++i) {
        for (size_t j = 0; j < 3; ++j) {
          if (_board.at(i, j) == player::none) {
            _current_player =
                static_cast<player>(static_cast<int>(_current_player) * -1);
            return ongoing{_current_player};
          }
        }
      }
      return draw;
    }
    else
      return won{_current_player};
  }

  action_list valid_moves() const {
    action_list result;
    result.reserve(9);
    for (size_t i = 0; i < 3; ++i) {
      for (size_t j = 0; j < 3; ++j) {
        if (_board.at(i, j) == player::none) {
          result.emplace_back(i, j);
        }
      }
    }
    return result;
  }

  player current_player() const { return _current_player; }

 private:
  board _board{};
  player _current_player;
};

action best_move(const game& g, player current_player);

int simulate_score(action act, game g, player p) {
  auto r = g.play_at(act);
  return r.dispatch(
      [](invalid_t) -> int { throw std::runtime_error("invalid move"); },
      [](draw_t) { return 0; },
      [p](won winner) {
        return winner.winner == p ? std::numeric_limits<int>::max()
                                  : std::numeric_limits<int>::min();
      },
      [&g, p](ongoing) { return simulate_score(best_move(g, p), g, p); });
}

action best_move(const game& g, player current_player) {
  action_list list = g.valid_moves();
  if (find(list.begin(), list.end(), action{1, 1}) != list.end()) {
    return action{1, 1};
  }
  action& least_bad = list.front();
  int least_bad_score = std::numeric_limits<int>::min();
  for (auto& move : list) {
    auto current_score = simulate_score(move, g, current_player);
    if (current_score == std::numeric_limits<int>::max()) {
      return move;
    }
    else if (current_score > least_bad_score) {
      least_bad_score = current_score;
      least_bad = move;
    }
  }
  return least_bad;
}

#endif  // GUARD_DPSG_TTT_ULTIMATE_HPP