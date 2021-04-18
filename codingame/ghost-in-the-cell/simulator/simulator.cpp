#include "types.hpp"

#include <stdexcept>

namespace simulator {

using gitc::bomb_id;
using gitc::duration;
using gitc::factory_id;
using gitc::production_capacity;
using gitc::strength;
using gitc::troop_id;

enum class player : int { neutral = 0, player1 = 1, player2 = 2 };

struct factory {
  player owner{player::neutral};
  strength cyborgs;
  production_capacity production;
  duration inactivity{0};
};

struct troop {
  constexpr troop(player o, strength c, factory_id f, factory_id t, duration d)
      : owner{o}, cyborgs{c}, from{f}, to{t}, distance{d} {
    if (o == player::neutral) {
      throw std::runtime_error("Neutral player cannot have troops");
    }
  }
  player owner;
  strength cyborgs;
  factory_id from;
  factory_id to;
  duration distance;
};

struct bomb {
  constexpr bomb(player o, factory_id f, factory_id t, duration d)
      : owner{o}, from{f}, to{t}, distance{d} {}

  player owner;
  factory_id from;
  factory_id to;
  duration distance;
};

struct player_info {
  int available_bombs{2};
};

struct status {
  enum class type : char { running, ended };

  constexpr status() : _what(type::running), _winner{player::neutral} {}
  constexpr explicit status(player winner)
      : _what(type::ended), _winner{winner} {}

  constexpr type what() const { return _what; }
  constexpr player winner() const {
    if (_what == type::ended) {
      return _winner;
    }
    throw std::out_of_range("No winner, the game is still running");
  }

 private:
  type _what;
  player _winner;
};

class game {
 public:
  game() = default;

  status play_one_turn() {
    // 1. move troops and bombs
    // 2. execute player orders
    // 3. produce new cyborgs
    // 4. resolve fights
    // 5. bombs explode
    // 6. Check for game end
  }

 private:
};

}  // namespace simulator

int main() {
  return 1;
}