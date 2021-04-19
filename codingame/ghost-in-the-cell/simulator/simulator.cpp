#include "types.hpp"

#include "decisions.hpp"

#include <stdexcept>

namespace simulator {

using gitc::bomb_container;
using gitc::bomb_id;
using gitc::bomb_info;
using gitc::decision_list;
using gitc::duration;
using gitc::factory_container;
using gitc::factory_id;
using gitc::factory_info;
using gitc::owner_type;
using gitc::production_capacity;
using gitc::strength;
using gitc::troop_container;
using gitc::troop_id;
using gitc::troop_info;

enum class player : int { neutral = 0, player1 = 1, player2 = 2 };

struct player_info {
  int available_bombs{2};
};
struct running_t {
} constexpr static run;
struct end {
  player winner;
};
struct forfeit {
  enum class type { invalid_action } reason;
  player winner;
};

struct status {
  enum class type : char { running, ended, forfeited };

  constexpr status() : _what{}, _running{} {}
  constexpr explicit status(end winner) : _what(type::ended), _ended{winner} {}
  constexpr explicit status(forfeit winner)
      : _what{type::forfeited}, _forfeit{winner} {}

  constexpr type what() const { return _what; }
  constexpr player winner() const {
    if (_what == type::ended) {
      return _ended.winner;
    }
    else if (_what == type::forfeited) {
      return _forfeit.winner;
    }
    throw std::out_of_range("No winner, the game is still running");
  }

 private:
  type _what{type::running};
  union {
    running_t _running;
    end _ended;
    forfeit _forfeit;
  };
};

class game {
 public:
  game() = default;

  status play_one_turn(const decision_list& player_decisions) {
    std::vector<troop_id> troops_at_destination;
    std::vector<bomb_id> bombs_at_destination;

    for (auto& troop : _troops) {
      --troop.second.distance;
      if (troop.second.distance == 0) {
        troops_at_destination.push_back(troop.first);
      }
    }
    for (auto& bomb : _bombs) {
      --bomb.second.distance;
      if (bomb.second.distance == 0) {
        bombs_at_destination.push_back(bomb.first);
      }
    }

    // 2. execute player orders
    for (const auto& decision : player_decisions) {
      decision.dispatch([] {},
                        [](const gitc::move& order) {},
                        [](const gitc::increment_production& order) {},
                        [](const gitc::launch_bomb& order) {});
    }

    // 3. Produce new cyborgs
    for (auto& factory : _factories) {
      factory.second.cyborgs +=
          (factory.second.owner == owner_type::neutral
               ? strength{0}
               : strength{factory.second.production.value});
    }

    // 4. resolve fights
    for (auto& troop : troops_at_destination) {
    }

    // 5. bombs explode
    for (auto& bombs : bombs_at_destination) {
    }

    // 6. Check for game end
    // If a player has 0 factories and 0 troops, he loses. otherwise if we reach
    // turn 200, the player with the most cyborgs wins
  }

 private:
  troop_container _troops{};
  factory_container _factories{};
  bomb_container _bombs{};
  player_info players[2]{};
};

}  // namespace simulator

int main() {
  return 1;
}