#include "decisions.hpp"
#include "graph.hpp"
#include "types.hpp"

#include <stdexcept>
#include <unordered_set>

namespace simulator {

using gitc::bomb_container;
using gitc::bomb_id;
using gitc::bomb_info;
using gitc::decision_list;
using gitc::duration;
using gitc::factory_container;
using gitc::factory_id;
using gitc::factory_info;
using gitc::graph;
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
  owner_type winner;
};
constexpr static end draw{owner_type::neutral};
struct forfeit {
  enum class type { invalid_action } reason;
  owner_type winner;
};

struct status {
  enum class type : char { running, ended, forfeited };

  constexpr status(running_t) : _what{}, _running{} {}
  constexpr status(end winner) : _what(type::ended), _ended{winner} {}
  constexpr status(forfeit winner) : _what{type::forfeited}, _forfeit{winner} {}

  constexpr type what() const { return _what; }
  constexpr owner_type winner() const {
    if (_what == type::ended) {
      return _ended.winner;
    }
    if (_what == type::forfeited) {
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
  template <class T>
  using iterator_vector = std::vector<typename T::iterator>;

 public:
  game(factory_container factories, graph map)
      : _factories(std::move(factories)), _map(std::move(map)) {}

  status play_one_turn(const decision_list& player_decisions,
                       const decision_list& opponent_decisions) {
    if (player_decisions.empty()) {
      throw std::runtime_error("No orders given for 'me'");
    }
    if (opponent_decisions.empty()) {
      throw std::runtime_error("No orders given for 'opponent'");
    }
    iterator_vector<troop_container> troops_at_destination;
    iterator_vector<bomb_container> bombs_at_destination;

    ++_turn;
    _move_troops_and_bombs(troops_at_destination, bombs_at_destination);
    _execute_player_orders(owner_type::me, player_decisions);
    _execute_player_orders(owner_type::opponent, opponent_decisions);
    _produce_new_cyborgs();
    _resolve_fights(troops_at_destination);
    _explode_bombs(bombs_at_destination);
    return _game_status();
  }

 private:
  troop_container _troops{};
  factory_container _factories{};
  bomb_container _bombs{};
  player_info _players[2]{};
  int _turn{0};
  int _current_troop_id{0};
  int _current_bomb_id{0};
  graph _map;

  void _move_troops_and_bombs(
      iterator_vector<troop_container>& troops_at_destination,
      iterator_vector<bomb_container>& bombs_at_destination) {
    // 1. Move troops and bombs
    for (auto troop_it = _troops.begin(); troop_it != _troops.end();
         ++troop_it) {
      auto& troop = troop_it->second;
      --troop.distance;
      if (troop.distance == 0) {
        troops_at_destination.push_back(troop_it);
      }
    }
    for (auto bomb_it = _bombs.begin(); bomb_it != _bombs.end(); ++bomb_it) {
      auto& bomb = bomb_it->second;
      --bomb.distance;
      if (bomb.distance == 0) {
        bombs_at_destination.push_back(bomb_it);
      }
    }
  }

  void _execute_player_orders(owner_type player,
                              const decision_list& player_decisions) {
    // 2. execute player orders
    std::unordered_set<factory_id> previous_moves;
    for (const auto& decision : player_decisions) {
      decision.dispatch(
          [] { /* wait */ },
          [&](const gitc::move& order) {
            // Move order
            if (previous_moves.count(order.origin) != 0) {
              throw std::runtime_error(
                  "Another order was sent from that factory");
            }
            previous_moves.insert(order.origin);

            auto t_it = _factories.find(order.destination);
            auto o_it = _factories.find(order.origin);
            if (t_it == _factories.end()) {
              throw std::runtime_error(
                  "Trying to send troops to a factory that doesn't exist");
            }
            if (o_it == _factories.end()) {
              throw std::runtime_error(
                  "Trying to send troops from a factory that doesn't exist");
            }
            auto& origin = o_it->second;
            auto target_id = t_it->first;
            auto origin_id = o_it->first;
            if (origin.owner != player) {
              throw std::runtime_error(
                  "Trying to send troops from a factory controlled by someone "
                  "else");
            }
            if (origin.cyborgs <= order.cyborgs) {
              throw std::runtime_error(
                  "Trying to send more troops than available");
            }
            auto distance = _map.distance(origin_id, target_id);

            troop_info info{
                player, origin_id, order.cyborgs, distance, target_id};
            origin.cyborgs -= order.cyborgs;
            _troops.emplace(_current_troop_id++, info);
          },
          [&](const gitc::increment_production& order) {
            // Factory production increment order
            auto it = _factories.find(order.target);
            if (it == _factories.end()) {
              throw std::runtime_error(
                  "Trying to increment the production of a "
                  "factory that doesn't exist");
            }
            auto& factory = it->second;
            if (factory.cyborgs < 10) {
              throw std::runtime_error(
                  "Target factory for production increment has "
                  "insufficient cyborgs");
            }
            if (factory.production >= 3) {
              throw std::runtime_error(
                  "Target factory for production increment "
                  "already at max capacity");
            }
            ++factory.production;
          },
          [&](const gitc::launch_bomb& order) {
            // Launch bomb order
            if ((player == owner_type::me &&
                 _players[0].available_bombs-- <= 0) ||
                (player == owner_type::opponent &&
                 _players[1].available_bombs-- <= 0)) {
              throw std::runtime_error("Insufficient bombs to send");
            }
            if (previous_moves.count(order.origin) != 0) {
              throw std::runtime_error(
                  "Another order was sent from that factory");
            }
            previous_moves.insert(order.origin);

            auto t_it = _factories.find(order.destination);
            auto o_it = _factories.find(order.origin);
            if (t_it == _factories.end()) {
              throw std::runtime_error(
                  "Trying to send a bomb to a factory that doesn't exist");
            }
            if (o_it == _factories.end()) {
              throw std::runtime_error(
                  "Trying to send a bomb from a factory that doesn't exist");
            }
            auto& origin = o_it->second;
            auto target_id = t_it->first;
            auto origin_id = o_it->first;
            if (origin.owner != player) {
              throw std::runtime_error(
                  "Trying to send a bomb from a factory controlled by someone "
                  "else");
            }

            auto distance = _map.distance(origin_id, target_id);
            bomb_info info{player, origin_id, target_id, distance};
            _bombs.emplace(_current_bomb_id++, info);
          });
    }
  }

  void _produce_new_cyborgs() {
    // 3. Produce new cyborgs
    for (auto& factory : _factories) {
      if (factory.second.inactivity > 0) {
        --factory.second.inactivity;
      }
      else {
        factory.second.cyborgs +=
            (factory.second.owner == owner_type::neutral
                 ? strength{0}
                 : strength{factory.second.production.value});
      }
    }
  }

  void _resolve_fights(
      const iterator_vector<troop_container>& troops_at_destination) {
    // 4. resolve fights
    struct fight {
      strength me_troops{};
      strength opponent_troops{};
    };
    std::unordered_map<factory_id, fight> fights;

    // Aggregate different troops reaching potentially same factories, and
    // remove them from the list
    for (const auto& troop_it : troops_at_destination) {
      auto& troop = troop_it->second;
      if (troop.owner == owner_type::me) {
        fights[troop.target].me_troops += troop.cyborgs;
      }
      else {
        fights[troop.target].opponent_troops += troop.cyborgs;
      }

      _troops.erase(troop_it);
    }
    // Resolve fights factory by factory. Newcomers first, then against defenses
    for (auto& fight : fights) {
      owner_type winner = owner_type::neutral;
      strength leftover{};
      if (fight.second.me_troops > fight.second.opponent_troops) {
        leftover = fight.second.me_troops - fight.second.opponent_troops;
        winner = owner_type::me;
      }
      else {
        leftover = fight.second.opponent_troops - fight.second.me_troops;
        winner = owner_type::opponent;
      }

      auto factory_it = _factories.find(fight.first);
      if (factory_it == _factories.end()) {
        throw std::logic_error("Fighting over a factory that doesn't exist");
      }

      auto& factory = factory_it->second;
      if (winner == factory.owner) {
        factory.cyborgs += leftover;
      }
      else if (factory.cyborgs >= leftover) {
        factory.cyborgs -= leftover;
      }
      else {
        factory.cyborgs = leftover - factory.cyborgs;
        factory.owner = winner;
      }
    }
  }

  void _explode_bombs(
      const iterator_vector<bomb_container>& bombs_at_destination) {
    // 5. bombs explode
    for (const auto& bomb_it : bombs_at_destination) {
      auto& bomb = bomb_it->second;

      auto factory_it = _factories.find(bomb.target);
      if (factory_it == _factories.end()) {
        throw std::logic_error("Trying to bomb a factory that doesn't exist.");
      }
      auto& factory = factory_it->second;

      auto destroyed = std::max(factory.cyborgs / 2, strength{10});
      factory.cyborgs = factory.cyborgs >= destroyed
                            ? factory.cyborgs - destroyed
                            : strength{0};
      factory.inactivity = duration{5};

      _bombs.erase(bomb_it);
    }
  }

  status _game_status() const {
    // 6. Check for game end
    // If a player has 0 factories and 0 troops, he loses. otherwise if we reach
    // turn 200, the player with the most cyborgs wins
    strength by_player[2]{strength{0}, strength{0}};
    for (const auto& troop : _troops) {
      if (troop.second.owner == owner_type::me) {
        by_player[0] += troop.second.cyborgs;
      }
      else if (troop.second.owner == owner_type::opponent) {
        by_player[1] += troop.second.cyborgs;
      }
    }
    for (const auto& factory : _factories) {
      if (factory.second.owner == owner_type::me) {
        by_player[0] += factory.second.cyborgs;
      }
      else if (factory.second.owner == owner_type::opponent) {
        by_player[1] += factory.second.cyborgs;
      }
    }

    if (by_player[0] == 0) {
      return end{owner_type::opponent};
    }
    if (by_player[1] == 0) {
      return end{owner_type::me};
    }

    if (_turn == 200) {
      if (by_player[0] > by_player[1]) {
        return end{owner_type::me};
      }
      if (by_player[0] < by_player[1]) {
        return end{owner_type::opponent};
      }
      return draw;
    }

    return run;
  }
};

}  // namespace simulator

int main() {
  return 1;
}