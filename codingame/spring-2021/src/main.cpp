#include <algorithm>
#include <iostream>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

#include "strong_types.hpp"

namespace st = dpsg::strong_types;

template <class T, class ValueType, class... Ts>
struct value_impl : st::derive_t<T, Ts...> {
  using real_type = T;
  using value_type = ValueType;
  value_type value;

 protected:
  constexpr explicit value_impl(value_type type) noexcept
      : value{std::move(type)} {}

 public:
  constexpr value_impl() noexcept = default;
  constexpr value_impl(real_type lower) noexcept
      : value{std::move(lower).value} {}
};

template <class Tag>
struct strong_id : value_impl<strong_id<Tag>,
                              int,
                              st::comparable,
                              st::comparable_with<int>> {
  constexpr explicit strong_id(int id = -1)
      : value_impl<strong_id<Tag>,
                   int,
                   st::comparable,
                   st::comparable_with<int>>{id} {}
};

namespace dpsg {
namespace strong_types {
template <class T>
struct hash;
template <class T>
struct hash<strong_id<T>> {
  constexpr auto operator()(strong_id<T> id) const {
    return ::std::hash<int>()(id.value);
  }
};
}  // namespace strong_types
}  // namespace dpsg

using namespace std;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

template <template <class...> class R,
          class C,
          class F,
          class... Args,
          class I = decltype(
              std::declval<F>()(std::declval<typename C::value_type>()))>
R<I> transform(const C& container, F&& transformer) {
  R<I> result;
  std::transform(container.begin(),
                 container.end(),
                 std::back_inserter(result),
                 FWD(transformer));
  return result;
}

constexpr int DAY_COUNT = 24;
constexpr int DAY_MAX = DAY_COUNT - 1;
constexpr int TREE_MAX = 3;
constexpr int COMPLETE_COST = 4;
constexpr int NEIGHBOR_COUNT = 6;
constexpr int CELL_COUNT = 37;
constexpr int GROW_COST_BASE[3] = {1, 3, 7};
constexpr int growth_cost_base(int size) {
  return GROW_COST_BASE[size];
}
constexpr int OPPOSITE_DIRECTION[6] = {3, 4, 5, 0, 1, 2};

using cell_id = strong_id<struct cell>;
constexpr cell_id invalid_cell{-1};
struct cell {
  int richness;
  cell_id neighbors[NEIGHBOR_COUNT];
};

std::string to_string(cell_id id) {
  return to_string(id.value);
}

struct tree {
  cell_id cell{-1};
  int size;
  bool mine;
  bool dormant;

  bool is_completeable() const { return size >= TREE_MAX; }
  bool is_actionable(const struct player& player) const;
};

const auto tree_size = [](const auto& t) -> int { return t.second.size; };
const auto of_size = [](int size) {
  return [size](const auto& p) { return tree_size(p) == size; };
};
const auto always = [](const auto& v) {
  return [v](const auto&...) { return v; };
};
const auto zero_ = always(0);
const auto true_ = always(true);
const auto false_ = always(false);

struct player {
  int score;
  int sun;

  using tree_container = unordered_map<cell_id, tree, st::hash<cell_id>>;
  tree_container trees{};

  bool can_complete_lifecycle() const { return sun >= 4; }

  template <class T, class F>
  auto tree_or(cell_id cell, T&& with_tree, F&& without) const {
    auto it = trees.find(cell);
    if (it != trees.end()) {
      return FWD(with_tree)(*it);
    }
    return FWD(without)();
  }

  int tree_size(cell_id cell) const {
    return tree_or(cell, ::tree_size, zero_);
  }

  int trees_of_size(int size) const {
    return static_cast<int>(
        count_if(trees.begin(), trees.end(), of_size(size)));
  }

  bool can_plant() const { return trees.size() > 0 && sun >= trees_of_size(0); }
};

int growth_cost(const tree& tree, const player& p) {
  if (tree.size < 0 || tree.size >= 3)
    throw runtime_error("growth_cost called for a tree that cannot grow");

  return growth_cost_base(tree.size) + p.trees_of_size(tree.size + 1);
}

bool tree::is_actionable(const player& player) const {
  return !dormant &&
         (is_completeable() ? player.sun >= COMPLETE_COST
                            : player.sun >= growth_cost(*this, player));
}

struct game {
  vector<cell> cells{};
  int day;
  int nutrients;
  int score;
  player me;
  player opponent;
  bool opponent_waiting;

  cell& cell_at(cell_id cell) {
    return cell >= 0 && cell < CELL_COUNT
               ? cells[static_cast<size_t>(cell.value)]
               : throw std::out_of_range("invalid cell id " +
                                         to_string(cell.value));
  }

  const cell& cell_at(cell_id cell) const {
    return const_cast<game*>(this)->cell_at(cell);
  }

  int richness_of(const tree& t) const { return cell_at(t.cell).richness; }

  int richness_of(cell_id cell) const { return cell_at(cell).richness; }

  template <class T, class F>
  auto tree_at(cell_id cell, T&& with_tree, F&& without) const {
    return me.tree_or(cell, FWD(with_tree), [&] {
      return opponent.tree_or(cell, forward<T>(with_tree), forward<F>(without));
    });
  }

  int tree_size(cell_id cell) const {
    return tree_at(cell, ::tree_size, zero_);
  }

  int sun_direction() const { return sun_after(0); }

  int sun_after(int offset) const { return (day + offset) % 6; }

  int distance(cell_id left, cell_id right) const {
    return _distance_lookup[left.value][right.value];
  }

  cell_id move(cell_id source, int direction) const {
    return direction >= 0 && direction < 6
               ? cell_at(source).neighbors[static_cast<size_t>(direction)]
               : throw std::out_of_range("invalid direction");
  }

  size_t tree_count() const { return me.trees.size() + opponent.trees.size(); }

 private:
  int _distance_lookup[37][37]{{-1}};
  int _compute_distance(cell_id start, cell_id end) {
    queue<pair<cell_id, int>> to_explore;
    unordered_set<cell_id, st::hash<cell_id>> explored;
    to_explore.push({start, 0});

    while (!to_explore.empty()) {
      auto c = to_explore.front();
      to_explore.pop();
      explored.insert(c.first);
      if (c.first == end)
        return c.second;

      for (int i = 0; i < NEIGHBOR_COUNT; ++i) {
        cell_id n = move(c.first, i);
        if (n < 0 || explored.count(n))
          continue;
        to_explore.push({n, c.second + 1});
      }
    }
    throw runtime_error("didn't find a path between " + to_string(start.value) +
                        " " + to_string(end.value));
  }

 public:
  void initialize_lookup() {
    if (_distance_lookup[0][0] == 0)
      return;
    for (int i = 0; i < CELL_COUNT - 1; ++i) {
      _distance_lookup[i][i] = 0;
      for (int j = i + 1; j < CELL_COUNT; ++j) {
        _distance_lookup[i][j] = _distance_lookup[j][i] =
            _compute_distance(cell_id(i), cell_id(j));
      }
    }
  }
};

struct wait_t {
} constexpr wait{};
struct complete {
  constexpr explicit complete(const tree& t) : tree{t.cell} {}
  constexpr explicit complete(const cell_id& c) : tree(c) {}
  cell_id tree;
};
struct grow {
  constexpr explicit grow(const tree& t) : cell{t.cell} {}
  constexpr explicit grow(const cell_id& c) : cell(c) {}
  cell_id cell;
};
struct seed {
  constexpr explicit seed(cell_id origin, cell_id cell)
      : source{origin}, target{cell} {}
  cell_id source;
  cell_id target;
};

struct action {
  enum type { wait, complete, grow, seed };
  constexpr action(wait_t) : _type{type::wait}, _wait{} {}
  constexpr action(struct complete c) : _type{type::complete}, _complete{c} {}
  constexpr action(struct grow g) : _type{type::grow}, _grow{g} {}
  constexpr action(struct seed s) : _type{type::seed}, _seed{s} {}

  template <class W, class C, class G, class S>
  constexpr auto dispatch(W&& with_wait,
                          C&& with_complete,
                          G&& with_grow,
                          S&& with_seed) const {
    switch (_type) {
      case type::wait:
        return FWD(with_wait)(_wait);
      case type::complete:
        return FWD(with_complete)(_complete);
      case type::grow:
        return FWD(with_grow)(_grow);
      case type::seed:
        return FWD(with_seed)(_seed);
      default:
        throw std::runtime_error("invalid inner type");
    }
  }

 private:
  type _type;
  union {
    wait_t _wait;
    struct complete _complete;
    struct grow _grow;
    struct seed _seed;
  };
};

ostream& operator<<(ostream& out, const action& action) {
  return out << action.dispatch([](wait_t) -> string { return "WAIT"; },
                                [](const complete& c) -> string {
                                  return "COMPLETE " + to_string(c.tree);
                                },
                                [](const grow& g) -> string {
                                  return "GROW " + to_string(g.cell);
                                },
                                [](const seed& s) -> string {
                                  return "SEED " + to_string(s.source) + " " +
                                         to_string(s.target);
                                });
}

const auto get_second = [](const auto& p) { return p.second; };

cell_id can_plant(cell_id cell, const player& player, const game& game) {
  cell_id result = invalid_cell;
  int distance = 4;
  for (const auto& tree : player.trees) {
    if (tree.second.cell != cell && !tree.second.dormant) {
      int d = game.distance(tree.second.cell, cell);
      if (tree.second.size >= d && d < distance) {
        distance = d;
        result = tree.first;
      }
    }
  }
  return result;
}

struct change {
  int tree_size;
  bool same_player;
};
using change_map = unordered_map<cell_id, change, st::hash<cell_id>>;

// Computes the sun generated at turn +offset by 'player' given the 'changes'
int sun_generated(const game& game,
                  const player& player,
                  int offset,
                  const change_map& changes) {
  int sun_points = 0;
  int sun_direction = game.sun_after(offset);
  int lookup_direction = OPPOSITE_DIRECTION[sun_direction];

  const auto find_tree = [&](cell_id c, int& tree_size, bool& same_player) {
    auto ch = changes.find(c);
    if (ch != changes.end()) {
      tree_size = ch->second.tree_size;
      same_player = ch->second.same_player;
    }
    else {
      player.tree_or(
          c,
          [&](const auto& tr) {
            tree_size = tr.second.size;
            same_player = true;
          },
          [&] {
            game.tree_at(
                c,
                [&](const auto& tree) {
                  tree_size = tree.second.size;
                  same_player = false;
                },
                [&] { /* there is no tree at this spot */ });
          });
    }
  };

  for (cell_id c{0}; c <= CELL_COUNT; ++c.value) {
    // get the info we want about the tree, taking the changes into account
    int tree_size = -1;
    bool belongs_to_player = false;
    find_tree(c, tree_size, belongs_to_player);

    // No tree of value here, try next cell;
    if (tree_size < 1 || !belongs_to_player)
      continue;

    // Compute the shadow at this cell, considering the changes
    int max_size = 0;
    cell_id current = c;
    for (int i = 1; i <= 3; ++i) {
      current = game.move(current, lookup_direction);
      if (current == invalid_cell) {
        break;
      }

      int size = -1;
      find_tree(current,
                size,
                belongs_to_player);  // we don't care about that bool anymore
      if (size >= i && size > max_size) {
        max_size = size;
      }
    }

    // we only get points if the size is bigger than the shadow
    if (max_size < tree_size) {
      sun_points += tree_size;
    }
  }

  return sun_points;
}

int sun_generated_by_me(const game& game,
                        int offset = 1,
                        const change_map& changes = {}) {
  return sun_generated(game, game.me, offset, changes);
}

int sun_generated_by_opponent(const game& game,
                              int offset = 1,
                              const change_map& changes = {}) {
  return sun_generated(game, game.opponent, offset, changes);
}

int sun_generated_by_me(const game& game, int offset, cell_id tree) {
  auto t = game.me.tree_or(tree, tree_size, zero_);
  change_map changes;
  changes.emplace(tree, change{std::min(t + offset, 3), true});
  return sun_generated_by_me(game, offset, changes);
}

int sun_generated_by_opponent(const game& game, int offset, cell_id tree) {
  auto t = game.opponent.tree_or(tree, tree_size, zero_);
  change_map changes;
  changes.emplace(tree, change{std::min(t + offset, 3), true});
  return sun_generated_by_opponent(game, offset, changes);
}

int my_sun_generated_with_seed(const game& game, int offset, cell_id seed) {
  change_map changes;
  changes.emplace(seed, change{std::max(0, std::min(offset - 1, 3)), true});
  return sun_generated_by_me(game, offset, changes);
}

int opponent_sun_generated_with_seed(const game& game,
                                     int offset,
                                     cell_id seed) {
  change_map changes;
  changes.emplace(seed, change{std::max(0, std::min(offset - 1, 3)), false});
  return sun_generated_by_opponent(game, offset, changes);
}

// Return true if this spot doesn't shadow any size 2 tree
bool no_shadowing(cell_id c, const game& game) {
  for (int dir = 0; dir < 6; ++dir) {
    cell_id current = c;
    for (int i = 1; i <= 2; ++i) {
      current = game.move(current, dir);
      if (current == invalid_cell)
        break;
      if (game.me.tree_or(current, true_, false_)) {
        return false;
      }
    }
  }

  return true;
}

class cache {
 public:
  void compute(const game& game) {
    for (int i = 0; i < 6; ++i) {
      _my_sun_generation[i] = sun_generated_by_me(game, i);
      _opponent_sun_generation[i] = sun_generated_by_opponent(game, 1);
    }
  }

  int opponent_sun_generation(int i = 0) const {
    return _opponent_sun_generation[i % 6];
  }

  int my_sun_generation(int i = 0) const { return _my_sun_generation[i % 6]; }

 private:
  int _my_sun_generation[6];
  int _opponent_sun_generation[6];
} cache;

seed best_spot_to_plant(const game& game) {
  if (game.me.can_plant() &&
      (game.me.trees.size() < game.opponent.trees.size() ||
       game.me.trees_of_size(0) == 0)) {
    cell_id best = invalid_cell;
    cell_id best_tree = invalid_cell;
    int best_yield = -1;
    int best_sun_value = 0;

    for (cell_id c{0}; c < CELL_COUNT; ++c.value) {
      if ((game.tree_count() < 14 && !no_shadowing(c, game)) ||
          game.tree_at(c, true_, false_)) {
        continue;  // can't plant on occupied cell, and don't want to plant on
                   // shadowed tiles
      }
      cell_id source = can_plant(c, game.me, game);
      if (source == invalid_cell) {
        continue;  // can't plant if no tree in range or all asleep
      }

      int total_sun_gen = 0;
      for (int i = 2; i < 7; ++i) {
        int m = my_sun_generated_with_seed(game, i, c);
        int o = opponent_sun_generated_with_seed(game, i, c);
        total_sun_gen += (m - cache.my_sun_generation(i)) +
                         (cache.opponent_sun_generation(i) - o);
      }

      int rc = game.richness_of(c);
      if (total_sun_gen > best_sun_value ||
          (total_sun_gen == best_sun_value && rc > best_yield)) {
        best_yield = rc;
        best = c;
        best_tree = source;
        best_sun_value = total_sun_gen;
      }
    }
    return seed{best_tree, best};
  }
  return seed{invalid_cell, invalid_cell};
}

// return -1 if fails. the best tree is a size 3 with minimum next turn shadow
// on opponent trees, and best yield (which needs to be > 0)
cell_id best_tree_to_complete(const game& game) {
  cell_id best = invalid_cell;
  int best_opponent_score = numeric_limits<int>::max();
  int best_yield = -1;
  int best_score = numeric_limits<int>::min();
  if (game.me.sun < COMPLETE_COST) {
    return best;
  }
  if (game.me.trees_of_size(3) <= game.opponent.trees_of_size(3)) {
    return best;
  }
  if (game.me.trees_of_size(1) + game.me.trees_of_size(2) <
          game.opponent.trees_of_size(1) + game.opponent.trees_of_size(2) &&
      game.me.score > game.opponent.score) {
    return best;
  }

  for (const auto& tree : game.me.trees) {
    if (tree_size(tree) < 3 ||
        game.richness_of(tree.second.cell) + game.nutrients == 0 ||
        tree.second.dormant) {
      continue;
    }

    int total_sun_gen = 0;
    int total_opponent_gen = 0;
    change_map changes;
    changes.emplace(tree.first, change{0, true});
    for (int i = 1; i <= 3; ++i) {
      total_sun_gen +=
          (cache.my_sun_generation(i) - sun_generated_by_me(game, i, changes)) /
          i;
      total_opponent_gen += (sun_generated_by_opponent(game, i, changes) -
                             cache.opponent_sun_generation(i)) /
                            i;
    }

    if (total_sun_gen < -total_opponent_gen) {
      continue;
    }

    int rc = game.richness_of(tree.second.cell);
    if (total_opponent_gen < best_opponent_score ||
        (total_opponent_gen == best_opponent_score &&
         (rc > best_yield ||
          (rc == best_yield && total_sun_gen > best_score)))) {
      best = tree.second.cell;
      best_score = total_sun_gen;
      best_opponent_score = total_opponent_gen;
      best_yield = rc;
    }
  }
  return best;
}

cell_id best_tree_to_grow(const game& game) {
  cell_id best = invalid_cell;

  int best_yield = -1;
  int worst_shadowing = std::numeric_limits<int>::min();
  int best_shadowing = 0;
  int best_size = -1;
  for (const auto& tree : game.me.trees) {
    if (tree.second.dormant || tree.second.size >= 3 ||
        game.me.sun < growth_cost(tree.second, game.me))
      continue;

    auto ts = tree_size(tree);
    change_map changes;
    changes.emplace(tree.first, change{tree.second.size + 1, true});

    int op_point = 0;
    int me_point = 0;
    for (int i = 1; i <= std::min(6, DAY_MAX - game.day); ++i) {
      op_point += (cache.opponent_sun_generation(i) -
                   sun_generated_by_opponent(game, i, changes)) /
                  i;
      me_point += (sun_generated_by_me(game, i, changes) -
                   cache.opponent_sun_generation(i)) /
                  i;
    }

    int rc = game.richness_of(tree.second.cell);

    if (op_point > worst_shadowing ||
        (op_point == worst_shadowing &&
         (me_point > best_shadowing ||
          (me_point == best_shadowing &&
           (best_yield < rc || (best_yield == rc && ts > best_size)))))) {
      best = tree.second.cell;
      best_yield = rc;
      best_shadowing = me_point;
      worst_shadowing = op_point;
      best_size = ts;
    }
  }
  return best;
}

action decide(const game& game) {
  // Goal! last turn should complete all trees on the board! -> all trees should
  // be size 3
  // 0. It's completion time! Complete a number of
  // trees, priority to those which would be shadowed next turn.
  //    Completion time:
  //      a. last turn

  if (game.day == DAY_MAX) {
    if (game.me.can_complete_lifecycle()) {
      vector<tree> v = transform<vector>(game.me.trees, get_second);
      auto last = remove_if(v.begin(), v.end(), [](const tree& t) {
        return !t.is_completeable();
      });
      auto it =
          max_element(v.begin(),
                      last,
                      [&game](const tree& left, const tree& right) -> bool {
                        return game.richness_of(left) < game.richness_of(right);
                      });
      if (it != last) {
        return complete{*it};
      }
    }
  }
  //      b. we don't have any good spot left, all trees are size 3, sun +
  //      leftover gen next turn is enough to grow
  //         replacements back
  else {
    cell_id best_tree = best_tree_to_complete(game);
    if (best_tree != invalid_cell) {
      // cerr << "can complete" << endl;
      // cerr << "best tree to complete: " << best_tree.value << endl;
      return complete{best_tree};
    }
    // 1. Grow what you can. Priority: maximize point gen + point denial next
    // turn
    else {
      // cerr << "cannot complete" << endl;
      cell_id best_tree = best_tree_to_grow(game);
      // cerr << "best tree to grow: " << best_tree.value << endl;
      if (best_tree != invalid_cell) {
        return grow{best_tree};
      }
      else {
        // cerr << "need to plant" << endl;
        // 2. Plant if can plant on good spot: no shadow with my own trees.
        // Maximize cell value. Prioritize high denial areas.
        auto best_spot = best_spot_to_plant(game);
        // cerr << "best spot to plant: " << endl;
        if (best_spot.source != invalid_cell) {
          return best_spot;
        }
      }
    }
  }
  return wait;
}

int main() {
  game game;
  size_t numberOfCells;  // 37
  cin >> numberOfCells;
  cin.ignore();
  game.cells.resize(numberOfCells);
  for (size_t i = 0; i < numberOfCells; i++) {
    cell_id index;  // 0 is the center cell, the next cells spiral outwards
    cin >> index.value;
    cell& cell_ref = game.cell_at(index);

    // 0 if the cell is unusable, 1-3 for usable cells
    cin >> cell_ref.richness;

    for (int n = 0; n < NEIGHBOR_COUNT; ++n) {
      cin >> cell_ref.neighbors[n].value;
    }
    cin.ignore();
  }

  game.initialize_lookup();

  // game loop
  while (1) {
    game.me.trees.clear();
    game.opponent.trees.clear();
    // the game lasts 24 days: 0-23
    cin >> game.day;
    cin.ignore();
    // the base score you gain from the next COMPLETE action
    cin >> game.nutrients;
    cin.ignore();
    // your sun points
    // your current score
    cin >> game.me.sun >> game.me.score;
    cin.ignore();
    // opponent's sun points
    // opponent's score
    // whether your opponent is asleep until the next day
    cin >> game.opponent.sun >> game.opponent.score >> game.opponent_waiting;
    cin.ignore();
    int numberOfTrees;  // the current amount of trees
    cin >> numberOfTrees;
    cin.ignore();
    for (int i = 0; i < numberOfTrees; i++) {
      // location of this tree
      // size of this tree: 0-3
      bool mine;  // 1 if this is your tree
      // 1 if this tree is dormant
      tree t;
      cin >> t.cell.value >> t.size >> mine >> t.dormant;
      cin.ignore();
      if (mine) {
        game.me.trees.emplace(t.cell, t);
      }
      else {
        game.opponent.trees.emplace(t.cell, t);
      }
    }
    int numberOfPossibleActions;  // all legal actions
    cin >> numberOfPossibleActions;
    cin.ignore();
    for (int i = 0; i < numberOfPossibleActions; i++) {
      string possibleAction;
      getline(
          cin,
          possibleAction);  // try printing something from here to start with
    }

    cache.compute(game);

    cout << decide(game) << endl;
  }
}