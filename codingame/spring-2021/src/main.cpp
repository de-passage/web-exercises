#include <algorithm>
#include <iostream>
#include <queue>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

using namespace std;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

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
// constexpr int OPPOSITE_DIRECTION[6] = {3, 4, 5, 0, 1, 2};

struct cell {
  int richness;
  int neighbors[NEIGHBOR_COUNT]{-1};
};

struct tree {
  int cell;
  int size;
  bool mine;
  bool dormant;

  bool is_completeable() const { return size >= TREE_MAX; }
  bool is_actionable(const struct player& player) const;
};

const auto of_size = [](int size) {
  return [size](const auto& p) { return p.second.size == size; };
};

struct player {
  int score;
  int sun;

  using tree_container = unordered_map<int, tree>;
  tree_container trees{};

  bool can_complete_lifecycle() const { return sun >= 4; }

  template <class T, class F>
  auto tree_or(int cell, T&& with_tree, F&& without) const {
    auto it = trees.find(cell);
    if (it != trees.end()) {
      return FWD(with_tree)(*it);
    }
    return FWD(without)();
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

template <class Source>
struct directional_iterator {
 private:
  constexpr static int invalid_value = -1;

 public:
  using value_type =
      std::conditional_t<std::is_const_v<Source>, const cell, cell>;
  using pointer = value_type*;
  using reference = value_type&;
  using difference_type = ptrdiff_t;
  using iterator_category = std::forward_iterator_tag;

  constexpr directional_iterator() = default;
  constexpr explicit directional_iterator(const struct game& game,
                                          int value,
                                          int direction)
      : _game{&game},
        _current{value >= 0 && value < CELL_COUNT && direction >= 0 &&
                         direction < 6
                     ? value
                     : invalid_value},
        _direction{direction} {}

  reference operator*() const { return _game.cells[_current]; }
  pointer operator->() const { return _game.cells + _current; }

  directional_iterator& operator++() {
    if (_current == invalid_value || _game == nullptr)
      throw std::out_of_range("Invalid iterator dereferenced");
    _current = _game.cells[_current];
    return *this;
  }

 private:
  const struct game* _game{nullptr};
  int _current{invalid_value};
  int _direction{0};
};

struct game {
  vector<cell> cells{};
  int day;
  int nutrients;
  int score;
  player me;
  player opponent;
  bool opponent_waiting;

  cell& cell_at(int cell) {
    return cell >= 0 && cell < CELL_COUNT
               ? cells[static_cast<size_t>(cell)]
               : throw std::out_of_range("invalid cell id");
  }

  const cell& cell_at(int cell) const {
    return const_cast<game*>(this)->cell_at(cell);
  }

  int richness_of(const tree& t) const { return cell_at(t.cell).richness; }

  int richness_of(int cell) const { return cell_at(cell).richness; }

  template <class T, class F>
  auto tree_at(int cell, T&& with_tree, F&& without) const {
    return me.tree_or(cell, FWD(with_tree), [&] {
      opponent.tree_or(cell, forward<T>(with_tree), forward<F>(without));
    });
  }

  int sun_direction() const { return day % 6; }

  int distance(int left, int right) const {
    return _distance_lookup[left][right];
  }

  int move(int source, int direction) const {
    return direction >= 0 && direction < 6
               ? cell_at(source).neighbors[static_cast<size_t>(direction)]
               : throw std::out_of_range("invalid direction");
  }

  using iterator = directional_iterator<game>;
  using const_iterator = directional_iterator<const game>;
  iterator begin(int cell, int direction) {
    return iterator{*this, cell, direction};
  }
  const_iterator begin(int cell, int direction) const {
    return const_iterator{*this, cell, direction};
  }
  iterator end() { return iterator{}; }
  const_iterator end() const { return const_iterator{}; }

 private:
  int _distance_lookup[37][37]{{-1}};
  int _compute_distance(int start, int end) {
    queue<pair<int, int>> to_explore;
    unordered_set<int> explored;
    to_explore.push({start, 0});

    while (!to_explore.empty()) {
      auto c = to_explore.front();
      to_explore.pop();
      explored.insert(c.first);
      if (c.first == end)
        return c.second;

      for (int i = 0; i < NEIGHBOR_COUNT; ++i) {
        int n = move(c.first, i);
        if (explored.count(n))
          continue;
        to_explore.push({n, c.second + 1});
      }
    }
    throw runtime_error("didn't find a path between " + to_string(start) + " " +
                        to_string(end));
  }

 public:
  void initialize_lookup() {
    if (_distance_lookup[0][0] == 0)
      return;
    for (int i = 0; i < CELL_COUNT - 1; ++i) {
      _distance_lookup[i][i] = 0;
      for (int j = i + 1; j < CELL_COUNT; ++j) {
        _distance_lookup[i][j] = _distance_lookup[j][i] =
            _compute_distance(i, j);
      }
    }
  }
};

struct wait_t {
} constexpr wait{};
struct complete {
  constexpr explicit complete(const tree& t) : tree{t.cell} {}
  int tree;
};
struct grow {
  constexpr explicit grow(const tree& t) : cell{t.cell} {}
  int cell;
};
struct seed {
  constexpr explicit seed(int origin, int cell)
      : source{origin}, target{cell} {}
  int source;
  int target;
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

int avg_distance_of_trees(int cell, const player& player, const game& game) {
  if (player.trees.size() == 0) {
    return 0;
  }

  int acc = 0;
  for (const auto& tree : player.trees) {
    acc += game.distance(tree.second.cell, cell);
  }
  return acc / static_cast<int>(player.trees.size());
}

int can_plant(int cell, const player& player, const game& game) {
  for (const auto& tree : player.trees) {
    if (tree.second.cell != cell &&
        tree.second.size >= game.distance(tree.second.cell, cell) &&
        !tree.second.dormant) {
      return tree.second.cell;
    }
  }
  return -1;
}

seed best_spot_to_plant(const game& game) {
  if (game.me.can_plant()) {
    vector<tuple<int /*cell*/,
                 int /*richness*/,
                 int /*avg distance*/,
                 int /* source */>>
        possibilities;

    for (int i = 0; i < CELL_COUNT; ++i) {
      if (game.me.trees.count(i) == 0 &&
          game.opponent.trees.count(i) == 0) {  // no tree
        int source = can_plant(i, game.me, game);
        if (source != -1) {
          possibilities.push_back(
              make_tuple(i,
                         game.richness_of(i),
                         avg_distance_of_trees(i, game.me, game),
                         source));
        }
      }
    }

    if (!possibilities.empty()) {
      auto it = max_element(possibilities.begin(),
                            possibilities.end(),
                            [&](const auto& p1, const auto& p2) -> bool {
                              auto r1 = get<1>(p1);
                              auto r2 = get<1>(p2);

                              if (r1 == r2) {
                                return get<2>(p1) < get<2>(p2);
                              }
                              return r1 < r2;
                            });
      return seed{get<3>(*it), get<0>(*it)};
    }
  }
  return seed{-1, -1};
}

int empty_cells_of_value_3(const game& game) {
  int acc = 0;
  for (int i = 0; i < CELL_COUNT; ++i) {
    game.tree_at(
        i, [&acc](...) { acc++; }, [] {});
  }
  return acc;
}

action decide(const game& game) {
  if (game.day == DAY_MAX) {
    if (game.me.can_complete_lifecycle()) {
      vector<tree> v;
      transform(game.me.trees.begin(),
                game.me.trees.end(),
                back_inserter(v),
                get_second);
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
  else {
    auto b = best_spot_to_plant(game);

    // If possible, take the 3 point spots;
    if (b.source >= 0 && game.richness_of(b.target) == 3) {
      return b;
    }

    // Otherwise find the cheapest action between growing and seeding
    vector<tree> v;
    transform(game.me.trees.begin(),
              game.me.trees.end(),
              back_inserter(v),
              get_second);

    // only consider trees that we can act on
    auto last = remove_if(v.begin(), v.end(), [&](const tree& tree) {
      return !tree.is_actionable(game.me);
    });  // need to check that we'll be able to complete the tree

    // Grow 3 cost first then seeds then rest
    auto it = max_element(
        v.begin(), last, [&](const tree& left, const tree& right) -> bool {
          auto lr = game.richness_of(left);
          auto rr = game.richness_of(right);

          if (lr == rr) {
            return left.size && left.size < right.size;
          }
          return lr < rr;
        });

    if (it != last) {
      if (it->size != TREE_MAX) {
        return grow{*it};
      }
      return complete{*it};
    }

    if (b.source >= 0 && game.me.trees_of_size(0) < 1 &&
        empty_cells_of_value_3(game) < 2) {
      return b;
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
    int index;  // 0 is the center cell, the next cells spiral outwards
    cin >> index;
    cell& cell_ref = game.cell_at(index);

    // 0 if the cell is unusable, 1-3 for usable cells
    cin >> cell_ref.richness;

    for (int n = 0; n < NEIGHBOR_COUNT; ++n) {
      cin >> cell_ref.neighbors[n];
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
      cin >> t.cell >> t.size >> mine >> t.dormant;
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

    // GROW cellIdx | SEED sourceIdx targetIdx | COMPLETE cellIdx | WAIT
    // <message>
    cout << decide(game) << endl;
  }
}