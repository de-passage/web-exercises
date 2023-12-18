#include <iostream>
#include <vector>
#include <numeric>

using namespace std;

enum class Ship { Invalid, Submarine, Destroyer, Cruiser, Battleship };

template<class T, class U>
using vector_ref = conditional_t<is_const_v<U>, typename T::const_reference, typename T::reference>;

template<class V, class R = typename V::value_type, class U = vector_ref<R, V>>
U at(V& v, int i) noexcept { return v[i/10][i%10]; }

int& at(int (&ships)[4], Ship s) noexcept { return ships[static_cast<int>(s) - 1]; }

int at_or_zero(const vector<vector<int>>& v, int i) { return (i >= 0 && i < 100) ? at(v, i) : 0; }

void set(vector<vector<bool>>& v, int j) { if (j >= 0 && j < 100) at(v, j) = true; }

enum { Up = -10, Down = 10, Left = -1, Right = 1 };

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0200r0.html
template<class Fun>
class y_combinator_result {
    Fun fun_;
public:
    template<class T>
    constexpr explicit y_combinator_result(T &&fun): fun_(std::forward<T>(fun)) {}

    template<class ...Args>
    constexpr decltype(auto) operator()(Args &&...args) const {
        return fun_(std::ref(*this), std::forward<Args>(args)...);
    }
};

template<class Fun>
constexpr decltype(auto) fix(Fun &&fun) {
    return y_combinator_result<std::decay_t<Fun>>(std::forward<Fun>(fun));
}

constexpr auto check_dir(int moveDir, int checkDir) {
  return [=](auto f, const vector<vector<int>>& v, int i, int acc) {
    if (acc > 4) return 0;
    if (!at_or_zero(v, i)) return acc;
    if (at_or_zero(v, i + checkDir - moveDir) || at_or_zero(v, i + checkDir) || at_or_zero(v, i + checkDir + moveDir)) return 0;
    return f(v, i + moveDir, acc + 1);
  };
}

constexpr auto fill_mask(int moveDir, int checkDir) {
  return [=](int count, int i, std::vector<std::vector<bool>>& mask) {
    for (int j = 0; j < count; ++j) {
      set(mask, i + (j * moveDir));
      set(mask, i + (j * moveDir) + checkDir);
      set(mask, i + (j * moveDir) + checkDir - moveDir);
      set(mask, i + (j * moveDir) + checkDir + moveDir);
    }
    set(mask, i + (count * moveDir));
    set(mask, i + (count * moveDir) - checkDir);
  };
}

constexpr auto check(int moveDir, int checkDir) {
  return 
  [ check_d = fix(check_dir(moveDir, checkDir))
  , fill_d = fill_mask(moveDir, checkDir)]
  (const auto& v, auto& mask, int i) {
    int result = check_d(v, i, 0);
    if (result) {
      fill_d(result, i, mask);
    }
    return result;
  };
}

constexpr static inline auto check_right = check(Right, Down);
constexpr static inline auto check_down = check(Down, Right);

Ship check_from(const vector<vector<int>>& v, int i, vector<vector<bool>>& mask) {
  return static_cast<Ship>(
    max(check_right(v, mask, i),
        check_down(v, mask, i)));
}

bool validate_battlefield(vector< vector<int> > field) {
  vector<vector<bool>> mask;
  for (int i = 0; i < 10; ++i) mask.emplace_back(10, false);
  int ships[4] = { 4, 3, 2, 1 };
  
  int i = -1;
  while(++i < 100) {
    if (at(mask, i)) continue; // ignore cells already considered
    if (at(field, i)) {
      Ship ship = check_from(field, i, mask);
      if (ship == Ship::Invalid || at(ships, ship) == 0) return false;
      --at(ships, ship);
    }
  }
  
  return accumulate(ships, ships + 4, 0) == 0;
}