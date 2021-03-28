#ifndef GUARD_DPSG_GITC_TYPES
#define GUARD_DPSG_GITC_TYPES

#include <unordered_map>
#include <utility>

namespace gitc {

enum class owner_type : int { me = 1, opponent = -1, neutral = 0 };
enum class entity_type : int { troop, factory };

struct strength {
  constexpr strength() noexcept = default;
  constexpr strength(int value) noexcept : value{value} {}
  int value{};

  friend bool operator<(strength left, strength right) noexcept {
    return left.value < right.value;
  }
};

struct factory_info {
  owner_type owner;
  strength cyborgs;
  strength production;
};

struct factory {
  constexpr explicit factory(size_t id) : _id{id} {}

  size_t id() const { return _id; }
  friend bool operator==(factory left, factory right) noexcept {
    return left._id == right._id;
  }

 private:
  size_t _id;
};

struct weight {
  constexpr weight() noexcept = default;
  constexpr weight(int d) : value{d} {}
  int value{};

  friend bool operator==(weight left, weight right) {
    return left.value == right.value;
  }

  friend bool operator!=(weight left, weight right) {
    return left.value != right.value;
  }
};

struct factory_distance {
  constexpr factory_distance(factory f, weight d) : target{f}, distance{d} {}
  factory target;
  weight distance;
};

struct troop_info {
  owner_type owner;
  factory origin;
  strength cyborgs;
  factory_distance distance;
};

struct entity_id {
  int id;
  friend bool operator==(entity_id left, entity_id right) noexcept {
    return left.id == right.id;
  }
  friend bool operator<(entity_id left, entity_id right) noexcept {
    return left.id < right.id;
  }
};

inline factory to_factory(entity_id id) {
  return factory{static_cast<size_t>(id.id)};
}
inline entity_id to_id(factory id) {
  return entity_id{static_cast<int>(id.id())};
}

using factory_container = std::unordered_map<entity_id, factory_info>;
using troop_container = std::unordered_map<entity_id, troop_info>;
template <class T>
inline strength production(const std::pair<T, factory_info>& p) {
  return p.second.production;
}
template <class T, class U>
inline strength cyborgs(const std::pair<T, U>& p) {
  return p.second.cyborgs;
}
template <class T, class U>
inline entity_id id(const std::pair<T, U>& p) {
  return p.first;
}
template <class T, class U>
inline owner_type owner(const std::pair<T, U>& p) {
  return p.second.owner;
}
inline owner_type owner(const factory_info& i) {
  return i.owner;
}
inline strength cyborgs(const factory_info& i) {
  return i.cyborgs;
}

namespace detail {
template <class...>
struct void_t_impl {
  using type = void;
};
template <class... Ts>
using void_t = typename void_t_impl<Ts...>::type;
template <class T, class = void>
struct has_value : std::false_type {};
template <class T>
struct has_value<T, void_t<decltype(std::declval<T>().value)>>
    : std::true_type {};
}  // namespace detail
template <class T,
          class U,
          std::enable_if_t<
              std::conjunction_v<detail::has_value<T>, detail::has_value<U>>,
              int> = 0>
inline double operator/(T t, U u) {
  return static_cast<double>(t.value) / static_cast<double>(u.value);
}
}  // namespace gitc

namespace std {
template <>
struct hash<::gitc::factory> {
  auto operator()(::gitc::factory f) const { return hash<size_t>()(f.id()); }
};
template <>
struct hash<::gitc::entity_id> {
  auto operator()(::gitc::entity_id id) const { return hash<int>()(id.id); }
};
}  // namespace std

#endif  // GUARD_DPSG_GITC_TYPES