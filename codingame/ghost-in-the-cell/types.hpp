#ifndef GUARD_DPSG_GITC_TYPES
#define GUARD_DPSG_GITC_TYPES

#include <functional>
#include <type_traits>
#include <unordered_map>
#include <utility>

#include "./strong_types.hpp"

namespace gitc {

namespace st = dpsg::strong_types;

enum class owner_type : int { me = 1, opponent = -1, neutral = 0 };
enum class entity_type : int { troop, factory };

template <class T, class ValueType = int>
struct value_impl : st::comparable<T>, st::arithmetic<T> {
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

struct strength : value_impl<strength, int> {
  constexpr strength() noexcept = default;
  constexpr strength(int value) noexcept : value_impl{value} {}
};

struct duration : value_impl<duration, int> {
  template <class... Ts>
  constexpr duration(Ts... ts) : value_impl<duration, int>{ts...} {}
};

struct factory_info {
  owner_type owner;
  strength cyborgs;
  strength production;
};

using id_t = int;
constexpr auto invalid_id = std::numeric_limits<id_t>::max();

struct factory_id : value_impl<factory_id, id_t> {
  constexpr explicit factory_id(id_t id) : value_impl{id} {}
};

struct weight : value_impl<weight, id_t> {
  constexpr weight() noexcept = default;
  constexpr weight(int d) : value_impl{d} {}
};

struct factory_distance {
  constexpr factory_distance(factory_id f, weight d) : target{f}, distance{d} {}
  factory_id target;
  weight distance;
};

struct troop_info {
  owner_type owner;
  factory_id origin;
  strength cyborgs;
  factory_distance distance;
};

struct troop_id : value_impl<troop_id, id_t> {
  constexpr troop_id(id_t id) noexcept : value_impl{id} {}
};
}  // namespace gitc

namespace std {
template <>
struct hash<::gitc::factory_id> {
  auto operator()(::gitc::factory_id id) const {
    return hash<::gitc::id_t>()(id.value);
  }
};
template <>
struct hash<::gitc::troop_id> {
  auto operator()(::gitc::troop_id id) const {
    return hash<::gitc::id_t>()(id.value);
  }
};
}  // namespace std

namespace gitc {

using factory_container = std::unordered_map<factory_id, factory_info>;
using factory_with_id = typename factory_container::value_type;
using troop_container = std::unordered_map<troop_id, troop_info>;
using troop_with_id = typename troop_container::value_type;

inline strength production(const factory_with_id& p) {
  return p.second.production;
}
template <class T, class U>
inline strength cyborgs(const std::pair<T, U>& p) {
  return p.second.cyborgs;
}
inline troop_id id(const troop_with_id& p) {
  return p.first;
}
inline factory_id id(const factory_with_id& p) {
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
inline factory_info info(const factory_with_id& p) {
  return p.second;
}
inline troop_info info(const troop_with_id& p) {
  return p.second;
}

template <
    class T,
    class U,
    std::enable_if_t<std::conjunction_v<st::has_value<T>, st::has_value<U>>,
                     int> = 0>
inline double operator/(T t, U u) {
  return static_cast<double>(t.value) / static_cast<double>(u.value);
}
}  // namespace gitc

#endif  // GUARD_DPSG_GITC_TYPES