#ifndef GUARD_DPSG_GITC_TYPES
#define GUARD_DPSG_GITC_TYPES

#include <functional>
#include <type_traits>
#include <unordered_map>
#include <utility>

#include "strong_types.hpp"

namespace gitc {

namespace st = dpsg::strong_types;

enum class owner_type : int { me = 1, opponent = -1, neutral = 0 };
enum class entity_type : int { troop, factory };

using id_t = int;

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

template <class T, class Ty, class... Ts>
using strong_pod =
    value_impl<T,
               Ty,
               st::comparable,
               st::arithmetic,
               st::comparable_with<Ty>,
               st::arithmetically_compatible_with<Ty, st::construct_t<T>>,
               Ts...>;

template <class T, class Ty, class... Ts>
struct tagged_value : strong_pod<tagged_value<T, Ty, Ts...>, Ty, Ts...> {
  using base = strong_pod<tagged_value<T, Ty, Ts...>, Ty, Ts...>;
  template <class... Us>
  constexpr explicit tagged_value(Us... ts) : base{ts...} {}
};
template <class T, class... Ts>
using int_value = tagged_value<T, int, Ts...>;
template <class T, class... Ts>
using double_value = tagged_value<T, double, Ts...>;

template <class Tag>
struct strong_id : value_impl<strong_id<Tag>, id_t, st::comparable> {
  constexpr explicit strong_id(id_t id)
      : value_impl<strong_id<Tag>, id_t, st::comparable>{id} {}
};

namespace detail {
template <class T>
using strength_impl = strong_pod<
    T,
    int,
    st::arithmetically_compatible_with<double,
                                       st::cast_to_then_construct_t<int, T>,
                                       st::get_value_then_cast_t<double>>>;
}  // namespace detail
struct strength : detail::strength_impl<strength> {
  template <class... Ts>
  constexpr explicit strength(Ts... ts)
      : detail::strength_impl<strength>{ts...} {}
};
using production_capacity = int_value<struct production_tag>;
using duration = int_value<struct duration_tag,
                           st::commutative_under<st::multiplies_t,
                                                 production_capacity,
                                                 st::construct_t<strength>>>;

using strategic_value = double_value<struct strategic_value_tag>;

struct factory_info {
  owner_type owner;
  strength cyborgs;
  production_capacity production;
  duration inactivity;
};
using factory_id = strong_id<factory_info>;

struct troop_info {
  owner_type owner;
  factory_id origin;
  strength cyborgs;
  duration distance;
  factory_id target;
};
using troop_id = strong_id<troop_info>;

struct bomb_info {
  owner_type owner;
  factory_id origin;
  factory_id target;
  duration distance;
};
using bomb_id = strong_id<bomb_info>;
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
template <>
struct hash<::gitc::bomb_id> {
  auto operator()(::gitc::bomb_id id) const {
    return hash<::gitc::id_t>()(id.value);
  }
};
}  // namespace std

namespace gitc {

template <class T>
using entity_container = std::unordered_map<strong_id<T>, T>;
template <class T>
using with_id = typename entity_container<T>::value_type;

using factory_container = entity_container<factory_info>;
using factory_with_id = with_id<factory_info>;
using troop_container = entity_container<troop_info>;
using troop_with_id = with_id<troop_info>;
using bomb_container = entity_container<bomb_info>;
using bomb_with_id = with_id<bomb_info>;

inline production_capacity production(const factory_with_id& p) {
  return p.second.production;
}
inline production_capacity production(const factory_info& p) {
  return p.production;
}
template <class T, class U>
inline strength cyborgs(const std::pair<T, U>& p) {
  return p.second.cyborgs;
}
inline strength cyborgs(const factory_info& i) {
  return i.cyborgs;
}
inline duration inactivity(const factory_with_id& i) {
  return i.second.inactivity;
}
inline duration inactivity(const factory_info& i) {
  return i.inactivity;
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
inline owner_type owner(const troop_info& i) {
  return i.owner;
}
inline factory_info info(const factory_with_id& p) {
  return p.second;
}
inline troop_info info(const troop_with_id& p) {
  return p.second;
}
inline duration time_to_arrival(const troop_with_id& p) {
  return p.second.distance;
}
inline duration time_to_arrival(const troop_info& p) {
  return p.distance;
}
inline factory_id target(const troop_with_id& p) {
  return p.second.target;
}
inline factory_id target(const troop_info& p) {
  return p.target;
}
inline factory_id origin(const troop_with_id& p) {
  return p.second.origin;
}
inline factory_id origin(const troop_info& p) {
  return p.origin;
}
inline factory_id target(const bomb_with_id& p) {
  return p.second.target;
}
inline factory_id target(const bomb_info& p) {
  return p.target;
}
inline factory_id origin(const bomb_with_id& p) {
  return p.second.origin;
}
inline factory_id origin(const bomb_info& p) {
  return p.origin;
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