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

using id_t = int;
constexpr auto invalid_id = std::numeric_limits<id_t>::max();

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

template <class T, class... Ts>
using int_value =
    value_impl<T,
               int,
               st::comparable,
               st::arithmetic,
               st::comparable_with<int>,
               st::arithmetically_compatible_with<int, st::construct_t<T>>,
               Ts...>;

template <class T, class... Ts>
struct tagged_int : int_value<tagged_int<T, Ts...>, Ts...> {
  using base = int_value<tagged_int<T, Ts...>, Ts...>;
  template <class... Us>
  constexpr explicit tagged_int(Us... ts) : base{ts...} {}
};

template <class T>
using strong_id = value_impl<T, id_t, st::comparable>;

namespace detail {
template <class T>
using strength_impl = int_value<
    T,
    st::arithmetically_compatible_with<double,
                                       st::cast_to_then_construct_t<int, T>,
                                       st::get_value_then_cast_t<double>>>;
}  // namespace detail
struct strength : detail::strength_impl<strength> {
  template <class... Ts>
  constexpr explicit strength(Ts... ts)
      : detail::strength_impl<strength>{ts...} {}
};
using production_capacity = tagged_int<struct production_tag>;
using duration = tagged_int<struct duration_tag,
                            st::commutative_under<st::multiplies_t,
                                                  production_capacity,
                                                  st::construct_t<strength>>>;

struct factory_info {
  owner_type owner;
  strength cyborgs;
  production_capacity production;
};

struct factory_id : strong_id<factory_id> {
  constexpr explicit factory_id(id_t id) : value_impl{id} {}
};

struct troop_info {
  owner_type owner;
  factory_id origin;
  strength cyborgs;
  duration distance;
  factory_id target;
};

struct troop_id : strong_id<troop_id> {
  constexpr explicit troop_id(id_t id) noexcept : value_impl{id} {}
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