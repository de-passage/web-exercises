#ifndef GUARD_DPSG_GITC_TYPES
#define GUARD_DPSG_GITC_TYPES

#include <unordered_map>
#include <utility>

namespace gitc {

enum class owner_type : int { Vme = 1, opponent = -1, neutral = 0 };
enum class entity_type : int { troop, factory };

struct strength {
  constexpr strength() noexcept = default;
  constexpr strength(int value) noexcept : value{value} {}
  int value{};
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
};

using factory_container = std::unordered_map<entity_id, factory_info>;
using troop_container = std::unordered_map<entity_id, troop_info>;
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