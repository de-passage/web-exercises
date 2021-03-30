#ifndef GUARD_DPSG_STRONG_TYPES_HPP
#define GUARD_DPSG_STRONG_TYPES_HPP

#include <utility>

namespace dpsg {
namespace strong_types {

#define DPSG_DEFINE_BINARY_OPERATOR(name, sym)                             \
  struct name##_t {                                                        \
    template <class T,                                                     \
              class U,                                                     \
              std::enable_if_t<!std::is_lvalue_reference_v<T>, int> = 0>   \
    constexpr inline decltype(auto) operator()(T&& left,                   \
                                               U&& right) const noexcept { \
      return std::forward<T>(left) sym std::forward<U>(right);             \
    }                                                                      \
    template <class T, class U>                                            \
    constexpr inline decltype(auto) operator()(T& left,                    \
                                               U&& right) const noexcept { \
      return left sym std::forward<U>(right);                              \
    }                                                                      \
  };

#define DPSG_DEFINE_UNARY_OPERATOR(name, sym)                          \
  struct name##_t {                                                    \
    template <class U>                                                 \
    constexpr inline decltype(auto) operator()(U&& u) const noexcept { \
      return sym std::forward<U>(u);                                   \
    }                                                                  \
  };

#define DPSG_APPLY_TO_BINARY_OPERATORS(f)                            \
  f(plus, +) f(minus, -) f(divides, /) f(multiplies, *) f(modulo, %) \
      f(equal, ==) f(not_equal, !=) f(lesser, <) f(greater, >)       \
          f(lesser_equal, <=) f(greater_equal, >=) f(binary_or, |)   \
              f(binary_and, &) f(binary_xor, ^) f(shift_right, <<)   \
                  f(shift_left, >>) f(boolean_or, ||) f(boolean_and, &&)

#define DPSG_APPLY_TO_SELF_ASSIGNING_BINARY_OPERATORS(f)                       \
  f(plus_assign, +=) f(minus_assign, -=) f(divides_assign, /=)                 \
      f(multiplies_assign, *=) f(modulo_assign, %=) f(shift_right_assign, <<=) \
          f(shift_left_assign, >>=) f(binary_and_assign, &=)                   \
              f(binary_or_assign, |=) f(binary_xor_assign, ^=)

#define DPSG_APPLY_TO_UNARY_OPERATORS(f)                           \
  f(boolean_not, !) f(binary_not, ~) f(negate, -) f(positivate, +) \
      f(dereference, *) f(address_of, &)

DPSG_APPLY_TO_BINARY_OPERATORS(DPSG_DEFINE_BINARY_OPERATOR)
DPSG_APPLY_TO_SELF_ASSIGNING_BINARY_OPERATORS(DPSG_DEFINE_BINARY_OPERATOR)
DPSG_APPLY_TO_UNARY_OPERATORS(DPSG_DEFINE_UNARY_OPERATOR)

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
template <class T>
using has_value = detail::has_value<T>;
template <class T>
constexpr bool has_value_v = has_value<T>::value;

struct get_value_t {
  template <class T, std::enable_if_t<has_value_v<T>, int> = 0>
  inline constexpr decltype(auto) operator()(T&& t) noexcept {
    return std::forward<T>(t).value;
  }
  template <class T, std::enable_if_t<!has_value_v<T>, int> = 0>
  inline constexpr decltype(auto) operator()(T&& t) noexcept {
    return std::forward<T>(t);
  }
  template <class T, std::enable_if_t<has_value_v<T>, int> = 0>
  inline constexpr auto& operator()(T& t) noexcept {
    return t.value;
  }
  template <class T, std::enable_if_t<!has_value_v<T>, int> = 0>
  inline constexpr auto& operator()(T& t) noexcept {
    return t;
  }
};

struct passthrough_t {
  template <class T>
  inline constexpr decltype(auto) operator()(T&& t) const noexcept {
    return std::forward<T>(t);
  }
};

template <class T>
struct construct_t {
  template <class... Ts>
  inline constexpr T operator()(Ts&&... ts) const noexcept {
    return T{std::forward<Ts>(ts)...};
  }
};

template <class Op,
          class Left,
          class Right,
          class Result = passthrough_t,
          class TransformLeft = get_value_t,
          class TransformRight = get_value_t>
struct binary_operation_implementation;

template <class Op,
          class Arg,
          class Result = passthrough_t,
          class Transform = get_value_t>
struct unary_operation_implementation;

#define DPSG_DEFINE_FRIEND_BINARY_OPERATOR_IMPLEMENTATION(op, sym)          \
  template <class Left,                                                     \
            class Right,                                                    \
            class Result,                                                   \
            class TransformLeft,                                            \
            class TransformRight>                                           \
  struct binary_operation_implementation<op##_t,                            \
                                         Left,                              \
                                         Right,                             \
                                         Result,                            \
                                         TransformLeft,                     \
                                         TransformRight> {                  \
    template <class T,                                                      \
              class U,                                                      \
              std::enable_if_t<                                             \
                  std::conjunction_v<std::is_same<std::decay_t<T>, Left>,   \
                                     std::is_same<std::decay_t<U>, Right>>, \
                  int> = 0>                                                 \
    friend constexpr decltype(auto) operator sym(T&& left, U&& right) {     \
      return Result{}(op##_t{}(TransformLeft{}(std::forward<T>(left)),      \
                               TransformRight{}(std::forward<U>(right))));  \
    }                                                                       \
  };

#define DPSG_DEFINE_FRIEND_SELF_ASSIGN_BINARY_OPERATOR_IMPLEMENTATION(op, sym) \
  template <class Left,                                                        \
            class Right,                                                       \
            class Result,                                                      \
            class TransformLeft,                                               \
            class TransformRight>                                              \
  struct binary_operation_implementation<op##_t,                               \
                                         Left,                                 \
                                         Right,                                \
                                         Result,                               \
                                         TransformLeft,                        \
                                         TransformRight> {                     \
    template <class T,                                                         \
              class U,                                                         \
              std::enable_if_t<                                                \
                  std::conjunction_v<std::is_same<std::decay_t<T>, Left>,      \
                                     std::is_same<std::decay_t<U>, Right>>,    \
                  int> = 0>                                                    \
    friend constexpr decltype(auto) operator sym(T& left, U&& right) {         \
      op##_t{}(TransformLeft{}(left),                                          \
               TransformRight{}(std::forward<U>(right)));                      \
      return left;                                                             \
    }                                                                          \
  };

#define DPSG_DEFINE_FRIEND_UNARY_OPERATOR_IMPLEMENTATION(op, sym)         \
  template <class Arg, class Result, class Transform>                     \
  struct unary_operation_implementation<op##_t, Arg, Result, Transform> { \
    template <class T,                                                    \
              std::enable_if_t<                                           \
                  std::conjunction_v<std::is_same<std::decay_t<T>, Arg>>, \
                  int> = 0>                                               \
    friend constexpr decltype(auto) operator sym(T&& arg) {               \
      return op##_t{}(Transform{}(std::forward<T>(arg)));                 \
    }                                                                     \
  };

DPSG_APPLY_TO_BINARY_OPERATORS(
    DPSG_DEFINE_FRIEND_BINARY_OPERATOR_IMPLEMENTATION)
DPSG_APPLY_TO_SELF_ASSIGNING_BINARY_OPERATORS(
    DPSG_DEFINE_FRIEND_SELF_ASSIGN_BINARY_OPERATOR_IMPLEMENTATION)
DPSG_APPLY_TO_UNARY_OPERATORS(DPSG_DEFINE_FRIEND_UNARY_OPERATOR_IMPLEMENTATION)

template <class Operation,
          class Arg,
          class Result,
          class Transform = get_value_t>
struct reflexive_operator_implementation
    : binary_operation_implementation<Operation,
                                      Arg,
                                      Arg,
                                      Result,
                                      Transform,
                                      Transform> {};

template <class Operation,
          class Left,
          class Right,
          class Return = passthrough_t,
          class TransformLeft = get_value_t,
          class TransformRight = get_value_t>
struct commutative_operator_implementation
    : binary_operation_implementation<Operation,
                                      Left,
                                      Right,
                                      Return,
                                      TransformLeft,
                                      TransformRight>,
      binary_operation_implementation<Operation,
                                      Right,
                                      Left,
                                      Return,
                                      TransformRight,
                                      TransformLeft> {};

template <class... Ts>
struct tuple;
namespace detail {
template <class T1, class T2>
struct concat_tuples;
template <class... T1s, class... T2s>
struct concat_tuples<tuple<T1s...>, tuple<T2s...>> {
  using type = tuple<T1s..., T2s...>;
};
template <class T1, class T2>
using concat_tuples_t = typename concat_tuples<T1, T2>::type;
}  // namespace detail

using comparison_operators = tuple<equal_t,
                                   not_equal_t,
                                   lesser_equal_t,
                                   greater_equal_t,
                                   lesser_t,
                                   greater_t>;

using unary_boolean_operators = tuple<boolean_not_t>;
using binary_boolean_operators = tuple<boolean_and_t, boolean_or_t>;
using boolean_operators =
    detail::concat_tuples_t<unary_boolean_operators, binary_boolean_operators>;

using unary_bitwise_operators = tuple<binary_not_t>;
using binary_bitwise_operators = tuple<binary_and_t,
                                       binary_or_t,
                                       binary_xor_t,
                                       shift_left_t,
                                       shift_right_t,
                                       shift_left_assign_t,
                                       shift_right_assign_t>;
using bitwise_operators =
    detail::concat_tuples_t<unary_boolean_operators, binary_boolean_operators>;

using unary_arithmetic_operators = tuple<negate_t, positivate_t>;
using binary_arithmetic_operators = tuple<plus_t,
                                          minus_t,
                                          multiplies_t,
                                          divides_t,
                                          modulo_t,
                                          plus_assign_t,
                                          minus_assign_t,
                                          multiplies_assign_t,
                                          divides_assign_t,
                                          modulo_assign_t>;
using arithmetic_operators =
    detail::concat_tuples_t<unary_arithmetic_operators,
                            binary_arithmetic_operators>;

template <class T, class U>
struct for_each;
template <class U, class... Ts>
struct for_each<tuple<Ts...>, U> : U::template type<Ts>... {};

namespace detail {
template <class Arg1, class Arg2, class R, class T1, class T2>
struct make_commutative_operator {
  template <class Op>
  using type = commutative_operator_implementation<Op, Arg1, Arg2, R, T1, T2>;
};
template <class Arg, class R>
struct make_reflexive_operator {
  template <class Op>
  using type = reflexive_operator_implementation<Op, Arg, R>;
};
template <class Arg>
struct make_unary_operator {
  template <class Op>
  using type = unary_operation_implementation<Op, Arg>;
};
}  // namespace detail

template <class Arg>
struct comparable
    : for_each<comparison_operators,
               detail::make_reflexive_operator<Arg, passthrough_t>> {};

template <class Arg>
struct arithmetic
    : for_each<binary_arithmetic_operators,
               detail::make_reflexive_operator<Arg, construct_t<Arg>>>,
      for_each<unary_arithmetic_operators, detail::make_unary_operator<Arg>> {};
}  // namespace strong_types
}  // namespace dpsg

#endif  // GUARD_DPSG_STRONG_TYPES_HPP