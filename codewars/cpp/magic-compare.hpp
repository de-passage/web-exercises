#include <utility>
#include <type_traits>
#include <ostream>

template<class T>
class MagicCompare {
    // technically nothing prevents the user to use a custom type for T that may throw at 
    // unexpected times
    inline static constexpr bool t_move_ne = std::is_nothrow_move_constructible_v<T>;
    inline static constexpr bool t_defctor_ne = std::is_nothrow_default_constructible_v<T>;
    inline static constexpr bool t_eq_ne = noexcept(std::declval<T>() == std::declval<T>());
    inline static constexpr bool t_neq_ne = noexcept(std::declval<T>() != std::declval<T>());
    inline static constexpr bool t_lt_ne = noexcept(std::declval<T>() < std::declval<T>());
    inline static constexpr bool t_gt_ne = noexcept(std::declval<T>() > std::declval<T>());
    inline static constexpr bool t_lte_ne = noexcept(std::declval<T>() <= std::declval<T>());
    inline static constexpr bool t_gte_ne = noexcept(std::declval<T>() >= std::declval<T>());
    inline static constexpr bool t_mult_ne = noexcept(std::declval<T>() * std::declval<T>());
    inline static constexpr bool t_plusassign_ne = noexcept(std::declval<T&>() += std::declval<T>());
    inline static constexpr bool t_preinc_ne = noexcept(++std::declval<T&>());
    inline static constexpr bool ctor_ne = std::is_nothrow_constructible_v<MagicCompare, T, bool>;
    inline static constexpr bool pub_ctor_ne = std::is_nothrow_constructible_v<MagicCompare, T>;
    
  public:
    constexpr MagicCompare() noexcept(t_defctor_ne) : value(), acc(true) {}
    constexpr MagicCompare(T val) noexcept(t_move_ne) : value(std::move(val)), acc(true) {}
    
    constexpr friend MagicCompare<T> operator==(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_eq_ne) {
      const bool a = left.acc && right.acc && left.value == right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator!=(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_neq_ne) {
      const bool a = left.acc && right.acc && left.value != right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator<(const T& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_lt_ne) {
      const bool a = right.acc && left < right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator<(const MagicCompare<T>& left, const T& right) noexcept(ctor_ne && t_lt_ne) {
      const bool a = left.acc && left.value < right;
      return { right, a };
    }
    
    constexpr friend MagicCompare<T> operator<(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_lt_ne) {
      const bool a = right.acc && left.acc && left.value < right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator>(const T& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_gt_ne) {
      const bool a = right.acc && left > right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator>(const MagicCompare<T>& left, const T& right) noexcept(ctor_ne && t_gt_ne) {
      const bool a = left.acc && left.value > right;
      return { right, a };
    }
    
    constexpr friend MagicCompare<T> operator>(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_gt_ne) {
      const bool a = left.acc && right.acc && left.value > right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator<=(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_lte_ne) {
      const bool a = left.acc && right.acc && left.value <= right.value;
      return { right.value, a };
    }
    
    constexpr friend MagicCompare<T> operator>=(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(ctor_ne && t_gte_ne) {
      const bool a = left.acc && right.acc && left.value >= right.value;
      return { right.value, a };
    }
    
    template<class C, class R>
    constexpr friend std::basic_ostream<C, R>& operator<<(std::basic_ostream<C, R>& out, const MagicCompare<T>& right) {
      return out << right.value;
    }
    
    constexpr MagicCompare<T>& operator+=(const MagicCompare<T>& right) noexcept(t_plusassign_ne) {
      value += right.value;
      return *this;
    }
    
    constexpr MagicCompare<T>& operator++() noexcept(t_preinc_ne) {
      ++value;
      return *this;
    }
    
    constexpr friend MagicCompare<T> operator*(const T& left, const MagicCompare<T>& right) noexcept(t_mult_ne && pub_ctor_ne) {
      return { left * right.value };
    }
    
    constexpr friend MagicCompare<T> operator*(const MagicCompare<T>& left, const MagicCompare<T>& right) noexcept(t_mult_ne && pub_ctor_ne) {
      return { left.value * right.value };
    }
    
    constexpr operator bool() const noexcept { return acc; }
    constexpr operator const T&() const noexcept { return value; }
  
  private:
    constexpr MagicCompare(T val, bool a) noexcept(t_move_ne) : value(std::move(val)), acc(a) {}
    
    T value;
    bool acc;
};