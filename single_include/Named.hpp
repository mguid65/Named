/**
 * @brief Single header include version of library
 * @author Matthew Guidry (github: mguid65)
 * @date 2025-05-01
 *
 * @cond IGNORE_LICENSE
 *
 * MIT License
 *
 * Copyright (c) 2024 Matthew Guidry
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * @endcond
 */

#ifndef MGUID_SINGLE_INCLUDE_NAMED_NAMED_HPP
#define MGUID_SINGLE_INCLUDE_NAMED_NAMED_HPP

#include <algorithm>
#include <array>
#include <bitset>
#include <compare>
#include <concepts>
#include <cstdint>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <type_traits>

namespace mguid {

/**
 * @brief Recreation of the exposition only function synth-three-way
 * @tparam T Lhs type
 * @tparam U Rhs type
 * @param t lhs
 * @param u rhs
 */
constexpr auto SynthThreeWay = []<class T, class U>(const T& t, const U& u)
  requires requires {
    { t < u } -> std::convertible_to<bool>;
    { u < t } -> std::convertible_to<bool>;
  }
{
  if constexpr (std::three_way_comparable_with<T, U>) {
    return t <=> u;
  } else {
    if (t < u) return std::weak_ordering::less;
    if (u < t) return std::weak_ordering::greater;
    return std::weak_ordering::equivalent;
  }
};

/**
 * @brief Helper type to get the result type of SynthThreeWay
 * @tparam T Lhs type
 * @tparam U Rhs type
 */
template <class T, class U = T>
using SynthThreeWayResultT = decltype(SynthThreeWay(std::declval<T&>(), std::declval<U&>()));

}  // namespace mguid

namespace mguid {

/**
 * @brief Find the index of a value in a pack of non-types
 * @tparam Needle value to search for
 * @tparam Haystack pack of non-types
 * @return the index equivalent of the location of the type within the pack
 */
template <auto Needle, auto... Haystack>
constexpr std::size_t index_in_pack() {
  std::size_t index{0};
  ([&index]<auto Type>() {
    if constexpr (Needle == Type) {
      return false;
    } else {
      ++index;
      return true;
    }
  }.template operator()<Haystack>() &&
   ...);
  if (index >= sizeof...(Haystack)) { throw std::out_of_range("Value does not exist in pack"); }
  return index;
}

/**
 * @brief Find the reverse index of a value in a pack of non-types
 * @tparam Needle value to search for
 * @tparam Haystack pack of non-types
 * @return the index equivalent of the location of the type within the pack
 */
template <auto Needle, auto... Haystack>
constexpr std::size_t reverse_index_in_pack() {
  std::size_t index{0};
  ([&index]<auto Type>() {
    if constexpr (Needle == Type) {
      return false;
    } else {
      ++index;
      return true;
    }
  }.template operator()<Haystack>() &&
   ...);
  if (index >= sizeof...(Haystack)) { throw std::out_of_range("Value does not exist in pack"); }
  return sizeof...(Haystack) - 1 - index;
}

/**
 * @brief Determine if all values within a pack of non-types are unique
 * @tparam Nttps pack of non-types
 * @return true if all values in the pack are unique; otherwise false
 */
template <auto... Nttps>
consteval bool all_unique_nttps() {
  if constexpr (sizeof...(Nttps) == 0) {
    return true;
  } else {
    bool seen[sizeof...(Nttps)] = {false};

    ([&seen]<auto Value>() { seen[index_in_pack<Value, Nttps...>()] = true; }.template operator()<Nttps>(), ...);

    return std::all_of(std::begin(seen), std::end(seen), [](bool val) { return val; });
  }
}

}  // namespace mguid

namespace mguid {

/**
 * @brief Compile time string literal container
 * @tparam NSize size of string literal
 */
template <size_t NSize>
struct StringLiteral {
  // NOLINTBEGIN(google-explicit-constructor)
  constexpr StringLiteral() noexcept = default;

  /**
   * @brief Construct a StringLiteral from a string literal
   * @param str a string literal as a const reference to a sized char array
   */
  constexpr explicit(false) StringLiteral(char const (&str)[NSize]) : value{'\0'} { std::copy_n(str, NSize, value); }
  // NOLINTEND(google-explicit-constructor)

  /**
   * @brief Equality compare against another StringLiteral
   * Since this type is only templated on the size,
   * we need the equality operator to determine sameness for lookup
   *
   * @tparam MSize size of other string literal
   * @param other StringLiteral to compare with
   * @return true if equal; otherwise false
   */
  template <size_t MSize>
  [[nodiscard]] constexpr bool operator==(StringLiteral<MSize> other) const {
    constexpr auto equal = [](const auto& lhs, const auto& rhs) {
      for (size_t i{0}; i < NSize; i++) {
        if (lhs.value[i] != rhs.value[i]) { return false; }
      }
      return true;
    };
    return MSize == NSize && equal(*this, other);
  }

  /**
   * @brief Equality compare against a string_view
   *
   * subtract one because the string_view doesn't account for the
   * null-terminator
   *
   * @param sv string_view to compare with
   * @return true if equal; otherwise false
   */
  [[nodiscard]] constexpr bool operator==(const std::string_view sv) const {
    return sv == std::string_view{value, size - 1};
  }

  /**
   * @brief Convert this StringLiteral to a string_view
   * @return a string_view of the data in this StringLiteral
   */
  [[nodiscard]] constexpr explicit operator std::string_view() const { return std::string_view{value, size - 1}; }

  /**
   * @brief Convert this StringLiteral to a string_view
   * @return a string_view of the data in this StringLiteral
   */
  [[nodiscard]] constexpr std::string_view view() const { return std::string_view{value, size - 1}; }

  char value[NSize];
  static constexpr size_t size{NSize};
};

template <StringLiteral... Tags>
consteval auto concat_literals() {
  constexpr size_t total_chars = 0 + ((Tags.size - 1) + ...);
  constexpr size_t BufSize = total_chars + 1;

  StringLiteral<BufSize> result;

  size_t offset = 0;
  (([&offset, &result] {
     for (size_t i = 0; i + 1 < Tags.size; ++i) { result.value[offset + i] = Tags.value[i]; }
     offset += Tags.size - 1;
   }()),
   ...);

  result.value[BufSize - 1] = '\0';
  return result;
}

template <StringLiteral Delim, StringLiteral... Tags>
consteval auto join_with_delimiter() {
  constexpr size_t Count = sizeof...(Tags);
  constexpr size_t chars_ss = 0 + ((Tags.size - 1) + ...);
  constexpr size_t delim_chars = (Count > 1 ? (Count - 1) * (Delim.size - 1) : 0);
  constexpr size_t BufSize = chars_ss + delim_chars + 1;

  StringLiteral<BufSize> result;
  size_t offset = 0;
  size_t idx = 0;
  (
      [&offset, &idx, &result] {
        for (size_t j = 0; j + 1 < Tags.size; ++j) { result.value[offset + j] = Tags.value[j]; }
        offset += Tags.size - 1;

        if (++idx < Count) {
          for (size_t j = 0; j + 1 < Delim.size; ++j) { result.value[offset + j] = Delim.value[j]; }
          offset += Delim.size - 1;
        }
      }(),
      ...);

  result.value[BufSize - 1] = '\0';
  return result;
}

/**
 * @brief Constrains that a pack of non-types contains all unique values
 * @tparam Nttps pack of non-types
 */
template <auto... Nttps>
concept AllUniqueTags = all_unique_nttps<Nttps...>();

/**
 * @brief Check if Key is one of the tags within the StringLiteral pack
 * @tparam Key key to search for
 * @tparam Tags pack of StringLiteral to search in
 * @return true if Key exists in StringLiteral; otherwise false
 */
template <StringLiteral Key, StringLiteral... Tags>
constexpr bool is_one_of() {
  return (... || (Key == Tags));
}

/**
 * @brief Wrap a constant into a type
 * @tparam Value value to turn into type
 */
template <auto Value>
struct ConstantWrapper {
  static constexpr auto value = Value;
};

namespace literals {

/**
 * @brief UDL to create a StringLiteralHolder from a char string literal
 * @tparam Tag value make into StringLiteralHolder
 * @return StringLiteralHolder with Tag as its value
 */
template <StringLiteral Tag>
constexpr auto operator""_t() {
  return ConstantWrapper<Tag>{};
}

/**
 * @brief UDL to create a StringLiteral from a char string literal
 * @tparam Tag String literal to return
 * @return Tag
 */
template <StringLiteral Tag>
constexpr auto operator""_sl() {
  return Tag;
}

}  // namespace literals

/**
 * @brief Ostream insertion operator overload for StringLiteral
 * @param os reference to some ostream
 * @param lit a StringLiteral
 * @return reference to os
 */
template <size_t NSize>
std::ostream& operator<<(std::ostream& os, const StringLiteral<NSize>& lit) {
  return os << lit.view();
}

}  // namespace mguid

namespace mguid {
/**
 * @brief A helper type to associate a StringLiteral tag with a type for use with NamedTuple
 * @tparam Tag the "name" associated with the type
 * @tparam Unused the type to forward to the tuple base in NamedTuple
 */
template <StringLiteral Tag, typename Unused>
struct NamedType {
  /**
   * @brief Equality compare against a StringLiteral
   * we need the equality operator to determine sameness for lookup
   *
   * Sameness in this case is only used to determine uniqueness where uniqueness is only determined
   * by differing keys.
   *
   * @tparam MSize size of other string literal
   * @param other StringLiteral to compare with
   * @return true if equal; otherwise false
   */
  template <std::size_t MSize>
  constexpr bool operator==(StringLiteral<MSize> other) const {
    constexpr auto equal = [](const auto& lhs, const auto& rhs) {
      for (std::size_t i{0}; i < Tag.size; i++) {
        if (lhs.value[i] != rhs.value[i]) { return false; }
      }
      return true;
    };
    return MSize == Tag.size && equal(Tag, other);
  }

  /**
   * @brief Equality compare this with another NamedType
   * @tparam OtherTag Tag of other NamedType
   * @tparam OtherType Type of other NamedType
   * @return true if both have the same tag; otherwise false
   */
  template <StringLiteral OtherTag, typename OtherType>
  constexpr bool operator==(const NamedType<OtherTag, OtherType>&) const {
    return Tag == OtherTag;
  }

  /**
   * @brief Equality compare this with a string_view
   * @param sv string_view to compare with
   * @return true if the string_view is equal to this instances Tag; otherwise false
   */
  constexpr bool operator==(std::string_view sv) const { return Tag == sv; }

  /**
   * @brief Get the tag passed to this NamedType
   * @return the tag passed to this NamedType
   */
  static constexpr auto tag() { return Tag; }
};

/**
 * @brief A type that associates a NamedType with a value
 * @tparam Tag StringLiteral element name
 * @tparam ValueType type of value this will hold
 */
template <StringLiteral Tag, typename ValueType>
struct NamedTypeValueHelper {
  using DecayT = NamedType<Tag, std::unwrap_ref_decay_t<ValueType>>;
  using TypeRRef = NamedType<Tag, ValueType&&>;
  static constexpr auto tag = Tag;
  std::unwrap_ref_decay_t<ValueType> value{};
};

template <typename>
struct IsNamedTypeValueHelperImpl : std::false_type {};

template <StringLiteral Tag, typename ValueType>
struct IsNamedTypeValueHelperImpl<NamedTypeValueHelper<Tag, ValueType>> : std::true_type {};

template <typename T>
concept IsNamedTypeValueHelper = IsNamedTypeValueHelperImpl<T>::value;

/**
 * @brief A helper to associate a value with a named type for use in make_tuple
 * @tparam Tag StringLiteral element name
 * @tparam ValueType type of value
 * @param value value to associate with NamedType
 * @return a NamedTypeValueHelper object with the given value and Tag
 */
template <StringLiteral Tag, typename ValueType>
constexpr NamedTypeValueHelper<Tag, ValueType> NamedTypeV(ValueType value) {
  return NamedTypeValueHelper<Tag, ValueType>{value};
}

// Deduce the tag and type from a helper
template <typename Helper>
struct NamedTypeFromHelper;

template <StringLiteral Tag, typename T>
struct NamedTypeFromHelper<NamedTypeValueHelper<Tag, T>> {
  using type = NamedType<Tag, std::unwrap_ref_decay_t<T>>;
};

// Convenience alias
template <typename Helper>
using NamedTypeFromHelperT = typename NamedTypeFromHelper<std::remove_cvref_t<Helper>>::type;

namespace literals {

template <StringLiteral Tag>
struct NamedTypeValueUDLHelper {
  template <typename ValueType>
  constexpr NamedTypeValueHelper<Tag, ValueType> operator=(ValueType value) const {
    return NamedTypeValueHelper<Tag, ValueType>{value};
  }
};

template <StringLiteral Tag>
constexpr NamedTypeValueUDLHelper<Tag> operator""_nt() {
  return NamedTypeValueUDLHelper<Tag>{};
}

template <StringLiteral Tag>
constexpr NamedTypeValueUDLHelper<Tag> operator""_tag() {
  return NamedTypeValueUDLHelper<Tag>{};
}

template <StringLiteral Tag>
constexpr NamedTypeValueUDLHelper<Tag> operator""_name() {
  return NamedTypeValueUDLHelper<Tag>{};
}
}  // namespace literals

/**
 * @brief Base template of helper template to extract the type from a NamedType
 * @tparam Type unconstrained type
 */
template <typename Type>
struct ExtractType;

/**
 * @brief Partially specialized helper template
 * @tparam Key key part of NamedType
 * @tparam Type type part of NamedType
 */
template <StringLiteral Key, typename Type>
struct ExtractType<NamedType<Key, Type>> {
  using type = Type;
};

/**
 * @brief Concept that constrains uniqueness on a pack of NamedTypes
 * @tparam NamedTypes pack of NamedTypes
 */
template <typename... NamedTypes>
concept AllUniqueNamedTypes = all_unique_nttps<NamedTypes{}...>();

/**
 * @brief Extract the NamedTypes from a NamedTuple<Ts...>
 */
template <typename>
struct NamedTypesFromTuple;

/**
 * @brief Extract the NamedTypes from a NamedTuple<Ts...>
 * @tparam TupleT A tuple type to extract NamedTypes from
 * @tparam NamedTypes The list of NamedTypes in the NamedTuple
 */
template <template <typename...> typename TupleT, typename... NamedTypes>
struct NamedTypesFromTuple<TupleT<NamedTypes...>> {
  using type = std::tuple<NamedTypes...>;
};

/**
 * @brief Check if Key is one of the tags of a NamedType within the NamedTypes pack
 * @tparam Key key to search for
 * @tparam NamedTypes pack of NamedType to search in
 * @return true if Key exists as the tag of a NamedType in NamedTypes; otherwise false
 */
template <StringLiteral Key, NamedType... NamedTypes>
constexpr bool is_one_of() {
  return (... || (Key == NamedTypes));
}

template <std::size_t MaxLen = 64>
consteval std::size_t levenshtein_distance(char const* s1, std::size_t len1, char const* s2, std::size_t len2) {
  std::array<std::size_t, MaxLen + 1> prev{}, curr{};

  for (std::size_t j = 0; j <= len2; ++j) prev[j] = j;
  for (std::size_t i = 1; i <= len1; ++i) {
    curr[0] = i;
    for (std::size_t j = 1; j <= len2; ++j) {
      std::size_t insertCost = curr[j - 1] + 1;
      std::size_t deleteCost = prev[j] + 1;
      std::size_t replaceCost = prev[j - 1] + (s1[i - 1] == s2[j - 1] ? 0 : 1);
      curr[j] = std::min({insertCost, deleteCost, replaceCost});
    }
    prev = curr;
  }
  return prev[len2];
}

// nearest_index: compute distances to each Haystack, then pick the smallest.
template <StringLiteral Needle, StringLiteral... Haystack>
consteval std::size_t nearest_index() {
  constexpr std::size_t count = sizeof...(Haystack);

  constexpr std::array<std::size_t, count> distances = {
      {levenshtein_distance(Needle.value, Needle.size, Haystack.value, Haystack.size)...}};

  std::size_t best = 0;
  std::size_t bestDist = distances[0];
  for (std::size_t i = 1; i < count; ++i) {
    if (distances[i] < bestDist) {
      bestDist = distances[i];
      best = i;
    }
  }
  return best;
}

template <StringLiteral Key, typename... NamedTypes>
consteval auto diagnose_key() {
  constexpr StringLiteral closest = []<size_t Idx, StringLiteral... Tags>() constexpr {
    constexpr std::tuple<decltype(Tags)...> tags_tp{Tags...};
    return std::get<Idx>(tags_tp);
  }.template operator()<nearest_index<Key, NamedTypes::tag()...>(), NamedTypes::tag()...>();
  return concat_literals<"`", Key, "` was not found in [",
                         join_with_delimiter<", ", concat_literals<"`", NamedTypes::tag(), "`">()...>(),
                         "], Did you mean `", closest, "`?">();
}

template <auto>
struct Msg {
  static constexpr auto value = false;
};

template <class Hint>
concept Diagnosis = Hint::value;

template <StringLiteral Key, typename... NamedTypes>
concept MissingKey = Diagnosis<Msg<diagnose_key<Key, NamedTypes...>()>>;

template <StringLiteral Key, typename... NamedTypes>
concept CheckKeys = is_one_of<Key, NamedTypes{}...>() || MissingKey<Key, NamedTypes...>;

template <typename... LhsNamedTypes>
struct NamedTypesEqualityComparable {
  template <typename... RhsNamedTypes>
  static consteval bool with() {
    if constexpr (sizeof...(LhsNamedTypes) != sizeof...(RhsNamedTypes)) {
      return false;
    } else {
      return ([]<typename LhsNamed>() {
        if constexpr (constexpr auto Tag = LhsNamed{}.tag(); !mguid::is_one_of<Tag, RhsNamedTypes{}...>()) {
          return false;
        } else {
          constexpr std::size_t RhsIndex = mguid::index_in_pack<Tag, RhsNamedTypes{}...>();
          using RhsNamed = std::tuple_element_t<RhsIndex, std::tuple<RhsNamedTypes...>>;

          using LhsT = typename mguid::ExtractType<LhsNamed>::type;
          using RhsT = typename mguid::ExtractType<RhsNamed>::type;

          return requires(const LhsT& a, const RhsT& b) { a == b; };
        }
      }.template operator()<LhsNamedTypes>() &&
              ...);
    }
  }
};

template <typename... LhsNamedTypes>
struct NamedTypesThreeWayComparable {
  template <typename... RhsNamedTypes>
  static consteval bool with() {
    if constexpr (sizeof...(LhsNamedTypes) != sizeof...(RhsNamedTypes)) {
      return false;
    } else {
      return ([]<typename LhsNamed>() {
        if constexpr (constexpr auto Tag = LhsNamed{}.tag(); !mguid::is_one_of<Tag, RhsNamedTypes{}...>()) {
          return false;
        } else {
          constexpr std::size_t RhsIndex = mguid::index_in_pack<Tag, RhsNamedTypes{}...>();
          using RhsNamed = std::tuple_element_t<RhsIndex, std::tuple<RhsNamedTypes...>>;

          using LhsT = typename mguid::ExtractType<LhsNamed>::type;
          using RhsT = typename mguid::ExtractType<RhsNamed>::type;

          return requires(const LhsT& a, const RhsT& b) { a <=> b; };
        }
      }.template operator()<LhsNamedTypes>() &&
              ...);
    }
  }
};

}  // namespace mguid

namespace mguid {

/**
 * @brief A tuple whose elements can be looked up by name(string literal) along with type and index
 * @tparam NamedTypes pack of NamedType types with unique names
 */
template <typename... NamedTypes>
  requires AllUniqueNamedTypes<NamedTypes...>
struct NamedTuple : std::tuple<typename ExtractType<NamedTypes>::type...> {
  using Base = std::tuple<typename ExtractType<NamedTypes>::type...>;

  /**
   * @brief Construct this NamedTuple initializing all elements
   * @tparam InitTypes types of initializer values
   * @param init_values values to initialize each tuple element
   */
  template <typename... InitTypes>
    requires(!IsNamedTypeValueHelper<InitTypes> && ...)
  constexpr explicit NamedTuple(InitTypes&&... init_values) : Base{std::forward<InitTypes>(init_values)...} {}

  /**
   * @brief Constructor overload used for tagged initialization
   * @tparam Helpers a pack of NamedTypeValueHelpers
   * @param helpers a pack of NamedTypeValueHelpers
   */
  template <typename... Helpers>
    requires((sizeof...(Helpers) > 0u) && (IsNamedTypeValueHelper<Helpers> && ...))
  constexpr explicit NamedTuple(Helpers&&... helpers) {
    (
        [this]<typename NamedValueHelper>(NamedValueHelper&& value_helper) {
          this->template get<NamedValueHelper::tag>() = value_helper.value;
        }(std::forward<decltype(helpers)>(helpers)),
        ...);
  }

  /**
   * @brief Get the number of elements this NamedTuple holds
   * @return the number of elements this NamedTuple holds
   */
  [[nodiscard]] static constexpr std::size_t size() { return sizeof...(NamedTypes); }

  /**
   * @brief Explicit conversion operator to const Base&
   * @return Const reference to Base
   */
  [[nodiscard]] constexpr explicit operator const Base&() const { return static_cast<const Base&>(*this); }

  /**
   * @brief Explicit conversion operator to const Base&&
   * @return Rvalue reference to Base
   */
  [[nodiscard]] constexpr explicit operator const Base&&() const { return static_cast<const Base&&>(*this); }

  /**
   * @brief Explicit conversion operator to Base&&
   * @return Const rvalue reference to Base
   */
  [[nodiscard]] constexpr explicit operator Base&&() { return static_cast<Base&&>(*this); }

  /**
   * @brief Explicit conversion operator to Base&
   * @return Reference to Base
   */
  [[nodiscard]] constexpr explicit operator Base&() { return static_cast<Base&>(*this); }

  /**
   * @brief Set the element of the NamedTuple with the name Tag to value
   * @tparam Tag StringLiteral element name
   * @tparam Value type of value, convertible to the type of the element associated with Tag
   * @param value value to set
   */
  template <StringLiteral Tag, typename Value>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...> &&
             std::is_convertible_v<
                 Value, std::remove_reference_t<std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), Base>>>)
  constexpr void set(Value&& value) {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    std::get<Index>(static_cast<Base&>(*this)) = std::forward<Value>(value);
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() & noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() && noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() const&& noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() & noexcept {
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() && noexcept {
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() const&& noexcept {
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Element-wise comparison of the elements in this NamedTuple with elements in the other
   * NamedTuple
   *
   * Note: The key is not a part of the value of this type,
   * however it does make the equality behave differently.
   *
   * Do we want to compare as if we were calling get<KeyLhs>() == get<KeyRhs>()
   * or as if we are calling get<IndexLhs>() == get<IndexRhs>()? This matters because looking up by
   * key makes it so the order doesnt matter.
   *
   * @tparam OtherNamedTypes pack of types in other NamedTuple
   * @param other another NamedTuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <typename... OtherNamedTypes>
    requires(NamedTypesEqualityComparable<NamedTypes...>::template with<OtherNamedTypes...>())
  [[nodiscard]] constexpr bool operator==(const NamedTuple<OtherNamedTypes...>& other) const {
    static_assert(sizeof...(NamedTypes) == sizeof...(OtherNamedTypes));
    return ((this->template get<NamedTypes{}.tag()>() == other.template get<NamedTypes{}.tag()>()) && ...);
  }

  /**
   * @brief Element-wise comparison of the elements in this NamedTuple with elements in a std::tuple
   *
   * Note: The key is not a part of the value of this type
   *
   * @tparam OtherTypes pack of types in the std::tuple
   * @param other a std::tuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <typename... OtherTypes>
  [[nodiscard]] constexpr bool operator==(const std::tuple<OtherTypes...>& other) const {
    return static_cast<const Base&>(*this) == other;
  }

  /**
   * @brief Spaceship compare against a std::tuple
   * @tparam OtherTypes pack of types in the std::tuple
   * @param other a std::tuple to compare against
   * @return The relation between the first pair of non-equivalent elements if there is any,
   * std::strong_ordering::equal otherwise. For two empty tuples, returns
   * std::strong_ordering::equal.
   */
  template <typename... OtherTypes>
  [[nodiscard]] constexpr auto operator<=>(const std::tuple<OtherTypes...>& other) const {
    return static_cast<const Base&>(*this) <=> other;
  }

  /**
   * @brief Spaceship compare against a std::tuple
   * @tparam OtherNamedTypes pack of types in the std::tuple
   * @param other a std::tuple to compare against
   * @returnThe relation between the first pair of non-equivalent elements if there is any,
   * std::strong_ordering::equal otherwise. For two empty tuples, returns
   * std::strong_ordering::equal.
   */
  template <typename... OtherNamedTypes>
    requires(NamedTypesThreeWayComparable<NamedTypes...>::template with<OtherNamedTypes...>())
  [[nodiscard]] constexpr auto operator<=>(const NamedTuple<OtherNamedTypes...>& other) const {
    static_assert(sizeof...(NamedTypes) == sizeof...(OtherNamedTypes));
    if constexpr (sizeof...(NamedTypes) == 0 && sizeof...(OtherNamedTypes) == 0) { return std::strong_ordering::equal; }

    std::common_comparison_category_t<
        SynthThreeWayResultT<typename ExtractType<NamedTypes>::type, typename ExtractType<OtherNamedTypes>::type>...>
        result = std::strong_ordering::equivalent;

    ([this, &other, &result]<StringLiteral Tag>() {
      result = SynthThreeWay(this->template get<Tag>(), other.template get<Tag>());
      return result != 0;
    }.template operator()<NamedTypes{}.tag()>() ||
     ...);

    return result;
  }

  /**
   * @brief Get the array of tags as a tuple
   * @return A tuple of the tags
   */
  [[nodiscard]] static constexpr auto tags() { return std::tuple{NamedTypes::tag()...}; }
};

/**
 * #brief Deduction guide for use in tagged initialization constructor to deduce NamedTypes from Helpers
 * @tparam Helpers pack of NamedTypeValueHelpers
 * @param helpers pack of NamedTypeValueHelpers
 */
template <typename... Helpers>
  requires(IsNamedTypeValueHelper<Helpers> && ...)
explicit NamedTuple(Helpers&&... helpers) -> NamedTuple<NamedTypeFromHelperT<Helpers>...>;
}  // namespace mguid

// NOLINTBEGIN(cert-dcl58-cpp)
/**
 * @brief Specialization of std::tuple_size for NamedTuple
 * @tparam NamedTypes type list for a NamedTuple
 */
template <typename... NamedTypes>
struct std::tuple_size<mguid::NamedTuple<NamedTypes...>> : std::integral_constant<std::size_t, sizeof...(NamedTypes)> {
};

/**
 * @brief Specialization of std::tuple_element for NamedTuple
 * @tparam Index index of tuple element in tuple
 * @tparam NamedTypes type list for a NamedTuple
 */
template <std::size_t Index, typename... NamedTypes>
struct std::tuple_element<Index, mguid::NamedTuple<NamedTypes...>> {
  static_assert(Index < sizeof...(NamedTypes), "Index out of range");
  using Base = typename mguid::NamedTuple<NamedTypes...>::Base;
  using type = std::tuple_element_t<Index, Base>;
};
// NOLINTEND(cert-dcl58-cpp)

namespace mguid {
/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam NamedTypeVs pack of zero or more named type value helpers
 * @param args zero or more arguments to construct the tuple from
 * @return A NamedTuple object containing the given values
 */
template <typename... NamedTypeVs>
[[nodiscard]] constexpr auto make_tuple(NamedTypeVs&&... args) {
  return std::invoke(
      []<typename... Types>(Types&&... inner_args) {
        return NamedTuple<typename NamedTypeVs::DecayT...>{std::forward<Types>(inner_args)...};
      },
      std::forward<NamedTypeVs>(args).value...);
}

/**
 * @brief Constructs a NamedTuple of references to the arguments in args suitable for forwarding as
 * an argument to a function.
 * @tparam NamedTypeVs pack of zero or more named type value helpers
 * @param args zero or more arguments to construct the tuple from
 * @return A NamedTuple object containing the given values
 */
template <typename... NamedTypeVs>
[[nodiscard]] constexpr auto forward_as_tuple(NamedTypeVs&&... args) {
  return std::invoke(
      []<typename... Types>(Types&&... inner_args) {
        return NamedTuple<typename NamedTypeVs::TypeRRef...>{std::forward<Types>(inner_args)...};
      },
      std::forward<NamedTypeVs>(args).value...);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, Tuple<NamedTypes...>&& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, const Tuple<NamedTypes...>&& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, const Tuple<NamedTypes...>& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, Tuple<NamedTypes...>& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

namespace detail {

/**
 * @brief Concatenate the NamedTypes list from several tuples into one
 */
template <typename... Tuples>
using MergedNamedTypesT =
    decltype(std::tuple_cat(typename NamedTypesFromTuple<std::remove_cvref_t<Tuples>>::type{}...));

/**
 * @brief The resulting NamedTuple type that would result from a tuple_cat
 */
template <typename>
struct NamedTupleFromNamedTypesTuple;

/**
 * @brief Get the NamedTuple type from a std::tuple of NamedTypes
 * @tparam NamedTypes Pack of named types
 */
template <typename... NamedTypes>
struct NamedTupleFromNamedTypesTuple<std::tuple<NamedTypes...>> {
  using type = NamedTuple<NamedTypes...>;
};

/**
 * @brief Get the NamedTuple type from a std::tuple of NamedTypes
 * @tparam NamedTypeTuple A std::tuple containing a list of NamedType
 */
template <typename NamedTypeTuple>
using NamedTupleFromNamedTypesTupleT = typename NamedTupleFromNamedTypesTuple<NamedTypeTuple>::type;

/**
 * @brief Convert a tuple to its base maintaining const ref qualifiers
 * @tparam Tuple type of NamedTuple
 * @param tuple tuple to convert to base
 * @return The tuple cast to Base
 */
template <typename Tuple>
constexpr decltype(auto) to_base_view(Tuple&& tuple) {
  using TupleType = std::remove_reference_t<Tuple>;
  if constexpr (std::is_rvalue_reference_v<decltype(tuple)>) {
    return static_cast<typename TupleType::Base&&>(std::forward<Tuple>(tuple));
  } else if constexpr (std::is_const_v<std::remove_reference_t<Tuple>>) {
    return static_cast<const typename TupleType::Base&>(tuple);
  } else {
    return static_cast<typename TupleType::Base&>(tuple);
  }
}

}  // namespace detail

/**
 * @brief Concatenate a set of NamedTuples
 * @tparam Tuples Types of NamedTuples to concatenate
 * @param tuples a pack of tuples to concatenate
 * @return A concatenated NamedTuple
 */
template <typename... Tuples>
constexpr auto tuple_cat(Tuples&&... tuples) {
  using CombinedTypesTuple = detail::MergedNamedTypesT<Tuples...>;
  using ResultNamedTuple = detail::NamedTupleFromNamedTypesTupleT<CombinedTypesTuple>;

  auto combined_values = std::tuple_cat(detail::to_base_view(std::forward<Tuples>(tuples))...);

  return std::apply([]<typename... TupleElement>(
                        TupleElement&&... elems) { return ResultNamedTuple{std::forward<TupleElement>(elems)...}; },
                    std::move(combined_values));
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>& get(
    NamedTuple<NamedTypes...>& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&& get(
    NamedTuple<NamedTypes...>&& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr const std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&
get(const NamedTuple<NamedTypes...>& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr const std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&&
get(const NamedTuple<NamedTypes...>&& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief A helper template to determine if a type is a NamedTuple
 */
template <typename>
struct IsNamedTupleTrait : std::false_type {};

/**
 * @brief A helper template to determine if a type is a NamedTuple
 * @tparam NamedTypes The types that this NamedTuple holds
 */
template <typename... NamedTypes>
struct IsNamedTupleTrait<NamedTuple<NamedTypes...>> : std::true_type {};

/**
 * @brief Is some type a NamedTuple
 * @tparam MaybeTuple some type to check for NamedTupleness
 */
template <typename MaybeTuple>
concept IsNamedTuple = IsNamedTupleTrait<MaybeTuple>::value;

/**
 * @brief Convenience variable template to check if some type a NamedTuple
 * @tparam MaybeTuple some type to check for NamedTupleness
 */
template <typename MaybeTuple>
constexpr auto IsNamedTupleV = IsNamedTupleTrait<MaybeTuple>::value;

}  // namespace mguid

namespace mguid {

/**
 * @brief An array where each element is tagged with a tag
 * @tparam ValueType element type
 * @tparam Tags pack of tag values
 */
template <typename ValueType, StringLiteral... Tags>
  requires AllUniqueTags<Tags...>
struct TaggedArray : std::array<ValueType, sizeof...(Tags)> {
  using Base = std::array<ValueType, sizeof...(Tags)>;
  using Base::Base;

  /**
   * @brief Construct from pack of values
   * @tparam ValueTypes type of pack of init values
   * @param vals variadic list of values
   */
  template <typename... ValueTypes>
  constexpr explicit(false) TaggedArray(ValueTypes&&... vals) : Base{std::forward<ValueTypes>(vals)...} {}

  /**
   * @brief Get the array element at the Tag provided
   * @tparam Tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  template <StringLiteral Tag>
  [[nodiscard]] constexpr typename Base::reference at() {
    return static_cast<Base&>(*this).at(index_in_pack<Tag, Tags...>());
  }

  /**
   * @brief Get the array element at the Tag provided
   * @tparam Tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  template <StringLiteral Tag>
  [[nodiscard]] constexpr typename Base::const_reference at() const {
    return static_cast<const Base&>(*this).at(index_in_pack<Tag, Tags...>());
  }

  /**
   * @brief Find the array element with the Tag provided
   * @param tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  [[nodiscard]] constexpr typename Base::reference find(std::string_view tag) {
    size_t index{0};
    ([&index, &tag]<auto Type>() {
      if (tag == Type) { return false; }
      ++index;
      return true;
    }.template operator()<Tags>() &&
     ...);
    if (index >= sizeof...(Tags)) { throw std::out_of_range("Out of range"); }
    return static_cast<Base&>(*this)[index];
  }

  /**
   * @brief Find the array element with the Tag provided
   * @param tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  [[nodiscard]] constexpr typename Base::const_reference find(std::string_view tag) const {
    size_t index{0};
    ([&index, &tag]<auto Type>() {
      if (tag == Type) { return false; }
      ++index;
      return true;
    }.template operator()<Tags>() &&
     ...);
    if (index >= sizeof...(Tags)) { throw std::out_of_range("Out of range"); }
    return static_cast<const Base&>(*this)[index];
  }

  /**
   * @brief Get the array element at the Tag provided
   *
   * Note: Get is provided for parity with NamedTuple
   *
   * @tparam Tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  template <StringLiteral Tag>
  [[nodiscard]] constexpr typename Base::reference get() {
    return static_cast<Base&>(*this)[index_in_pack<Tag, Tags...>()];
  }

  /**
   * @brief Get the array element at the Tag provided
   *
   * Note: Get is provided for parity with NamedTuple
   *
   * @tparam Tag a tag to find the element for
   * @return the element corresponding to Tag
   */
  template <StringLiteral Tag>
  [[nodiscard]] constexpr typename Base::const_reference get() const {
    return static_cast<const Base&>(*this)[index_in_pack<Tag, Tags...>()];
  }

  /**
   * @brief Set the value of the element at position corresponding to the Tag
   * @tparam Tag a tag to search for
   * @param value the value to set the element to
   */
  template <StringLiteral Tag>
  constexpr void set(const ValueType& value) {
    static_cast<Base&>(*this).at(index_in_pack<Tag, Tags...>()) = value;
  }

  /**
   * @brief Comparison of the elements in this TaggedArray with elements in the other
   * TaggedArray
   * @tparam OtherTags pack of tags in other TaggedArray
   * @param other another TaggedArray to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <StringLiteral... OtherTags>
  [[nodiscard]] constexpr bool operator==(const TaggedArray<ValueType, OtherTags...>& other) const {
    static_assert(sizeof...(Tags) == sizeof...(OtherTags));
    return ((this->at<Tags>() == other.template at<OtherTags>()) && ...);
  }

  /**
   * @brief Comparison of the elements in this TaggedArray with elements in a std::array
   * @param other a std::array to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  [[nodiscard]] constexpr bool operator==(const std::array<ValueType, sizeof...(Tags)>& other) const {
    return static_cast<const Base&>(*this) == other;
  }

  /**
   * @brief Get tags as a tuple
   * @return A tuple of the tags
   */
  [[nodiscard]] static constexpr auto tags() { return std::tuple{Tags...}; }
};

}  // namespace mguid

// NOLINTBEGIN(cert-dcl58-cpp)
template <typename ValueType, mguid::StringLiteral... Tags>
struct std::tuple_size<mguid::TaggedArray<ValueType, Tags...>> : std::integral_constant<std::size_t, sizeof...(Tags)> {
};

// Specialization of std::tuple_element for TaggedArray
template <std::size_t Index, typename ValueType, mguid::StringLiteral... Tags>
struct std::tuple_element<Index, mguid::TaggedArray<ValueType, Tags...>> {
  using type = ValueType;
};
// NOLINTEND(cert-dcl58-cpp)

namespace mguid {

/**
 * @brief Retrieves the element in the TaggedArray at the specified index.
 * @tparam Index The index of the element to retrieve in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element at the specified index.
 */
template <std::size_t Index, typename ValueType, StringLiteral... Tags>
constexpr ValueType& get(TaggedArray<ValueType, Tags...>& arr) noexcept {
  return std::get<Index>(static_cast<std::array<ValueType, sizeof...(Tags)>&>(arr));
}

/**
 * @brief Retrieves the element in the TaggedArray at the specified index.
 * @tparam Index The index of the element to retrieve in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element at the specified index.
 */
template <std::size_t Index, typename ValueType, StringLiteral... Tags>
constexpr const ValueType& get(const TaggedArray<ValueType, Tags...>& arr) noexcept {
  return std::get<Index>(static_cast<const std::array<ValueType, sizeof...(Tags)>&>(arr));
}

/**
 * @brief Retrieves the element in the TaggedArray at the specified index.
 * @tparam Index The index of the element to retrieve in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element at the specified index.
 */
template <std::size_t Index, typename ValueType, StringLiteral... Tags>
constexpr ValueType&& get(TaggedArray<ValueType, Tags...>&& arr) noexcept {
  return std::get<Index>(std::move(static_cast<std::array<ValueType, sizeof...(Tags)>&&>(arr)));
}

/**
 * @brief Retrieves the element in the TaggedArray at the specified index.
 * @tparam Index The index of the element to retrieve in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element at the specified index.
 */
template <std::size_t Index, typename ValueType, StringLiteral... Tags>
constexpr const ValueType&& get(const TaggedArray<ValueType, Tags...>&& arr) noexcept {
  return std::move(std::get<Index>(static_cast<const std::array<ValueType, sizeof...(Tags)>&>(arr)));
}

/**
 * @brief Retrieves the element in the TaggedArray corresponding to the specified tag.
 * @tparam Tag The tag identifying the element in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element corresponding to the specified tag.
 */
template <StringLiteral Tag, typename ValueType, StringLiteral... Tags>
constexpr ValueType& get(TaggedArray<ValueType, Tags...>& arr) noexcept {
  return arr.template at<Tag>();
}

/**
 * @brief Retrieves the element in the TaggedArray corresponding to the specified tag.
 * @tparam Tag The tag identifying the element in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element corresponding to the specified tag.
 */
template <StringLiteral Tag, typename ValueType, StringLiteral... Tags>
constexpr const ValueType& get(const TaggedArray<ValueType, Tags...>& arr) noexcept {
  return arr.template at<Tag>();
}

/**
 * @brief Retrieves the element in the TaggedArray corresponding to the specified tag.
 * @tparam Tag The tag identifying the element in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element corresponding to the specified tag.
 */
template <StringLiteral Tag, typename ValueType, StringLiteral... Tags>
constexpr ValueType&& get(TaggedArray<ValueType, Tags...>&& arr) noexcept {
  return std::move(arr.template at<Tag>());
}

/**
 * @brief Retrieves the element in the TaggedArray corresponding to the specified tag.
 * @tparam Tag The tag identifying the element in the TaggedArray.
 * @tparam ValueType The type of the elements in the TaggedArray.
 * @tparam Tags The pack of tags associated with the TaggedArray.
 * @param arr The TaggedArray object from which to retrieve the element.
 * @return The element corresponding to the specified tag.
 */
template <StringLiteral Tag, typename ValueType, StringLiteral... Tags>
constexpr const ValueType&& get(const TaggedArray<ValueType, Tags...>&& arr) noexcept {
  return std::move(arr.template at<Tag>());
}

}  // namespace mguid

namespace mguid {

/**
 * @brief A bitset where each bit has a tag
 * @tparam Tags pack of StringLiteral tags
 */
template <StringLiteral... Tags>
  requires AllUniqueTags<Tags...>
struct TaggedBitset : std::bitset<sizeof...(Tags)> {
  using Base = std::bitset<sizeof...(Tags)>;
  using Base::Base;
  using Base::flip;
  using Base::reset;

  /**
   * @brief Find the bit with the Tag provided
   * @param tag a tag to find the bit for
   * @return the bit corresponding to Tag
   */
  [[nodiscard]] constexpr typename Base::reference find(std::string_view tag) {
    size_t index{0};
    ([&index, &tag]<auto Type>() {
      if (tag == Type) { return false; }
      ++index;
      return true;
    }.template operator()<Tags>() &&
     ...);
    if (index >= sizeof...(Tags)) { throw std::out_of_range("Out of range"); }
    return static_cast<Base&>(*this)[sizeof...(Tags) - 1 - index];
  }

  /**
   * @brief Find the bit with the Tag provided
   * @param tag a tag to find the bit for
   * @return the bit corresponding to Tag
   */
  [[nodiscard]] constexpr bool find(std::string_view tag) const {
    size_t index{0};
    ([&index, &tag]<auto Type>() {
      if (tag == Type) { return false; }
      ++index;
      return true;
    }.template operator()<Tags>() &&
     ...);
    if (index >= sizeof...(Tags)) { throw std::out_of_range("Out of range"); }
    return static_cast<const Base&>(*this)[sizeof...(Tags) - 1 - index];
  }

  /**
   * @brief Get the value of the bit at position corresponding to the Tag
   * @tparam Tag a tag to search for
   * @return value of the bit at position corresponding to the Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  [[nodiscard]] bool test() const {
    return static_cast<const Base&>(*this).test(reverse_index_in_pack<Tag, Tags...>());
  }

  /**
   * @brief Set the value of the bit at position corresponding to the Tag
   * @tparam Tag a tag to search for
   * @param value the value to set the bit to
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  TaggedBitset<Tags...>& set(bool value = true) {
    static_cast<Base&>(*this).set(reverse_index_in_pack<Tag, Tags...>(), value);
    return *this;
  }

  /**
   * @brief Reset the bit at the tag position
   * @tparam Tag a tag to search for
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  TaggedBitset<Tags...>& reset() {
    static_cast<Base&>(*this).reset(reverse_index_in_pack<Tag, Tags...>());
    return *this;
  }

  /**
   * @brief Flip the bit at the tag position
   * @tparam Tag a tag to search for
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  TaggedBitset<Tags...>& flip() {
    static_cast<Base&>(*this).flip(reverse_index_in_pack<Tag, Tags...>());
    return *this;
  }

  /**
   * @brief Comparison of the bits in this TaggedBitset with bits in the other
   * TaggedBitset
   *
   * Note: The key is not a part of the value of this type,
   * however it does make the equality behave differently.
   *
   * @tparam OtherTags pack of tags in other TaggedBitset
   * @param other another TaggedBitset to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <StringLiteral... OtherTags>
  [[nodiscard]] constexpr bool operator==(const TaggedBitset<OtherTags...>& other) const {
    static_assert(sizeof...(Tags) == sizeof...(OtherTags));
    return ((this->test<Tags>() == other.template test<OtherTags>()) && ...);
  }

  /**
   * @brief Comparison of the bits in this TaggedBitset with bits in a std::bitset
   * @tparam NSize size of other std::bitset
   * @param other a std::tuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <std::size_t NSize>
  [[nodiscard]] constexpr bool operator==(const std::bitset<NSize>& other) const {
    return static_cast<const Base&>(*this) == other;
  }

  /**
   * @brief Get tags as a tuple
   * @return A tuple of the tags
   */
  [[nodiscard]] static constexpr auto tags() { return std::tuple{Tags...}; }
};

}  // namespace mguid

#endif  // MGUID_SINGLE_INCLUDE_NAMED_NAMED_HPP
