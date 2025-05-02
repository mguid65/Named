/**
 * @brief Utilities used in NamedTuple implementation
 * @author Matthew Guidry (github: mguid65)
 * @date 2024-11-1
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

#ifndef NAMED_NAMEDTUPLEUTIL_HPP
#define NAMED_NAMEDTUPLEUTIL_HPP

#include "Named/NamedTuple.hpp"
#include <algorithm>
#include <cstdint>
#include <functional>
#include <string_view>
#include <type_traits>

#include "Named/detail/Common.hpp"
#include "Named/detail/StringLiteral.hpp"
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
  return concat_literals<
      "`",
      Key,
      "` was not found in [",
      join_with_delimiter<", ", concat_literals<"`", NamedTypes::tag(), "`">()...>(),
      "], Did you mean `",
      closest,
      "`?"
    >();
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

#endif  // NAMED_NAMEDTUPLEUTIL_HPP
