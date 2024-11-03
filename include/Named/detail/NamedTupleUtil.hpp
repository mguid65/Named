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

#include <algorithm>
#include <cstdint>
#include <functional>
#include <string_view>
#include <type_traits>

#include "Named/detail/StringLiteral.hpp"
#include "Named/detail/Common.hpp"

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
  std::unwrap_ref_decay_t<ValueType> value{};
};

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
 * @brief Check if Key is one of the tags of a NamedType within the NamedTypes pack
 * @tparam Key key to search for
 * @tparam NamedTypes pack of NamedType to search in
 * @return true if Key exists as the tag of a NamedType in NamedTypes; otherwise false
 */
template <StringLiteral Key, NamedType... NamedTypes>
constexpr bool is_one_of() {
  return (... || (Key == NamedTypes));
}

} // namespace mguid

#endif // NAMED_NAMEDTUPLEUTIL_HPP
