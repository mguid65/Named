/**
 * @brief Definitions for compile time string literal class
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

#ifndef NAMED_STRINGLITERAL_HPP
#define NAMED_STRINGLITERAL_HPP

#include <algorithm>
#include <string_view>

#include "Named/detail/Common.hpp"

namespace mguid {

/**
 * @brief Compile time string literal container
 * @tparam NSize size of string literal
 */
template <size_t NSize>
struct StringLiteral {
  // NOLINTBEGIN(google-explicit-constructor)
  /**
   * @brief Construct a StringLiteral from a string literal
   * @param str a string literal as a const reference to a sized char array
   */
  constexpr explicit(false) StringLiteral(char const (&str)[NSize]) : value{'\0'} {
    std::copy_n(str, NSize, value);
  }
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
  [[nodiscard]] constexpr operator std::string_view() const {
    return std::string_view{value, size - 1};
  }

  /**
   * @brief Convert this StringLiteral to a string_view
   * @return a string_view of the data in this StringLiteral
   */
  [[nodiscard]] constexpr std::string_view view() const {
    return std::string_view{value, size - 1};
  }

  char value[NSize];
  static constexpr size_t size{NSize};
};

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

}  // namespace mguid

#endif  // NAMED_STRINGLITERAL_HPP
