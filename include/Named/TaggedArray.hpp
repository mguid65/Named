/**
 * @brief Definitions for tagged array type
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

#ifndef NAMED_TAGGEDARRAY_HPP
#define NAMED_TAGGEDARRAY_HPP

#include <array>

#include "Named/detail/Common.hpp"
#include "Named/detail/StringLiteral.hpp"

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
  constexpr explicit(false) TaggedArray(ValueTypes&&... vals)
      : Base{std::forward<ValueTypes>(vals)...} {}

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
};

}  // namespace mguid

#endif  // NAMED_TAGGEDARRAY_HPP
