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

#include "Named/detail/StringLiteral.hpp"

namespace mguid {

/**
 * @brief An array where each element is tagged with a tag
 * @tparam ValueType element type
 * @tparam Tags pack of tag values
 */
template <typename ValueType, StringLiteral... Tags>
struct TaggedArray : std::array<ValueType, sizeof...(Tags)> {
  using Base = std::array<ValueType, sizeof...(Tags)>;
  using Base::Base;

  template <StringLiteral Tag>
  [[nodiscard]] constexpr Base::reference at() {
    return at(key_index<Tag, Tags...>());
  }

  template <StringLiteral Tag>
  [[nodiscard]] constexpr Base::const_reference at() const {
    return at(key_index<Tag, Tags...>());
  }
};

}

#endif // NAMED_TAGGEDARRAY_HPP
