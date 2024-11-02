/**
* @brief Definitions for tagged bitset type
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

#ifndef NAMED_TAGGEDBITSET_HPP
#define NAMED_TAGGEDBITSET_HPP

#include <bitset>

#include "Named/detail/StringLiteral.hpp"
#include "Named/detail/Common.hpp"

namespace mguid {

/**
 * @brief A bitset where each bit has a tag
 * @tparam Tags pack of StringLiteral tags
 */
template <StringLiteral... Tags>
struct TaggedBitset : std::bitset<sizeof...(Tags)> {
  using Base = std::bitset<sizeof...(Tags)>;
  using Base::Base;
  using Base::flip;
  using Base::reset;

  /**
   * @brief Get the value of the bit at position corresponding to the Tag
   * @tparam Tag a tag to search for
   * @return value of the bit at position corresponding to the Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  [[nodiscard]] bool test() const {
    return static_cast<const Base&>(*this).test(index_in_pack<Tag, Tags...>());
  }

  /**
   * @brief Set the value of the bit at position corresponding to the Tag
   * @tparam Tag a tag to search for
   * @param value the value to set the bit to
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  [[nodiscard]] TaggedBitset<Tags...>& set(bool value = true) {
    static_cast<const Base&>(*this).set(index_in_pack<Tag, Tags...>(), value);
    return *this;
  }

  /**
   * @brief Reset the bit at the tag position
   * @tparam Tag a tag to search for
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  [[nodiscard]] TaggedBitset<Tags...>& reset() {
    static_cast<const Base&>(*this).reset(index_in_pack<Tag, Tags...>());
    return *this;
  }

  /**
   * @brief Flip the bit at the tag position
   * @tparam Tag a tag to search for
   * @return reference to this
   */
  template <StringLiteral Tag>
    requires(sizeof...(Tags) > 0 && is_one_of<Tag, Tags...>())
  [[nodiscard]] TaggedBitset<Tags...>& flip() {
    static_cast<const Base&>(*this).flip(index_in_pack<Tag, Tags...>());
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
  template <typename... OtherTags>
  [[nodiscard]] constexpr bool operator==(const NamedTuple<OtherTags...>& other) const {
    static_assert(sizeof...(Tags) == sizeof...(OtherTags));
    return ((this->test<Tags>() == other.template test<OtherTags>()) && ...);
  }

  /**
   * @brief Comparison of the bits in this TaggedBitset with bits in a std::bitset
   *
   * @tparam NSize size of other std::bitset
   * @param other a std::tuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <std::size_t NSize>
  [[nodiscard]] constexpr bool operator==(const std::bitset<NSize>& other) const {
    return static_cast<const Base&>(*this) == other;
  }
};

} // namespace mguid

#endif // NAMED_TAGGEDBITSET_HPP
