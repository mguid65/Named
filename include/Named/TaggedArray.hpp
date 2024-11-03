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
#include <tuple>

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
  [[nodiscard]] static constexpr auto tags() {
    return std::tuple{Tags...};
  }
};

}  // namespace mguid

template <typename ValueType, mguid::StringLiteral... Tags>
struct std::tuple_size<mguid::TaggedArray<ValueType, Tags...>>
    : std::integral_constant<std::size_t, sizeof...(Tags)> {};

// Specialization of std::tuple_element for TaggedArray
template <std::size_t Index, typename ValueType, mguid::StringLiteral... Tags>
struct std::tuple_element<Index, mguid::TaggedArray<ValueType, Tags...>> {
  using type = ValueType;
};

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

} // namespace mguid

#endif  // NAMED_TAGGEDARRAY_HPP
