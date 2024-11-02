/**
 * @brief TODO
 * @author Matthew Guidry (github: mguid65)
 * @date 11/1/24
 */

#ifndef NAMED_TAGGEDBITSET_HPP
#define NAMED_TAGGEDBITSET_HPP

#include <bitset>

#include "Named/detail/StringLiteral.hpp"

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
    return static_cast<const Base&>(*this).test(key_index<Tag, Tags...>());
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
    static_cast<const Base&>(*this).set(key_index<Tag, Tags...>(), value);
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
    static_cast<const Base&>(*this).reset(key_index<Tag, Tags...>());
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
    static_cast<const Base&>(*this).flip(key_index<Tag, Tags...>());
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
