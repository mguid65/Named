/**
 * @brief TODO
 * @author Matthew Guidry (github: mguid65)
 * @date 11/1/24
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
