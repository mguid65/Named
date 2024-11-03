/**
 * @brief General utilities
 * @author Matthew Guidry (github: mguid65)
 * @date 2024-11-2
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

#ifndef MGUID_NAMED_COMMON_HPP
#define MGUID_NAMED_COMMON_HPP

#include <algorithm>
#include <stdexcept>

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
  }.template operator()<Haystack>() && ...);
  if (index >= sizeof...(Haystack)) { throw std::out_of_range("Value does not exist in pack"); }
  return sizeof...(Haystack) - 1 - index;
}

/**
 * @brief Determine if all values within a pack of non-types are unique
 * @tparam Nttps pack of non-types
 * @return true if all values in the pack are unique; otherwise false
 */
template <auto... Nttps>
constexpr bool all_unique_nttps() {
  if constexpr (sizeof...(Nttps) == 0) {
    return true;
  } else {
    bool seen[sizeof...(Nttps)] = {false};

    (
        [&seen]<auto Value>() {
          seen[index_in_pack<Value, Nttps...>()] = true;
        }.template operator()<Nttps>(),
        ...);

    return std::all_of(std::begin(seen), std::end(seen), [](bool val) { return val; });
  }
}

}  // namespace mguid

#endif  // MGUID_NAMED_COMMON_HPP
