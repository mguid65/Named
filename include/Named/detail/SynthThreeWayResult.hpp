/**
* @brief Definitions for SynthThreeWay/ResultT
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

#ifndef NAMED_SYNTHTHREEWAYRESULT_HPP
#define NAMED_SYNTHTHREEWAYRESULT_HPP

#include <compare>
#include <concepts>

namespace mguid {

/**
 * @brief Recreation of the exposition only function synth-three-way
 * @tparam T Lhs type
 * @tparam U Rhs type
 * @param t lhs
 * @param u rhs
 */
constexpr auto SynthThreeWay = []<class T, class U>(const T& t, const U& u)
  requires requires {
    { t < u } -> std::convertible_to<bool>;
    { u < t } -> std::convertible_to<bool>;
  }
{
  if constexpr (std::three_way_comparable_with<T, U>) {
    return t <=> u;
  } else {
    if (t < u) return std::weak_ordering::less;
    if (u < t) return std::weak_ordering::greater;
    return std::weak_ordering::equivalent;
  }
};

/**
 * @brief Helper type to get the result type of SynthThreeWay
 * @tparam T Lhs type
 * @tparam U Rhs type
 */
template <class T, class U = T>
using SynthThreeWayResultT = decltype(SynthThreeWay(std::declval<T&>(), std::declval<U&>()));

}

#endif // NAMED_SYNTHTHREEWAYRESULT_HPP
