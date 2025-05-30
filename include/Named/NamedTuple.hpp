/**
 * @author Matthew Guidry (github: mguid65)
 * @date 2024-10-11
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

#ifndef MGUID_NAMED_NAMEDTUPLE_H
#define MGUID_NAMED_NAMEDTUPLE_H

#include <algorithm>
#include <cstdint>
#include <functional>
#include <tuple>
#include <type_traits>

#include "Named/detail/NamedTupleUtil.hpp"
#include "Named/detail/StringLiteral.hpp"
#include "Named/detail/SynthThreeWayResult.hpp"

namespace mguid {

/**
 * @brief A tuple whose elements can be looked up by name(string literal) along with type and index
 * @tparam NamedTypes pack of NamedType types with unique names
 */
template <typename... NamedTypes>
  requires AllUniqueNamedTypes<NamedTypes...>
struct NamedTuple : std::tuple<typename ExtractType<NamedTypes>::type...> {
  using Base = std::tuple<typename ExtractType<NamedTypes>::type...>;

  /**
   * @brief Construct this NamedTuple initializing all elements
   * @tparam InitTypes types of initializer values
   * @param init_values values to initialize each tuple element
   */
  template <typename... InitTypes>
    requires(!IsNamedTypeValueHelper<InitTypes> && ...)
  constexpr explicit NamedTuple(InitTypes&&... init_values) : Base{std::forward<InitTypes>(init_values)...} {}

  /**
   * @brief Constructor overload used for tagged initialization
   * @tparam Helpers a pack of NamedTypeValueHelpers
   * @param helpers a pack of NamedTypeValueHelpers
   */
  template <typename... Helpers>
    requires((sizeof...(Helpers) > 0u) && (IsNamedTypeValueHelper<Helpers> && ...))
  constexpr explicit NamedTuple(Helpers&&... helpers) {
    (
        [this]<typename NamedValueHelper>(NamedValueHelper&& value_helper) {
          this->template get<NamedValueHelper::tag>() = value_helper.value;
        }(std::forward<decltype(helpers)>(helpers)),
        ...);
  }

  /**
   * @brief Get the number of elements this NamedTuple holds
   * @return the number of elements this NamedTuple holds
   */
  [[nodiscard]] static constexpr std::size_t size() { return sizeof...(NamedTypes); }

  /**
   * @brief Explicit conversion operator to const Base&
   * @return Const reference to Base
   */
  [[nodiscard]] constexpr explicit operator const Base&() const { return static_cast<const Base&>(*this); }

  /**
   * @brief Explicit conversion operator to const Base&&
   * @return Rvalue reference to Base
   */
  [[nodiscard]] constexpr explicit operator const Base&&() const { return static_cast<const Base&&>(*this); }

  /**
   * @brief Explicit conversion operator to Base&&
   * @return Const rvalue reference to Base
   */
  [[nodiscard]] constexpr explicit operator Base&&() { return static_cast<Base&&>(*this); }

  /**
   * @brief Explicit conversion operator to Base&
   * @return Reference to Base
   */
  [[nodiscard]] constexpr explicit operator Base&() { return static_cast<Base&>(*this); }

  /**
   * @brief Set the element of the NamedTuple with the name Tag to value
   * @tparam Tag StringLiteral element name
   * @tparam Value type of value, convertible to the type of the element associated with Tag
   * @param value value to set
   */
  template <StringLiteral Tag, typename Value>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...> &&
             std::is_convertible_v<
                 Value, std::remove_reference_t<std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), Base>>>)
  constexpr void set(Value&& value) {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    std::get<Index>(static_cast<Base&>(*this)) = std::forward<Value>(value);
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() & noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() && noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose name is Tag
   * @tparam Tag Name of the element in the NamedTuple to get
   * @return the element of the NamedTuple whose name is Tag
   */
  template <StringLiteral Tag>
    requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
  [[nodiscard]] constexpr decltype(auto) get() const&& noexcept {
    constexpr std::size_t Index = index_in_pack<Tag, NamedTypes{}...>();
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() & noexcept {
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() const& noexcept {
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() && noexcept {
    return std::get<Index>(static_cast<Base&>(*this));
  }

  /**
   * @brief Extracts the element of the NamedTuple whose index is Index
   * @tparam Index index of the element to get
   * @return the element of the NamedTuple whose index is Index
   */
  template <std::size_t Index>
    requires(sizeof...(NamedTypes) > 0 && Index < sizeof...(NamedTypes))
  [[nodiscard]] constexpr decltype(auto) get() const&& noexcept {
    return std::get<Index>(static_cast<const Base&>(*this));
  }

  /**
   * @brief Element-wise comparison of the elements in this NamedTuple with elements in the other
   * NamedTuple
   *
   * Note: The key is not a part of the value of this type,
   * however it does make the equality behave differently.
   *
   * Do we want to compare as if we were calling get<KeyLhs>() == get<KeyRhs>()
   * or as if we are calling get<IndexLhs>() == get<IndexRhs>()? This matters because looking up by
   * key makes it so the order doesnt matter.
   *
   * @tparam OtherNamedTypes pack of types in other NamedTuple
   * @param other another NamedTuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <typename... OtherNamedTypes>
    requires(NamedTypesEqualityComparable<NamedTypes...>::template with<OtherNamedTypes...>())
  [[nodiscard]] constexpr bool operator==(const NamedTuple<OtherNamedTypes...>& other) const {
    static_assert(sizeof...(NamedTypes) == sizeof...(OtherNamedTypes));
    return ((this->template get<NamedTypes{}.tag()>() == other.template get<NamedTypes{}.tag()>()) && ...);
  }

  /**
   * @brief Element-wise equality comparison of the elements in this NamedTuple with elements in a std::tuple
   *
   * Note: The key order is irrelevant for this comparison
   *
   * @tparam OtherTypes pack of types in the std::tuple
   * @param other a std::tuple to compare against
   * @return Returns true if all pairs of corresponding elements are equal; otherwise false
   */
  template <typename... OtherTypes>
  [[nodiscard]] constexpr bool operator==(const std::tuple<OtherTypes...>& other) const {
    return static_cast<const Base&>(*this) == other;
  }

  /**
   * @brief Spaceship compare against a std::tuple
   * @tparam OtherTypes pack of types in the std::tuple
   * @param other a std::tuple to compare against
   * @return The relation between the first pair of non-equivalent elements if there is any,
   * std::strong_ordering::equal otherwise. For two empty tuples, returns
   * std::strong_ordering::equal.
   */
  template <typename... OtherTypes>
  [[nodiscard]] constexpr auto operator<=>(const std::tuple<OtherTypes...>& other) const {
    return static_cast<const Base&>(*this) <=> other;
  }

  /**
   * @brief Threeway compare with another NamedTuple 
   * @tparam OtherNamedTypes pack of NamedTypes in the NamedTuple
   * @param other another NamedTuple to compare against
   * @returnThe relation between the first pair of non-equivalent elements if there is any,
   * std::strong_ordering::equal otherwise. For two empty tuples, returns
   * std::strong_ordering::equal.
   */
  template <typename... OtherNamedTypes>
    requires(sizeof...(NamedTypes) == sizeof...(OtherNamedTypes) &&
             NamedTypesThreeWayComparable<NamedTypes...>::template with<OtherNamedTypes...>())
  [[nodiscard]] constexpr auto operator<=>(const NamedTuple<OtherNamedTypes...>& other) const {
    if constexpr (sizeof...(NamedTypes) == 0 && sizeof...(OtherNamedTypes) == 0) { return std::strong_ordering::equal; }

    std::common_comparison_category_t<
        SynthThreeWayResultT<typename ExtractType<NamedTypes>::type, typename ExtractType<OtherNamedTypes>::type>...>
        result = std::strong_ordering::equivalent;

    ([this, &other, &result]<StringLiteral Tag>() {
      result = SynthThreeWay(this->template get<Tag>(), other.template get<Tag>());
      return result != 0;
    }.template operator()<NamedTypes{}.tag()>() ||
     ...);

    return result;
  }

  /**
   * @brief Get the array of tags as a tuple
   * @return A tuple of the tags
   */
  [[nodiscard]] static constexpr auto tags() { return std::tuple{NamedTypes::tag()...}; }
};

/**
 * #brief Deduction guide for use in tagged initialization constructor to deduce NamedTypes from Helpers
 * @tparam Helpers pack of NamedTypeValueHelpers
 * @param helpers pack of NamedTypeValueHelpers
 */
template <typename... Helpers>
  requires(IsNamedTypeValueHelper<Helpers> && ...)
explicit NamedTuple(Helpers&&... helpers) -> NamedTuple<NamedTypeFromHelperT<Helpers>...>;
}  // namespace mguid

// NOLINTBEGIN(cert-dcl58-cpp)
/**
 * @brief Specialization of std::tuple_size for NamedTuple
 * @tparam NamedTypes type list for a NamedTuple
 */
template <typename... NamedTypes>
struct std::tuple_size<mguid::NamedTuple<NamedTypes...>> : std::integral_constant<std::size_t, sizeof...(NamedTypes)> {
};

/**
 * @brief Specialization of std::tuple_element for NamedTuple
 * @tparam Index index of tuple element in tuple
 * @tparam NamedTypes type list for a NamedTuple
 */
template <std::size_t Index, typename... NamedTypes>
struct std::tuple_element<Index, mguid::NamedTuple<NamedTypes...>> {
  static_assert(Index < sizeof...(NamedTypes), "Index out of range");
  using Base = typename mguid::NamedTuple<NamedTypes...>::Base;
  using type = std::tuple_element_t<Index, Base>;
};
// NOLINTEND(cert-dcl58-cpp)

namespace mguid {
/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam NamedTypeVs pack of zero or more named type value helpers
 * @param args zero or more arguments to construct the tuple from
 * @return A NamedTuple object containing the given values
 */
template <typename... NamedTypeVs>
[[nodiscard]] constexpr auto make_tuple(NamedTypeVs&&... args) {
  return std::invoke(
      []<typename... Types>(Types&&... inner_args) {
        return NamedTuple<typename NamedTypeVs::DecayT...>{std::forward<Types>(inner_args)...};
      },
      std::forward<NamedTypeVs>(args).value...);
}

/**
 * @brief Constructs a NamedTuple of references to the arguments in args suitable for forwarding as
 * an argument to a function.
 * @tparam NamedTypeVs pack of zero or more named type value helpers
 * @param args zero or more arguments to construct the tuple from
 * @return A NamedTuple object containing the given values
 */
template <typename... NamedTypeVs>
[[nodiscard]] constexpr auto forward_as_tuple(NamedTypeVs&&... args) {
  return std::invoke(
      []<typename... Types>(Types&&... inner_args) {
        return NamedTuple<typename NamedTypeVs::TypeRRef...>{std::forward<Types>(inner_args)...};
      },
      std::forward<NamedTypeVs>(args).value...);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, Tuple<NamedTypes...>&& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, const Tuple<NamedTypes...>&& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, const Tuple<NamedTypes...>& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

/**
 * @brief Creates a NamedTuple object, deducing the target type from the types of arguments.
 * @tparam Func type of function to apply
 * @tparam Tuple type of tuple
 * @tparam NamedTypes pack of zero or more named type value helpers
 * @param func A function to apply to named tuple
 * @param named_tuple a tuple to apply the function to
 * @return A NamedTuple object containing the given values
 */
template <typename Func, template <typename...> typename Tuple, typename... NamedTypes>
[[nodiscard]] constexpr decltype(auto) apply(Func&& func, Tuple<NamedTypes...>& named_tuple) {
  return std::apply(
      [&]<typename... Args>(Args&&... args) {
        return std::invoke(std::forward<Func>(func), std::pair(NamedTypes{}.tag(), std::forward<Args>(args))...);
      },
      named_tuple);
}

namespace detail {

/**
 * @brief Concatenate the NamedTypes list from several tuples into one
 */
template <typename... Tuples>
using MergedNamedTypesT =
    decltype(std::tuple_cat(typename NamedTypesFromTuple<std::remove_cvref_t<Tuples>>::type{}...));

/**
 * @brief The resulting NamedTuple type that would result from a tuple_cat
 */
template <typename>
struct NamedTupleFromNamedTypesTuple;

/**
 * @brief Get the NamedTuple type from a std::tuple of NamedTypes
 * @tparam NamedTypes Pack of named types
 */
template <typename... NamedTypes>
struct NamedTupleFromNamedTypesTuple<std::tuple<NamedTypes...>> {
  using type = NamedTuple<NamedTypes...>;
};

/**
 * @brief Get the NamedTuple type from a std::tuple of NamedTypes
 * @tparam NamedTypeTuple A std::tuple containing a list of NamedType
 */
template <typename NamedTypeTuple>
using NamedTupleFromNamedTypesTupleT = typename NamedTupleFromNamedTypesTuple<NamedTypeTuple>::type;

/**
 * @brief Convert a tuple to its base maintaining const ref qualifiers
 * @tparam Tuple type of NamedTuple
 * @param tuple tuple to convert to base
 * @return The tuple cast to Base
 */
template <typename Tuple>
constexpr decltype(auto) to_base_view(Tuple&& tuple) {
  using TupleType = std::remove_reference_t<Tuple>;
  if constexpr (std::is_rvalue_reference_v<decltype(tuple)>) {
    return static_cast<typename TupleType::Base&&>(std::forward<Tuple>(tuple));
  } else if constexpr (std::is_const_v<std::remove_reference_t<Tuple>>) {
    return static_cast<const typename TupleType::Base&>(tuple);
  } else {
    return static_cast<typename TupleType::Base&>(tuple);
  }
}

}  // namespace detail

/**
 * @brief Concatenate a set of NamedTuples
 * @tparam Tuples Types of NamedTuples to concatenate
 * @param tuples a pack of tuples to concatenate
 * @return A concatenated NamedTuple
 */
template <typename... Tuples>
constexpr auto tuple_cat(Tuples&&... tuples) {
  using CombinedTypesTuple = detail::MergedNamedTypesT<Tuples...>;
  using ResultNamedTuple = detail::NamedTupleFromNamedTypesTupleT<CombinedTypesTuple>;

  auto combined_values = std::tuple_cat(detail::to_base_view(std::forward<Tuples>(tuples))...);

  return std::apply([]<typename... TupleElement>(
                        TupleElement&&... elems) { return ResultNamedTuple{std::forward<TupleElement>(elems)...}; },
                    std::move(combined_values));
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>& get(
    NamedTuple<NamedTypes...>& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&& get(
    NamedTuple<NamedTypes...>&& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr const std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&
get(const NamedTuple<NamedTypes...>& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief Extracts the element from the NamedTuple with the key Tag. Tag must be one of the tags
 * associated with a type in NamedTypes.
 * @tparam Tag the tag for the element to find
 * @tparam NamedTypes pack of NamedType in the NamedTuple
 * @param nt NamedTuple whose element to extract
 * @return A reference to the selected element of nt
 */
template <StringLiteral Tag, typename... NamedTypes>
  requires(sizeof...(NamedTypes) > 0 && CheckKeys<Tag, NamedTypes...>)
[[nodiscard]] constexpr const std::tuple_element_t<index_in_pack<Tag, NamedTypes{}...>(), NamedTuple<NamedTypes...>>&&
get(const NamedTuple<NamedTypes...>&& nt) noexcept {
  return nt.template get<Tag>();
}

/**
 * @brief A helper template to determine if a type is a NamedTuple
 */
template <typename>
struct IsNamedTupleTrait : std::false_type {};

/**
 * @brief A helper template to determine if a type is a NamedTuple
 * @tparam NamedTypes The types that this NamedTuple holds
 */
template <typename... NamedTypes>
struct IsNamedTupleTrait<NamedTuple<NamedTypes...>> : std::true_type {};

/**
 * @brief Is some type a NamedTuple
 * @tparam MaybeTuple some type to check for NamedTupleness
 */
template <typename MaybeTuple>
concept IsNamedTuple = IsNamedTupleTrait<MaybeTuple>::value;

/**
 * @brief Convenience variable template to check if some type a NamedTuple
 * @tparam MaybeTuple some type to check for NamedTupleness
 */
template <typename MaybeTuple>
constexpr auto IsNamedTupleV = IsNamedTupleTrait<MaybeTuple>::value;

}  // namespace mguid

#endif  // MGUID_NAMED_NAMEDTUPLE_H
