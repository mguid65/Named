#include <Named/NamedTuple.hpp>
#include <Named/TaggedArray.hpp>
#include <Named/TaggedBitset.hpp>

#include <iostream>

using mguid::NamedTuple;
using mguid::NamedType;
using mguid::NamedTypeV;

int main() {
  using namespace mguid::literals;

  [[maybe_unused]] auto nt10 = NamedTuple{"key"_nt = 5};

  std::cout << nt10.get<"key">() << '\n';

  using Vec3i = NamedTuple<NamedType<"x", int>, NamedType<"y", int>, NamedType<"z", int>>;

  Vec3i vec1{NamedTypeV<"x">(11), NamedTypeV<"z">(13), NamedTypeV<"y">(12)};
  Vec3i vec2{1, 2, 3};

  Vec3i vec3{"x"_nt = 4, "y"_nt = 5, "z"_nt = 6};

  std::cout << vec1.get<"x">() << std::endl;
  std::cout << vec1.get<"y">() << std::endl;
  std::cout << vec1.get<"z">() << std::endl;

  std::cout << vec2.get<"x">() << std::endl;
  std::cout << vec2.get<"y">() << std::endl;
  std::cout << vec2.get<"z">() << std::endl;

  std::cout << vec3.get<"x">() << std::endl;
  std::cout << vec3.get<"y">() << std::endl;
  std::cout << vec3.get<"z">() << std::endl;

  int i = 5;
  const std::reference_wrapper<int> i_ref{i};
  const auto nt = mguid::make_tuple(NamedTypeV<"int_key">(i_ref), NamedTypeV<"float_key">(1.0f),
                                    NamedTypeV<"char_key">('c'));

  mguid::apply(
      [](const auto&&... args) {
        std::cout << "{";
        std::size_t count{1};
        ((std::cout << args.first.view() << ":" << args.second
                    << ((count++ != sizeof...(args)) ? "," : "")),
         ...);
        std::cout << "}\n";
      },
      nt);

  std::cout << nt.get<"int_key">() << '\n';
  std::cout << nt.get<"float_key">() << '\n';
  std::cout << nt.get<"char_key">() << '\n';

  std::cout << nt.get<0>() << '\n';
  std::cout << nt.get<1>() << '\n';
  std::cout << nt.get<2>() << '\n';

  std::cout << mguid::get<"int_key">(nt) << '\n';
  std::cout << mguid::get<"float_key">(nt) << '\n';
  std::cout << mguid::get<"char_key">(nt) << '\n';

  std::cout << std::get<0>(nt) << '\n';
  std::cout << std::get<1>(nt) << '\n';
  std::cout << std::get<2>(nt) << '\n';

  const auto [int_key, float_key, char_key] = nt;
  std::cout << int_key << '\n';
  std::cout << float_key << '\n';
  std::cout << char_key << '\n';

  mguid::TaggedBitset<"first", "second", "third"> tb{0b101};
  std::cout << tb.test<"first">() << '\n';
  std::cout << tb.test<"second">() << '\n';
  std::cout << tb.test<"third">() << '\n';
  std::cout << tb.size() << '\n';
  tb.reset();

  std::cout << tb.test<"first">() << '\n';
  std::cout << tb.test<"second">() << '\n';
  std::cout << tb.test<"third">() << '\n';
  std::cout << tb.size() << '\n';

  mguid::TaggedArray<int, "first", "second", "third"> ta{1, 2, 3};
  std::cout << ta.at<"first">() << '\n';
  std::cout << ta.at<"second">() << '\n';
  std::cout << ta.at<"third">() << '\n';

  std::apply(
      [](auto&&... args) {
        std::cout << "(";
        std::size_t count{1};
        constexpr auto size = sizeof...(args);
        ((std::cout << "\"" << args << "\"" << (count++ != size ? "," : "")), ...);
        std::cout << ")" << '\n';
      },
      ta.tags());

  const auto& first = ta.find("first");

  std::cout << first << '\n';
}
