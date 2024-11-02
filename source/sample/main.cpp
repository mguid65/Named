#include <Named/NamedTuple.hpp>
#include <Named/TaggedBitset.hpp>
#include <Named/TaggedArray.hpp>

#include <iostream>

using mguid::NamedTuple;
using mguid::NamedType;
using mguid::NamedTypeV;

int main() {
  int i = 5;
  std::reference_wrapper<int> i_ref{i};
  const auto nt = mguid::make_tuple(NamedTypeV<"int_key">(i_ref), NamedTypeV<"float_key">(1.0f),
                                    NamedTypeV<"char_key">('c'));

  mguid::apply(
      [](const auto&&... args) {
        std::cout << "(";
        std::size_t count{1};
        ((std::cout << "{" << args.first.view() << ":" << args.second
                    << ((count++ != sizeof...(args)) ? "}," : "}")),
         ...);
        std::cout << ")";
      },
      nt);

  std::cout << nt.get<"int_key">() << '\n';
  std::cout << nt.get<"float_key">() << '\n';
  std::cout << nt.get<"char_key">() << '\n';

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

  mguid::TaggedArray<int,"first", "second", "third"> ta{1, 2, 3};
  std::cout << ta.at<"first">() << '\n';
  std::cout << ta.at<"second">() << '\n';
  std::cout << ta.at<"third">() << '\n';
}
