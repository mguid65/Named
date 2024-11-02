#include <Named/NamedTuple.hpp>

#include <iostream>

using mguid::NamedTuple;
using mguid::NamedType;

int main() {
  NamedTuple<
        NamedType<"int_key", int>,
        NamedType<"float_key", float>,
        NamedType<"char_key", char>
      > nt;

  nt.get<"int_key">() = 42;
  nt.get<"float_key">() = 0.3f;
  nt.get<"char_key">() = 'g';

  std::cout << nt.get<"int_key">() << '\n';
  std::cout << nt.get<"float_key">() << '\n';
  std::cout << nt.get<"char_key">() << '\n';
}
