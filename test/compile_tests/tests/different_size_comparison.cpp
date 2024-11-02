#include "Named/NamedTuple.hpp"

#include <tuple>

int main() {
  mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>> nt1{0, 1};
  mguid::NamedTuple<mguid::NamedType<"key1", int>> nt2{0};

  [[maybe_unused]] bool good = nt1 == nt2;
}