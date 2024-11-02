#include "Named/NamedTuple.hpp"

#include <tuple>

int main() {
  mguid::NamedTuple<mguid::NamedType<"key", int>, mguid::NamedType<"key", int>> nt1{0, 1};
}