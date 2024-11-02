#include "Named/NamedTuple.hpp"

#include <tuple>

int main() {
  std::tuple_element_t<2, mguid::NamedTuple<mguid::NamedType<"key", int>>> t;
}