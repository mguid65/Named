#include "Named/NamedTuple.hpp"

#include <tuple>

int main() {
  std::tuple_element_t<0, mguid::NamedTuple<>> t;
}