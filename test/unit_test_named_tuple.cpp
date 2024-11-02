#include "Named/NamedTuple.hpp"

#include <catch2/catch_all.hpp>

#include <cstdint>
#include <string>
#include <tuple>
#include <type_traits>

TEST_CASE("NamedTuple Constructor") {
  SECTION("Empty") {
    [[maybe_unused]] mguid::NamedTuple<> nt{};
    REQUIRE(std::tuple_size_v<decltype(nt)> == std::size_t{0});
  }
  SECTION("Single Type") {
    [[maybe_unused]] mguid::NamedTuple<mguid::NamedType<"key", int>> nt{};
    REQUIRE(std::tuple_size_v<decltype(nt)> == std::size_t{1});
    REQUIRE(std::same_as<std::tuple_element_t<0, decltype(nt)>, int>);
  }
  SECTION("Multiple Types") {
    [[maybe_unused]] mguid::NamedTuple<
        mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>, mguid::NamedType<"key3", int>,
        mguid::NamedType<"key4", int>, mguid::NamedType<"key5", int>, mguid::NamedType<"key6", int>,
        mguid::NamedType<"key7", int>, mguid::NamedType<"key8", int>> nt{};
    REQUIRE(std::tuple_size_v<decltype(nt)> == std::size_t{8});
  }
  SECTION("Aggregate Initialization") {
    [[maybe_unused]] mguid::NamedTuple<
        mguid::NamedType<"key1", int>, mguid::NamedType<"key2", char>,
        mguid::NamedType<"key3", int>, mguid::NamedType<"key4", char>,
        mguid::NamedType<"key5", int>, mguid::NamedType<"key6", char>,
        mguid::NamedType<"key7", int>, mguid::NamedType<"key8", char>> nt{1, 'a', 3, 'b',
                                                                          5, 'c', 7, 'd'};
    SUCCEED();
  }
  SECTION("Nested") {
    [[maybe_unused]] mguid::NamedTuple<mguid::NamedType<
        "nested",
        mguid::NamedTuple<mguid::NamedType<
            "nested", mguid::NamedTuple<mguid::NamedType<
                          "nested", mguid::NamedTuple<mguid::NamedType<"inner", int>>>>>>>> nt;

    REQUIRE(std::is_same_v<
            std::remove_cvref_t<decltype(nt.get<"nested">())>,
            mguid::NamedTuple<mguid::NamedType<
                "nested", mguid::NamedTuple<mguid::NamedType<
                              "nested", mguid::NamedTuple<mguid::NamedType<"inner", int>>>>>>>);

    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"nested">().get<"nested">())>,
                           mguid::NamedTuple<mguid::NamedType<
                               "nested", mguid::NamedTuple<mguid::NamedType<"inner", int>>>>>);
  }
  SECTION("Multiple Types") {
    [[maybe_unused]] mguid::NamedTuple<
        mguid::NamedType<"bool", bool>, mguid::NamedType<"int8_t", std::int8_t>,
        mguid::NamedType<"int16_t", std::int16_t>, mguid::NamedType<"int32_t", std::int32_t>,
        mguid::NamedType<"int64_t", std::int64_t>, mguid::NamedType<"uint8_t", std::uint8_t>,
        mguid::NamedType<"uint16_t", std::uint16_t>, mguid::NamedType<"uint32_t", std::uint32_t>,
        mguid::NamedType<"uint64_t", std::uint64_t>, mguid::NamedType<"float", float>,
        mguid::NamedType<"double", double>, mguid::NamedType<"string", std::string>> nt;

    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"bool">())>, bool>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"int8_t">())>, std::int8_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"int16_t">())>, std::int16_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"int32_t">())>, std::int32_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"int64_t">())>, std::int64_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"uint8_t">())>, std::uint8_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"uint16_t">())>, std::uint16_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"uint32_t">())>, std::uint32_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"uint64_t">())>, std::uint64_t>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"float">())>, float>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"double">())>, double>);
    REQUIRE(std::is_same_v<std::remove_cvref_t<decltype(nt.get<"string">())>, std::string>);
  }
}

TEST_CASE("NamedTuple Setter") {
  SECTION("Set") {
    [[maybe_unused]] mguid::NamedTuple<mguid::NamedType<"key", int>> nt{2};
    REQUIRE(nt.get<"key">() == 2);
    nt.set<"key">(42);
    REQUIRE(nt.get<"key">() == 42);
  }
}

TEST_CASE("NamedTuple Getter") {
  [[maybe_unused]] mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", char>,
                                     mguid::NamedType<"key3", int>, mguid::NamedType<"key4", char>>
      nt{1, 'a', 3, 'b'};

  [[maybe_unused]] const mguid::NamedTuple<
      mguid::NamedType<"key1", int>, mguid::NamedType<"key2", char>, mguid::NamedType<"key3", int>,
      mguid::NamedType<"key4", char>> const_nt{1, 'a', 3, 'b'};

  SECTION("Std Get") {
    REQUIRE(std::get<0>(nt) == 1);
    REQUIRE(std::get<1>(nt) == 'a');
    REQUIRE(std::get<2>(nt) == 3);
    REQUIRE(std::get<3>(nt) == 'b');

    REQUIRE(std::get<0>(const_nt) == 1);
    REQUIRE(std::get<1>(const_nt) == 'a');
    REQUIRE(std::get<2>(const_nt) == 3);
    REQUIRE(std::get<3>(const_nt) == 'b');

    STATIC_REQUIRE(std::is_same_v<decltype(std::get<0>(nt)), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<1>(nt)), char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<2>(nt)), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<3>(nt)), char&>);

    STATIC_REQUIRE(std::is_same_v<decltype(std::get<0>(const_nt)), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<1>(const_nt)), const char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<2>(const_nt)), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(std::get<3>(const_nt)), const char&>);
  }
  SECTION("Mguid Get") {
    REQUIRE(mguid::get<"key1">(nt) == 1);
    REQUIRE(mguid::get<"key2">(nt) == 'a');
    REQUIRE(mguid::get<"key3">(nt) == 3);
    REQUIRE(mguid::get<"key4">(nt) == 'b');

    REQUIRE(mguid::get<"key1">(const_nt) == 1);
    REQUIRE(mguid::get<"key2">(const_nt) == 'a');
    REQUIRE(mguid::get<"key3">(const_nt) == 3);
    REQUIRE(mguid::get<"key4">(const_nt) == 'b');

    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key1">(nt)), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key2">(nt)), char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key3">(nt)), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key4">(nt)), char&>);

    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key1">(const_nt)), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key2">(const_nt)), const char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key3">(const_nt)), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(mguid::get<"key4">(const_nt)), const char&>);
  }
  SECTION("Member Get") {
    REQUIRE(nt.get<"key1">() == 1);
    REQUIRE(nt.get<"key2">() == 'a');
    REQUIRE(nt.get<"key3">() == 3);
    REQUIRE(nt.get<"key4">() == 'b');

    REQUIRE(const_nt.get<"key1">() == 1);
    REQUIRE(const_nt.get<"key2">() == 'a');
    REQUIRE(const_nt.get<"key3">() == 3);
    REQUIRE(const_nt.get<"key4">() == 'b');

    STATIC_REQUIRE(std::is_same_v<decltype(nt.get<"key1">()), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(nt.get<"key2">()), char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(nt.get<"key3">()), int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(nt.get<"key4">()), char&>);

    STATIC_REQUIRE(std::is_same_v<decltype(const_nt.get<"key1">()), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(const_nt.get<"key2">()), const char&>);
    STATIC_REQUIRE(std::is_same_v<decltype(const_nt.get<"key3">()), const int&>);
    STATIC_REQUIRE(std::is_same_v<decltype(const_nt.get<"key4">()), const char&>);
  }
}

TEST_CASE("Comparison") {
  SECTION("Empty Equality") {
    mguid::NamedTuple<> nt1;
    mguid::NamedTuple<> nt2;

    const mguid::NamedTuple<> const_nt1;
    const mguid::NamedTuple<> const_nt2;

    REQUIRE(nt1 == nt2);
    REQUIRE(const_nt1 == const_nt2);
    REQUIRE(const_nt1 == nt2);
    REQUIRE(nt1 == const_nt2);
  }
  SECTION("Single Type Equality") {
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt1{1};
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt2{1};

    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt1{1};
    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt2{1};

    REQUIRE(nt1 == nt2);
    REQUIRE(const_nt1 == const_nt2);
    REQUIRE(const_nt1 == nt2);
    REQUIRE(nt1 == const_nt2);
  }
  SECTION("Multi Type Equality Same Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{1, 2, 3};
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt2{1, 2, 3};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{1, 2, 3};
    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt2{1, 2, 3};

    REQUIRE(nt1 == nt2);
    REQUIRE(const_nt1 == const_nt2);
    REQUIRE(const_nt1 == nt2);
    REQUIRE(nt1 == const_nt2);
  }
  SECTION("Multi Type Equality Different Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{1, 2, 3};
    mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key1", int>>
        nt2{3, 2, 1};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{1, 2, 3};
    const mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key1", int>>
        const_nt2{3, 2, 1};

    REQUIRE(nt1 == nt2);
    REQUIRE(const_nt1 == const_nt2);
    REQUIRE(const_nt1 == nt2);
    REQUIRE(nt1 == const_nt2);
  }

  SECTION("Single Element Less Than") {
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt1{1};
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt2{2};

    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt1{1};
    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt2{2};

    REQUIRE(nt1 < nt2);
    REQUIRE(const_nt1 < const_nt2);
    REQUIRE(const_nt1 < nt2);
    REQUIRE(nt1 < const_nt2);
  }
  SECTION("Single Element Greater Than") {
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt1{2};
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt2{1};

    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt1{2};
    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt2{1};

    REQUIRE(nt1 > nt2);
    REQUIRE(const_nt1 > const_nt2);
    REQUIRE(const_nt1 > nt2);
    REQUIRE(nt1 > const_nt2);
  }
  SECTION("Multi Type Less Than Same Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{1, 1, 1};
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt2{2, 2, 2};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{1, 1, 1};
    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt2{2, 2, 2};

    REQUIRE(nt1 < nt2);
    REQUIRE(const_nt1 < const_nt2);
    REQUIRE(const_nt1 < nt2);
    REQUIRE(nt1 < const_nt2);
  }

  SECTION("Multi Type Less Than Different Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{1, 1, 1};
    mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key1", int>>
        nt2{2, 2, 2};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{1, 1, 1};
    const mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key1", int>>
        const_nt2{2, 2, 2};

    REQUIRE(nt1 < nt2);
    REQUIRE(const_nt1 < const_nt2);
    REQUIRE(const_nt1 < nt2);
    REQUIRE(nt1 < const_nt2);
  }

  SECTION("Multi Type Greater Than Same Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{2, 2, 2};
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt2{1, 1, 1};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{2, 2, 2};
    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt2{1, 1, 1};

    REQUIRE(nt1 > nt2);
    REQUIRE(const_nt1 > const_nt2);
    REQUIRE(const_nt1 > nt2);
    REQUIRE(nt1 > const_nt2);
  }

  SECTION("Multi Type Greater Than Different Order") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{2, 2, 2};
    mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key1", int>>
        nt2{1, 1, 1};

    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{2, 2, 2};
    const mguid::NamedTuple<mguid::NamedType<"key3", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key1", int>>
        const_nt2{1, 1, 1};

    REQUIRE(nt1 > nt2);
    REQUIRE(const_nt1 > const_nt2);
    REQUIRE(const_nt1 > nt2);
    REQUIRE(nt1 > const_nt2);
  }

  SECTION("Empty Equality With Std Tuple") {
    mguid::NamedTuple<> nt1;
    const mguid::NamedTuple<> const_nt1;

    std::tuple<> t1;
    const std::tuple<> const_t1;

    REQUIRE(nt1 == t1);
    REQUIRE(const_nt1 == const_t1);
    REQUIRE(nt1 == const_t1);
    REQUIRE(const_nt1 == t1);
  }

  SECTION("Single Element Equality With Std Tuple") {
    mguid::NamedTuple<mguid::NamedType<"key", int>> nt1{1};
    const mguid::NamedTuple<mguid::NamedType<"key", int>> const_nt1{1};

    std::tuple<int> t1{1};
    const std::tuple<int> const_t1{1};

    REQUIRE(nt1 == t1);
    REQUIRE(const_nt1 == const_t1);
    REQUIRE(nt1 == const_t1);
    REQUIRE(const_nt1 == t1);
  }

  SECTION("Multi Element Equality With Std Tuple") {
    mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                      mguid::NamedType<"key3", int>>
        nt1{1, 2, 3};
    const mguid::NamedTuple<mguid::NamedType<"key1", int>, mguid::NamedType<"key2", int>,
                            mguid::NamedType<"key3", int>>
        const_nt1{1, 2, 3};

    std::tuple<int, int, int> t1{1, 2, 3};
    const std::tuple<int, int, int> const_t1{1, 2, 3};

    REQUIRE(nt1 == t1);
    REQUIRE(const_nt1 == const_t1);
    REQUIRE(nt1 == const_t1);
    REQUIRE(const_nt1 == t1);
  }
}

TEST_CASE("Specialization of std::tuple_element") {
  SECTION("Single Element") {
    REQUIRE(
        std::is_same_v<std::tuple_element_t<0, mguid::NamedTuple<mguid::NamedType<"test", float>>>,
                       float>);
    REQUIRE(
        std::is_same_v<std::tuple_element_t<0, mguid::NamedTuple<mguid::NamedType<"test", int>>>,
                       int>);
  }
  SECTION("Multiple Elements") {
    REQUIRE(
        std::is_same_v<std::tuple_element_t<0, mguid::NamedTuple<mguid::NamedType<"test1", float>,
                                                                 mguid::NamedType<"test2", char>,
                                                                 mguid::NamedType<"test3", int>>>,
                       float>);
    REQUIRE(
        std::is_same_v<std::tuple_element_t<1, mguid::NamedTuple<mguid::NamedType<"test1", float>,
                                                                 mguid::NamedType<"test2", char>,
                                                                 mguid::NamedType<"test3", int>>>,
                       char>);
    REQUIRE(
        std::is_same_v<std::tuple_element_t<2, mguid::NamedTuple<mguid::NamedType<"test1", float>,
                                                                 mguid::NamedType<"test2", char>,
                                                                 mguid::NamedType<"test3", int>>>,
                       int>);
  }
}

TEST_CASE("Specialization of std::tuple_size") {
  SECTION("Empty") { REQUIRE(std::tuple_size_v<mguid::NamedTuple<>> == 0); }
  SECTION("Single Element") {
    REQUIRE(std::tuple_size_v<mguid::NamedTuple<mguid::NamedType<"test", int>>> == 1);
  }
  SECTION("Multiple Elements") {
    REQUIRE(std::tuple_size_v<
                mguid::NamedTuple<mguid::NamedType<"test1", int>, mguid::NamedType<"test2", int>,
                                  mguid::NamedType<"test3", int>>> == 3);
  }
}

TEST_CASE("Forward As Tuple") {
  int i{1};
  int j{2};
  int k{3};

  STATIC_REQUIRE(
      std::is_same_v<
          decltype(mguid::forward_as_tuple(mguid::NamedTypeV<"first">(i),
                                           mguid::NamedTypeV<"second">(j),
                                           mguid::NamedTypeV<"third">(k))),
          mguid::NamedTuple<mguid::NamedType<"first", int&&>, mguid::NamedType<"second", int&&>,
                            mguid::NamedType<"third", int&&>>>);

  [](auto named_tuple) {
    REQUIRE(named_tuple.template get<"first">() == 1);
    REQUIRE(named_tuple.template get<"second">() == 2);
    REQUIRE(named_tuple.template get<"third">() == 3);
  }(mguid::forward_as_tuple(mguid::NamedTypeV<"first">(i), mguid::NamedTypeV<"second">(j),
                            mguid::NamedTypeV<"third">(k)));
}

TEST_CASE("Make Tuple") {
  int i{1};
  int j{2};
  int k{3};

  STATIC_REQUIRE(
      std::is_same_v<
          decltype(mguid::make_tuple(mguid::NamedTypeV<"first">(i), mguid::NamedTypeV<"second">(j),
                                     mguid::NamedTypeV<"third">(k))),
          mguid::NamedTuple<mguid::NamedType<"first", int>, mguid::NamedType<"second", int>,
                            mguid::NamedType<"third", int>>>);

  STATIC_REQUIRE(
      std::is_same_v<
          decltype(mguid::make_tuple(mguid::NamedTypeV<"first">(std::ref(i)),
                                     mguid::NamedTypeV<"second">(std::ref(j)),
                                     mguid::NamedTypeV<"third">(std::ref(k)))),
          mguid::NamedTuple<mguid::NamedType<"first", int&>, mguid::NamedType<"second", int&>,
                            mguid::NamedType<"third", int&>>>);

  [](auto named_tuple) {
    REQUIRE(named_tuple.template get<"first">() == 1);
    REQUIRE(named_tuple.template get<"second">() == 2);
    REQUIRE(named_tuple.template get<"third">() == 3);
  }(mguid::make_tuple(mguid::NamedTypeV<"first">(i), mguid::NamedTypeV<"second">(j),
                      mguid::NamedTypeV<"third">(k)));
}

TEST_CASE("Apply") {
  SECTION("Empty Apply") {
    mguid::NamedTuple<> nt;
    const mguid::NamedTuple<> const_nt;

    mguid::apply([]() {}, nt);
    mguid::apply([]() {}, const_nt);

    REQUIRE(mguid::apply([]([[maybe_unused]] auto&&... args) { return sizeof...(args); }, nt) == 0);
    REQUIRE(mguid::apply([]([[maybe_unused]] auto&&... args) { return sizeof...(args); },
                         const_nt) == 0);
  }
  SECTION("Single Element Apply") {
    mguid::NamedTuple<mguid::NamedType<"test", int>> nt{42};
    const mguid::NamedTuple<mguid::NamedType<"test", int>> const_nt{42};

    mguid::apply(
        []([[maybe_unused]] const auto&& arg) {
          const auto& [first, second] = arg;
          REQUIRE(first.view() == "test");
          REQUIRE(second == 42);
        },
        nt);
    mguid::apply(
        []([[maybe_unused]] const auto&& arg) {
          const auto& [first, second] = arg;
          REQUIRE(first.view() == "test");
          REQUIRE(second == 42);
        },
        const_nt);
  }
  SECTION("Multi Element Apply") {
    mguid::NamedTuple<mguid::NamedType<"test1", int>, mguid::NamedType<"test2", int>, mguid::NamedType<"test3", int>> nt{1, 2, 3};
    const mguid::NamedTuple<mguid::NamedType<"test1", int>, mguid::NamedType<"test2", int>, mguid::NamedType<"test3", int>> const_nt{1, 2, 3};

    mguid::apply(
        []([[maybe_unused]] const auto&& arg1, const auto&& arg2, const auto&& arg3) {
          const auto& [first1, second1] = arg1;
          REQUIRE(first1.view() == "test1");
          REQUIRE(second1 == 1);
          const auto& [first2, second2] = arg2;
          REQUIRE(first2.view() == "test2");
          REQUIRE(second2 == 2);
          const auto& [first3, second3] = arg3;
          REQUIRE(first3.view() == "test3");
          REQUIRE(second3 == 3);
        },
        nt);
    mguid::apply(
        []([[maybe_unused]] const auto&& arg1, const auto&& arg2, const auto&& arg3) {
          const auto& [first1, second1] = arg1;
          REQUIRE(first1.view() == "test1");
          REQUIRE(second1 == 1);
          const auto& [first2, second2] = arg2;
          REQUIRE(first2.view() == "test2");
          REQUIRE(second2 == 2);
          const auto& [first3, second3] = arg3;
          REQUIRE(first3.view() == "test3");
          REQUIRE(second3 == 3);
        },
        const_nt);
  }
}
