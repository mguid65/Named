#include "Named/TaggedArray.hpp"

#include <catch2/catch_all.hpp>

#include <string>

TEST_CASE("TaggedArray Constructor") {
  SECTION("Empty Constructor") {
    mguid::TaggedArray<int, "key1", "key2"> arr{};
    REQUIRE(arr.size() == 2);
    REQUIRE(arr.at<"key1">() == 0);
    REQUIRE(arr.at<"key2">() == 0);
  }

  SECTION("Constructor with Values") {
    mguid::TaggedArray<int, "key1", "key2"> arr{1, 2};
    REQUIRE(arr.at<"key1">() == 1);
    REQUIRE(arr.at<"key2">() == 2);
  }
}

TEST_CASE("TaggedArray Access") {
  mguid::TaggedArray<std::string, "name", "type"> arr{"Alice", "Admin"};

  SECTION("Access by Tag") {
    REQUIRE(arr.at<"name">() == "Alice");
    REQUIRE(arr.at<"type">() == "Admin");
  }

  SECTION("Const Access by Tag") {
    const auto& const_arr = arr;
    REQUIRE(const_arr.at<"name">() == "Alice");
    REQUIRE(const_arr.at<"type">() == "Admin");
  }
}

TEST_CASE("TaggedArray Set") {
  mguid::TaggedArray<int, "key1", "key2"> arr{0, 0};

  SECTION("Set by Tag") {
    arr.set<"key1">(10);
    arr.set<"key2">(20);
    REQUIRE(arr.at<"key1">() == 10);
    REQUIRE(arr.at<"key2">() == 20);
  }
}

TEST_CASE("TaggedArray Comparison") {
  mguid::TaggedArray<int, "key1", "key2"> arr1{1, 2};
  mguid::TaggedArray<int, "key1", "key2"> arr2{1, 2};
  mguid::TaggedArray<int, "key1", "key2"> arr3{2, 3};

  SECTION("TaggedArray Equality") {
    REQUIRE(arr1 == arr2);
    REQUIRE(!(arr1 == arr3));
  }

  SECTION("Comparison with std::array") {
    std::array<int, 2> std_arr{1, 2};
    REQUIRE(arr1 == std_arr);

    std::array<int, 2> std_arr_diff{2, 3};
    REQUIRE(!(arr1 == std_arr_diff));
  }
}

TEST_CASE("TaggedArray Tags") {
  using ArrayType = mguid::TaggedArray<int, "key1", "key2">;

  SECTION("Tags Reflection") {
    constexpr auto tags = ArrayType::tags();
    REQUIRE(std::get<0>(tags) == "key1");
    REQUIRE(std::get<1>(tags) == "key2");
  }
}

TEST_CASE("TaggedArray Mixed Operations") {
  mguid::TaggedArray<int, "key1", "key2", "key3"> arr{0, 0, 0};

  SECTION("Set, Access, and Compare Sequentially") {
    arr.set<"key1">(100);
    arr.set<"key2">(200);
    arr.set<"key3">(300);

    REQUIRE(arr.at<"key1">() == 100);
    REQUIRE(arr.at<"key2">() == 200);
    REQUIRE(arr.at<"key3">() == 300);

    mguid::TaggedArray<int, "key1", "key2", "key3"> arr_copy{100, 200, 300};
    REQUIRE(arr == arr_copy);
  }
}

TEST_CASE("std::tuple_size for TaggedArray") {
  using ArrayType = mguid::TaggedArray<int, "x", "y", "z">;
  REQUIRE(std::tuple_size<ArrayType>::value == 3);
}

// Test that std::tuple_element retrieves the correct type for each index in TaggedArray
TEST_CASE("std::tuple_element for TaggedArray") {
  using ArrayType = mguid::TaggedArray<int, "x", "y", "z">;

  // Check that each index has the correct type (in this case, all elements are int)
  STATIC_REQUIRE(std::is_same_v<std::tuple_element<0, ArrayType>::type, int>);
  STATIC_REQUIRE(std::is_same_v<std::tuple_element<1, ArrayType>::type, int>);
  STATIC_REQUIRE(std::is_same_v<std::tuple_element<2, ArrayType>::type, int>);
}

// Test that accessing elements with std::get works with tuple_element
TEST_CASE("std::get with tuple_element for TaggedArray") {
  mguid::TaggedArray<int, "x", "y", "z"> arr{1, 2, 3};

  // Check access using std::get
  REQUIRE(std::get<0>(arr) == 1);
  REQUIRE(std::get<1>(arr) == 2);
  REQUIRE(std::get<2>(arr) == 3);

  // Check modifying elements with std::get
  std::get<1>(arr) = 42;
  REQUIRE(std::get<1>(arr) == 42);
}