#include "Named/TaggedBitset.hpp"

#include <catch2/catch_all.hpp>

#include <cstdint>
#include <string>

TEST_CASE("TaggedBitset Constructor") {
  SECTION("Empty") {
    mguid::TaggedBitset<> tb;
    REQUIRE(tb.size() == 0);
    const mguid::TaggedBitset<> const_tb;
    REQUIRE(const_tb.size() == 0);
  }
  SECTION("Single Bit") {
    mguid::TaggedBitset<"key1"> tb;
    REQUIRE(tb.size() == 1);

    const mguid::TaggedBitset<"key1"> const_tb;
    REQUIRE(const_tb.size() == 1);
  }
  SECTION("Single Bit") {
    [[maybe_unused]] mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                         "key8"> tb;
    REQUIRE(tb.size() == 8);

    const mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                     "key8"> const_tb;
    REQUIRE(const_tb.size() == 8);
  }
  SECTION("From Binary Literal") {
    mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                         "key8"> tb{0b10101010};
    REQUIRE(tb.size() == 8);
    REQUIRE(tb.count() == 4);

    const mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                     "key8"> const_tb{0b10101010};
    REQUIRE(const_tb.size() == 8);
    REQUIRE(const_tb.count() == 4);
  }
  SECTION("From String") {
    mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                         "key8"> tb{"10101010"};
    REQUIRE(tb.size() == 8);
    REQUIRE(tb.count() == 4);

    const mguid::TaggedBitset<"key1", "key2", "key3", "key4", "key5", "key6", "key7",
                                     "key8"> const_tb{"10101010"};
    REQUIRE(const_tb.size() == 8);
    REQUIRE(const_tb.count() == 4);
  }
}

TEST_CASE("TaggedBitset Set") {
  SECTION("Set") {
    mguid::TaggedBitset<"key"> tb;
    tb.set<"key">();
    REQUIRE(tb.test<"key">());
    tb.set<"key">(false);
    REQUIRE(!tb.test<"key">());
  }
}