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


TEST_CASE("TaggedBitset Test") {
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb{0b1010};
  const mguid::TaggedBitset<"key1", "key2", "key3", "key4"> const_tb{0b1010};
  SECTION("Test") {
    REQUIRE(tb.test<"key1">());
    REQUIRE(!tb.test<"key2">());
    REQUIRE(tb.test<"key3">());
    REQUIRE(!tb.test<"key4">());

    REQUIRE(const_tb.test<"key1">());
    REQUIRE(!const_tb.test<"key2">());
    REQUIRE(const_tb.test<"key3">());
    REQUIRE(!const_tb.test<"key4">());

    tb.flip<"key1">();
    tb.flip<"key2">();
    tb.flip<"key3">();
    tb.flip<"key4">();

    REQUIRE(tb.test<"key1">() == 0);
    REQUIRE(tb.test<"key2">() == 1);
    REQUIRE(tb.test<"key3">() == 0);
    REQUIRE(tb.test<"key4">() == 1);
  }
}

TEST_CASE("TaggedBitset Equality Comparison") {
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb1{0b1010};
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb2{0b1010};
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb3{0b0101};

  SECTION("Equality operator with same TaggedBitset") {
    REQUIRE(tb1 == tb2);
    REQUIRE(!(tb1 == tb3));
  }

  SECTION("Equality operator with std::bitset") {
    std::bitset<4> std_bs{0b1010};
    REQUIRE(tb1 == std_bs);

    std::bitset<4> std_bs_diff{0b0101};
    REQUIRE(!(tb1 == std_bs_diff));
  }
}

TEST_CASE("TaggedBitset Reset") {
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb{0b1111};

  SECTION("Reset individual bit") {
    tb.reset<"key1">();
    REQUIRE(!tb.test<"key1">());
    REQUIRE(tb.test<"key2">());
    REQUIRE(tb.test<"key3">());
    REQUIRE(tb.test<"key4">());
  }

  SECTION("Reset all bits") {
    tb.reset();
    REQUIRE(!tb.test<"key1">());
    REQUIRE(!tb.test<"key2">());
    REQUIRE(!tb.test<"key3">());
    REQUIRE(!tb.test<"key4">());
  }
}

TEST_CASE("TaggedBitset Flip") {
  SECTION("Flip individual bit") {
    mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb{"0101"};

    tb.flip<"key1">();
    tb.flip<"key2">();

    REQUIRE(tb.test<"key1">());
    REQUIRE(!tb.test<"key2">());
    REQUIRE(!tb.test<"key3">());
    REQUIRE(tb.test<"key4">());
  }

  SECTION("Flip all bits") {
    mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb{0b0101};

    tb.flip();
    REQUIRE(tb.test<"key1">());
    REQUIRE(!tb.test<"key2">());
    REQUIRE(tb.test<"key3">());
    REQUIRE(!tb.test<"key4">());
  }
}

TEST_CASE("TaggedBitset Mixed Operations") {
  mguid::TaggedBitset<"key1", "key2", "key3", "key4"> tb;

  SECTION("Set, test, and reset sequentially") {
    tb.set<"key1">();
    REQUIRE(tb.test<"key1">());

    tb.set<"key2">(false);
    REQUIRE(!tb.test<"key2">());

    tb.set<"key3">(true);
    REQUIRE(tb.test<"key3">());

    tb.reset<"key1">();
    REQUIRE(!tb.test<"key1">());

    tb.flip<"key4">();
    REQUIRE(tb.test<"key4">());
  }
}