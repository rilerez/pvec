#define CATCH_CONFIG_MAIN
#include "pvec.hpp"
#include <catch2/catch.hpp>

TEST_CASE("leq_multiple returns the greatest less-than-or-equal-to multiple of "
          "2^pow_of_2 ") {
  REQUIRE(leq_multiple(1, 0) == 0);
  REQUIRE(leq_multiple(1, 1) == 0);
  REQUIRE(leq_multiple(1, 2) == 2);
  REQUIRE(leq_multiple(1, 3) == 2);
  REQUIRE(leq_multiple(2, 5) == 4);
  REQUIRE(leq_multiple(5, 34) == 32);
}

TEST_CASE("log2 gives floor of log base 2") {
  REQUIRE(log2(1u) == 0);
  REQUIRE(log2(2u) == 1);
  REQUIRE(log2(3u) == 1);
  REQUIRE(log2(4u) == 2);
}

using node_data =
    with_exponent<2>::with_node_data<int, std::shared_ptr, std::size_t>;
using node = node_data::pvec_node;
using pvec = node_data::pvec<std::shared_ptr>;
using maker = shared_ptr_node_maker<node>;
TEST_CASE("conj to a pvec") {
  pvec x = {};
  SECTION("A pvec is initially empty") { REQUIRE(x.empty() == true); }
  auto pushed = conj(maker{}, x, 3);
  SECTION("Conj increases size by 1 ") { REQUIRE(pushed.size() == 1); }
  SECTION("Can access the data") { REQUIRE(pushed[0] == 3); }
  SECTION("Can add handful elements") {
    auto myconj = [](auto vec, auto elt) { return conj(maker{}, vec, elt); };
    auto pushed_many = myconj(
        myconj(myconj(myconj(myconj(myconj(myconj(x, 0), 1), 2), 3), 4), 5),
        6);
    REQUIRE(pushed_many[0] == 0);
    REQUIRE(pushed_many[3] == 3);
    REQUIRE(pushed_many[6] == 6);
  }
}
