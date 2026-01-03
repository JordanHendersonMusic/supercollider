#include <boost/test/unit_test.hpp>
#include <cstdint>

#include "PyrSlot.h"
#include "PyrObject.h"
#include "PyrKernel.h"

BOOST_AUTO_TEST_CASE(slot_test) {
    {
        PyrSlot i = PyrSlot::make(static_cast<int32_t>(32));
        BOOST_TEST_REQUIRE(i.isInt());
        BOOST_TEST_REQUIRE(!i.isPtr());
        BOOST_TEST_REQUIRE(!i.isDouble());
        BOOST_TEST_REQUIRE(!i.isObjectHdr());
        BOOST_TEST_REQUIRE(!i.isNil());
        BOOST_TEST_REQUIRE(!i.isTrue());
        BOOST_TEST_REQUIRE(!i.isFalse());
        BOOST_TEST_REQUIRE(!i.isChar());
        BOOST_TEST_REQUIRE(i.getInt() == 32);
    }
    {
        PyrSlot i = PyrSlot::make(static_cast<char>(52));
        BOOST_TEST_REQUIRE(!i.isInt());
        BOOST_TEST_REQUIRE(!i.isPtr());
        BOOST_TEST_REQUIRE(!i.isDouble());
        BOOST_TEST_REQUIRE(!i.isObjectHdr());
        BOOST_TEST_REQUIRE(!i.isNil());
        BOOST_TEST_REQUIRE(!i.isTrue());
        BOOST_TEST_REQUIRE(!i.isFalse());
        BOOST_TEST_REQUIRE(i.isChar());
        BOOST_TEST_REQUIRE(i.getChar() == 52);
    }
    {
        int a = 10;
        int* ap = &a;
        PyrSlot s_p = PyrSlot::make(ap);
        BOOST_TEST_REQUIRE(s_p.isPtr());
        BOOST_TEST_REQUIRE(!s_p.isDouble());
        BOOST_TEST_REQUIRE(!s_p.isSymbol());
        BOOST_TEST_REQUIRE(!s_p.isObjectHdr());
        BOOST_TEST_REQUIRE(!s_p.isInt());
        BOOST_TEST_REQUIRE(!s_p.isChar());
        BOOST_TEST_REQUIRE(!s_p.isNil());
        BOOST_TEST_REQUIRE(!s_p.isTrue());
        BOOST_TEST_REQUIRE(!s_p.isFalse());
        void* s_pt = s_p.getPtr();
        BOOST_TEST_REQUIRE(s_pt == ap);
    }
    {
        PyrObject o;
        PyrSlot s = PyrSlot::make(&o);
        BOOST_TEST_REQUIRE(s.isObjectHdr());
        BOOST_TEST_REQUIRE(!s.isPtr());
        BOOST_TEST_REQUIRE(!s.isInt());
        BOOST_TEST_REQUIRE(!s.isChar());
        BOOST_TEST_REQUIRE(!s.isNil());
        BOOST_TEST_REQUIRE(!s.isTrue());
        BOOST_TEST_REQUIRE(!s.isFalse());
        BOOST_TEST_REQUIRE(s.getObjectHdr() == &o);
    }
    {
        PyrBlock b;
        PyrSlot s = PyrSlot::make(&b);
        BOOST_TEST_REQUIRE(s.isObjectHdr());
        BOOST_TEST_REQUIRE(!s.isPtr());
        BOOST_TEST_REQUIRE(!s.isInt());
        BOOST_TEST_REQUIRE(!s.isChar());
        BOOST_TEST_REQUIRE(!s.isNil());
        BOOST_TEST_REQUIRE(!s.isTrue());
        BOOST_TEST_REQUIRE(!s.isFalse());
        BOOST_TEST_REQUIRE(s.getObjectHdr() == &b);
    }
    {
        PyrSlot s = PyrSlot::make((PyrBlock*)nullptr);
        BOOST_TEST_REQUIRE(s.isObjectHdr());
        BOOST_TEST_REQUIRE(!s.isPtr());
        BOOST_TEST_REQUIRE(!s.isInt());
        BOOST_TEST_REQUIRE(!s.isChar());
        BOOST_TEST_REQUIRE(!s.isNil());
        BOOST_TEST_REQUIRE(!s.isTrue());
        BOOST_TEST_REQUIRE(!s.isFalse());
        BOOST_TEST_REQUIRE(s.getObjectHdr() == nullptr);
    }
    // clang-format off
#ifndef _MSC_VER
    // Cannot divide by zero in constexpr on MSVC
    {
        const auto r = PyrSlot::make(1.0 / 0.0);
        BOOST_TEST_REQUIRE(r.isDouble());
        BOOST_TEST_REQUIRE(r.getDouble() == 1.0 / 0.0);
    }
    {
        const auto r = PyrSlot::make(-1.0 / 0.0);
        BOOST_TEST_REQUIRE(r.isDouble());
        BOOST_TEST_REQUIRE(r.getDouble() == -1.0 / 0.0);
    }
    {
        const auto r = PyrSlot::make(-0.0 / 0.0);
        BOOST_TEST_REQUIRE(r.isDouble());
        const auto d = r.getDouble();
        BOOST_TEST_REQUIRE(std::isnan(r.getDouble()));
    }
    {
        const auto r = PyrSlot::make(0.0 / 0.0);
        BOOST_TEST_REQUIRE(r.isDouble());
        BOOST_TEST_REQUIRE(std::isnan(r.getDouble()));
    }
#endif
    // clang-format on
}

BOOST_AUTO_TEST_CASE(nan_tests) {
    const auto safe_nan_double = details::bit_cast<double>(details::safeNaN);
    BOOST_TEST(std::isnan(safe_nan_double));

    BOOST_TEST(removeBadNans(1.0) == 1.0);
    BOOST_TEST(removeBadNans(-1.0) == -1.0);

    // This also tests bit_cast.
    {
        // safeNan is the first quiet nan.
        const uint64_t d1 = details::bit_cast<uint64_t>(std::nan("1"));
        BOOST_TEST(d1 == details::safeNaN);

        // all other nans are not allowed.
        const uint64_t d2 = details::bit_cast<uint64_t>(std::nan("2"));
        BOOST_TEST(d2 != details::safeNaN);
    }
    {
        const auto r = removeBadNans(safe_nan_double);
        BOOST_TEST(std::isnan(r));
    }
    const auto test_quiet_nan = [](const char* s) {
        const auto nan = std::nan(s);
        const auto r = removeBadNans(nan);
        const uint64_t r_uint = details::bit_cast<uint64_t>(r);
        // r should still be a nan
        BOOST_TEST(std::isnan(r));

        // r should not be the same nan
        BOOST_TEST(r_uint != details::bit_cast<uint64_t>(nan));

        // r should be a the safe nan
        BOOST_TEST(r_uint == details::bit_cast<uint64_t>(details::safeNaN));

        const auto s_nan = PyrSlot::make<AssertDouble::CouldBeBadNan>(nan);
        BOOST_TEST(s_nan.isDouble());
        BOOST_TEST(std::isnan(s_nan.getDouble()));
    };

    // Just some random quiet nans. These should all be converted to the safe nan.
    test_quiet_nan("2");
    test_quiet_nan("3");
    test_quiet_nan("33456344");
    test_quiet_nan("93563455656");
}
