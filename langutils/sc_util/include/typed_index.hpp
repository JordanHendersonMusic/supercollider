// Copyright Jordan Henderson 2026
#pragma once

#include <array>
#include <cstddef>
#include <limits>
#include <cassert>
#include <optional>
#include <type_traits>

/*
The heart of this is an index type that has a compile time set of enum values associated with it.
If one enum set is a subset of the other, it can implicitly convert to it, this is checked at compile time.
Of course, C++ makes this simple concept hard to read, but see the conversion operator in the TypedIndexHelper, this is
where the main logic is applied.

This is mainly useful in graphs where a node of type T has children of types A, B, or C, i.e., a subset of the entire
node types. To resolve an index which represents multiple types into a single type, this must be done at run time.
Mostly likely by checking the variant/tagged-union to lookup the type.

Consider these two typed index sets. A is a subset of B, so it can implicitly convert to a B.
enum struct E { X, Y , Z };
using A = TypedIndex<E::X>;
using B = TypedIndex<E::X, E::Y, E::Z>;

A a{...};
B b {a}; // << converts implicitly

B b{...};
A a{b}; // << compile time error, 'B' is superset of 'A', *not* a subset!

Additionally, some helper functions (using templates) are provided to construct these sets by combining them together.

If you add a new feature, **please** ensure you write a compile time test.
This code is complicated and daunting to those who have not yet had the misfortune of doing C++ meta programming.
The tests (at the end of the file) also serve as documentation, but here is a basic use.


1. Declare an enum set

enum struct Elements { X, Y, Z };

2. Declare a Spec

using MySpec = Spec<
    std::uint32_t, // index type
    Elements, // enum set
    struct MyGraphTag // some type (can be unimplemented) that prevents conversions between similar indexes
>;

3. Define types, using helper

// Defines them
using MyDef = QuicklyDefineTypesFromSpec<MySpec>;

// Create similar using decls.

using Index = MyDef::Index;
using OptionalIndex = MyDef::OptionalIndex;

template<Elements...Es>
using TypedIndex = MyDef::TypedIndex<Es...>;

template<Elements...Es>
using OptionalTypedIndex = MyDef::OptionalTypedIndex<Es...>;

4. Define index types as needed

using X_Index = TypedIndex<Elements::X>;
using Y_Index = TypedIndex<Elements::Y>;

// Can merge sets with 'join'
using XY_Index = join<X_Index, Y_Index>;

// Or by naming the elements directly
using YZ_Index = TypedIndex<Elements::Y, Elements::Z>;

5. Construct indexes

auto x = X_Index{1};

XY_Index xy {x};

std::uint32_t xy_value {*xy};

YZ_Index yz {xy}; // Compile time error, xy is not a subset of yz.

*/

namespace sc::util::typed_index {

// This should be passed to every single template below.
template <typename INDEX_TYPE, typename ENUM_SET, typename GraphTypeID = void> struct Spec {
    using IndexType = INDEX_TYPE;
    using EnumSet = ENUM_SET;
    // By providing a unique GraphID from each graph type, we ensure that indexes from different types of graph cannot
    // convert. This does allow graphs of the same type to convert, so it is recommend to 'tag' the indexes with a graph
    // instance ID if the owning graph is ambiguous.
    using ID = GraphTypeID;
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Indexs
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename SPEC> struct IndexT {
    using IndexType = typename SPEC::IndexType;
    constexpr IndexT(IndexType v) noexcept: m_value(v) {}
    constexpr IndexT() = delete;

    constexpr IndexT(IndexT&&) noexcept = default;
    constexpr IndexT(const IndexT&) noexcept = default;
    constexpr IndexT& operator=(IndexT&&) noexcept = default;
    constexpr IndexT& operator=(const IndexT&) noexcept = default;

    [[nodiscard]] constexpr IndexType value() const noexcept { return m_value; }
    [[nodiscard]] constexpr IndexType operator*() const noexcept { return m_value; }

private:
    IndexType m_value;
};

template <typename SPEC> struct OptionalIndexT {
public:
    using IndexType = typename SPEC::IndexType;
    static constexpr IndexType invalid_value { std::numeric_limits<IndexType>::max() };

    constexpr OptionalIndexT(IndexType v) noexcept: m_value(v) {}
    constexpr OptionalIndexT() noexcept: m_value(invalid_value) {}
    constexpr OptionalIndexT(IndexT<SPEC> i) noexcept: m_value(*i) {}

    constexpr OptionalIndexT(OptionalIndexT&&) noexcept = default;
    constexpr OptionalIndexT(const OptionalIndexT&) noexcept = default;
    constexpr OptionalIndexT& operator=(OptionalIndexT&&) noexcept = default;
    constexpr OptionalIndexT& operator=(const OptionalIndexT&) noexcept = default;


    [[nodiscard]] constexpr explicit operator bool() const noexcept { return m_value != invalid_value; }

    // The value() function is checked, but the de-ref isn't.
    [[nodiscard]] constexpr IndexType value() const noexcept {
        assert(*this);
        return m_value;
    }
    [[nodiscard]] constexpr IndexType operator*() const noexcept { return m_value; }


private:
    IndexType m_value;
};


template <typename SPEC> [[nodiscard]] inline constexpr OptionalIndexT<SPEC> as_optional(IndexT<SPEC> i) noexcept {
    return { *i };
}

template <typename SPEC>
[[nodiscard]] inline constexpr std::optional<IndexT<SPEC>> as_index(OptionalIndexT<SPEC> i) noexcept {
    return i ? std::optional<IndexT<SPEC>> { *i } : std::nullopt;
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Typed Index Details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace details {

template <typename Enum, Enum ToCheck, Enum... ElementsInSet> [[nodiscard]] inline constexpr bool enum_set_includes() {
    return ((ToCheck == ElementsInSet) || ...);
}

template <typename SPEC, typename INDEX_BASE, typename SPEC::EnumSet... ThisSet>
struct TypedIndexHelper : public INDEX_BASE {
    using EnumSet = typename SPEC::EnumSet;
    static_assert(sizeof...(ThisSet) > 0, "Must provide at least one 'Type' per TypedIndex.");
    static constexpr auto Possible { std::array<EnumSet, sizeof...(ThisSet)> { ThisSet... } };

    using INDEX_BASE::INDEX_BASE;
    using INDEX_BASE::operator*;

    template <EnumSet... OtherSet> [[nodiscard]] static constexpr bool is_sub_set_of() {
        return ((details::enum_set_includes<EnumSet, ThisSet, OtherSet...>()) && ...);
    }

    // This does the conversion, it looks complicate, but it is just an `operator T()`.
    // We have to do the conversion check in SFINAE rather than a static assert because we want to delete the function,
    //    this means it will work with things like std::variant assignments.
    template <EnumSet... OtherSet, typename = std::enable_if_t<is_sub_set_of<OtherSet...>()>>
    [[nodiscard]] constexpr operator TypedIndexHelper<SPEC, INDEX_BASE, OtherSet...>() const noexcept {
        return TypedIndexHelper<SPEC, INDEX_BASE, OtherSet...> { **this };
    }
};

} // details

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper Function Details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace details::convertible {
template <typename L, typename R> struct Convertible;

template <typename SPEC, typename INDEX_BASE, typename SPEC::EnumSet... A, typename SPEC::EnumSet... B>
struct Convertible<details::TypedIndexHelper<SPEC, INDEX_BASE, A...>,
                   details::TypedIndexHelper<SPEC, INDEX_BASE, B...>> {
    using L = details::TypedIndexHelper<SPEC, INDEX_BASE, A...>;
    static constexpr bool value { L::template is_sub_set_of<B...>() };
};

} // details::convertible

namespace details::join {

template <typename L, typename R> struct JoinTwo;

template <typename SPEC, typename INDEX_BASE, typename SPEC::EnumSet... A, typename SPEC::EnumSet... B>
struct JoinTwo<details::TypedIndexHelper<SPEC, INDEX_BASE, A...>, details::TypedIndexHelper<SPEC, INDEX_BASE, B...>> {
    using Result = details::TypedIndexHelper<SPEC, INDEX_BASE, A..., B...>;
};

template <typename L, typename R> using join_two = typename JoinTwo<L, R>::Result;

// Turns JoinSingle into the '+' operator for fold expressions
template <class T> struct JoinTwoAsBinaryOp {
    using Type = T;
    template <class O> [[nodiscard]] constexpr auto operator+(O) {
        return JoinTwoAsBinaryOp<join_two<T, typename O::Type>> { };
    }
};

template <typename... ToJoin> [[nodiscard]] inline constexpr auto join_all() {
    return (JoinTwoAsBinaryOp<ToJoin> { } + ...);
}

} // details::join


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper Functions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename... ToJoin> using join = typename decltype(details::join::join_all<ToJoin...>())::Type;

template <typename From, typename To> [[nodiscard]] inline constexpr bool convertible() {
    return details::convertible::Convertible<From, To>::value;
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Typed Indexs
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename SPEC, typename SPEC::EnumSet... ThisSet>
using TypedIndex = details::TypedIndexHelper<SPEC, IndexT<SPEC>, ThisSet...>;

template <typename SPEC, typename SPEC::EnumSet... ThisSet>
using OptionalTypedIndex = details::TypedIndexHelper<SPEC, OptionalIndexT<SPEC>, ThisSet...>;

template <typename SPEC, typename SPEC::EnumSet... Set>
[[nodiscard]] auto as_optional(TypedIndex<SPEC, Set...> t) noexcept -> OptionalTypedIndex<SPEC, Set...> {
    return OptionalTypedIndex<SPEC, Set...> { *t };
}

template <typename SPEC, typename SPEC::EnumSet... Set>
[[nodiscard]] auto as_index(OptionalTypedIndex<SPEC, Set...> t) noexcept -> std::optional<TypedIndex<SPEC, Set...>> {
    return t ? std::optional<TypedIndex<SPEC, Set...>> { *t } : std::nullopt;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Auto define all types given a spec
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename SPEC> struct QuicklyDefineTypesFromSpec {
    using Index = IndexT<SPEC>;
    using OptionalIndex = OptionalIndexT<SPEC>;
    using EnumSet = typename SPEC::EnumSet;

    template <EnumSet... SET> using TypedIndex = TypedIndex<SPEC, SET...>;

    template <EnumSet... SET> using OptionalTypedIndex = OptionalTypedIndex<SPEC, SET...>;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Static tests / Examples
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace details_basic {

enum struct Elements { A, B, C, D };

// size_t is the underlying index type.
// SomeCollectionType is just a tag, it could be anything. Only indexes with the same type here can convert to one
// another.
using Spec = Spec<std::size_t, Elements, struct SomeCollectionType>;

using Defs = QuicklyDefineTypesFromSpec<Spec>;

template <Elements... Set> using TypedIndex = Defs::TypedIndex<Set...>;
template <Elements... Set> using OptionalTypedIndex = Defs::OptionalTypedIndex<Set...>;

using A_Index = TypedIndex<Elements::A>;
using B_Index = TypedIndex<Elements::B>;
using C_Index = TypedIndex<Elements::C>;
using D_Index = TypedIndex<Elements::D>;

using AB_Index = join<A_Index, B_Index>;

using AnyIndex = join<A_Index, B_Index, C_Index, D_Index>;


// Normal index stuff.
static_assert([]() -> bool {
    A_Index a1 { 0 };
    A_Index a2 { 1 };
    return *a1 != *a2;
}());

static_assert([]() -> bool {
    A_Index a1 { 0 };
    A_Index a2 { 0 };
    return *a1 == *a2;
}());

// Conversions
static_assert([]() -> bool {
    A_Index a { 0 };
    AB_Index b { a }; // a is a subset of ab, okay
    // B_index bb{a}; // this is a compile time error
    return *b == 0;
}());
}

namespace details_both_types {

// Elements of the set.
enum struct Elements { A, B, C };

// Helper to define the types.
using Spec = Spec<std::size_t, Elements, struct Test>;

// Test/example code, templated on a typed index or an optionally typed index.
template <template <Elements...> typename IndexType> struct Tester {
    // Define index types that represent only a single element in the set.
    using A_Index = IndexType<Elements::A>;
    using B_Index = IndexType<Elements::B>;
    using C_Index = IndexType<Elements::C>;


    // Define index types that represent sets with many elements.
    // Verbose way.
    using AB_Index = IndexType<Elements::A, Elements::B>;
    // Using the 'join' helper
    using AC_Index = join<A_Index, C_Index>;

    using Any_Index = join<A_Index, B_Index, C_Index>;

    static_assert(convertible<A_Index, AB_Index>());

    static_assert(!convertible<AB_Index, A_Index>());

    static_assert(!convertible<AB_Index, A_Index>());
    static_assert(!convertible<A_Index, C_Index>());

    static_assert(convertible<A_Index, A_Index>());
    static_assert(convertible<AB_Index, AB_Index>());

    static_assert(convertible<A_Index, Any_Index>());
    static_assert(convertible<B_Index, Any_Index>());
    static_assert(convertible<C_Index, Any_Index>());
    static_assert(convertible<AB_Index, Any_Index>());
    static_assert(convertible<Any_Index, Any_Index>());

    static_assert(!convertible<Any_Index, A_Index>());

    static_assert([]() -> bool {
        A_Index a { std::size_t(0) };
        AB_Index ab { a };
        return *ab == 0;
    }());
};

// This contains the definitions of the type erased index and optional index.
using G = QuicklyDefineTypesFromSpec<Spec>;

static constexpr auto test1 { Tester<G::TypedIndex> { } };
static constexpr auto test2 { Tester<G::OptionalTypedIndex> { } };

}

namespace details_graph_conversion_test {
enum struct Elements { X };
using SpecA = Spec<std::size_t, Elements, struct GraphTypeA>;
using SpecB = Spec<std::size_t, Elements, struct GraphTypeB>;

using DefA = QuicklyDefineTypesFromSpec<SpecA>;
using DefB = QuicklyDefineTypesFromSpec<SpecB>;

using AX_I = DefA::TypedIndex<Elements::X>;
using BX_I = DefB::TypedIndex<Elements::X>;

// These don't compile!
// Can only convert from indexes of the same graph
/*
static_assert([]() -> bool {
    AX_I a{1};
    BX_I b{a};
    return true;
}());
static_assert(convertible<AX_I, BX_I>())
*/
}


}
