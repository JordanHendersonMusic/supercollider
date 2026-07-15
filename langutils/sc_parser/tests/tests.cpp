#include "nodes.hpp"
#include "normalise_source.hpp"
#include "sc_parser.hpp"
#include "text_location.hpp"
#include <memory>
#define BOOST_TEST_MODULE sc_parser_tests
#include <boost/test/included/unit_test.hpp>

namespace P = sc ::parser;

const auto tester_base = [](const char* src, bool is_class_file) {
    std::cout << "\n--------------------------------\n src: ";
    std::cout << src << "\n";

    auto text_info = std::shared_ptr<P::TextInfo>(new P::TextInfo {
        sc::lex::NormalisedSource(src),
        sc::lex::FileCodeLocation {},
        "test_file",
        is_class_file,
    });

    auto [graph, result] = P::parse(text_info);
    BOOST_TEST(result == 0);
    namespace N = P::nodes;

    //  graph.flat_walk([&](sc::lex::SourceCodeRange loc, const N::Edges& e, const N::NodeVariant& payload, size_t i) {
    //      std::cout << i << " " << N::NodeCollection::get_name(payload);
    //      std::cout << " ";
    //      std::cout << "parent: " << *e.parent;
    //      std::cout << std::endl;
    //  });

    const auto root = graph.root_any();
    BOOST_TEST(root.has_value());

    for (const auto& d : graph.diagnostics())
        std::cout << d << std::endl;

    graph.depth_first_traverse(*root,
                               [&](const sc::lex::SourceCodeRange& loc, const N::NodeVariant& payload, size_t depth) {
                                   for (size_t di { 0 }; di < depth; ++di)
                                       std::cout << "|  ";

                                   std::cout << N::NodeCollection::get_name(payload) << ": ";

                                   const auto [ptr, sz] = text_info->read(loc);
                                   for (size_t i { 0 }; i < sz; ++i) {
                                       if (ptr[i] == '\n') {
                                           std::cout << "'\\n'";
                                       } else {
                                           std::cout.write(ptr + i, 1);
                                       }
                                   }
                                   std::cout << "\n";
                               });
};

const auto tester = [](const char* src) { tester_base(src, false); };

const auto tester_class = [](const char* src) { tester_base(src, true); };


BOOST_AUTO_TEST_CASE(literals) { tester("\n\n1.2 +.(1 + 1) ('a' *.meow b; 1 + 1.3323 - true ++ \\meow or: $a)\n\n"); }


BOOST_AUTO_TEST_CASE(expr) {
    tester("meow(1, 10, a: 20, *args)");
    tester("meow(*1, 10, a: 20, *a, *b, woof: 75)");

    tester("if(a) {1} {2}");

    tester("if(*a, foo, a: 10) { 'meow' } { 'woof' } ");

    tester("(+)(1, 2)");
    tester("(+)(1, 2) { 'meow'} { 'woof'} ");
    tester("(+)(1, hello: 2) { 'meow'} { 'woof'} ");
    tester("(+) { 'meow'} { 'woof'} ");


    tester("1.meow(arg: 1)");
    tester("1.meow");
    tester("1.if { 'true' } { 'false' }");
    tester("{}.if { 'true' } { 'false' }");


    tester("meow.()");
    tester("meow.() {'cat' } ");


    tester("WOOF.new()");
    tester("WOOF()");
    tester("WOOF{a} {b}");
    tester("WOOF(meow: cat) {a} {b}");
    tester("WOOF(meow: cat, *  ~  ar) {a} {b}");


    tester("~meow");

    tester("FOO");
    tester("`FOO");
    tester("FOO.[1]");

    tester("`a = b = 1 * 2");


    tester("~meow = ~foo = 1 + 1");


    tester("~foo.meow = 10");


    tester("foo(1) = 10");
    tester("foo(1, 2, 3, meow: 10, *a) = 10");


    // This is different from the original!
    // It would not allow you to pass nil
    tester("a[] = 1");
    // Nor would it allow you to use kwargs
    tester("a[meow: 10] = 1");

    tester("a[1] = 1");

    tester("a.[1] = 1");


    tester("[1, 2, 3]");
    tester("#[1, 2, 3]");


    tester("(a: 1)");
    tester("(a : 1)");
    tester("()");
    tester("(1 + 1: 2)");
    tester("(1 + 1 : 2)");

    tester("(1+1)");


    tester("Foo[1: 2]");

    tester("(1+1)[foo: 2]");
    tester("(a: 1, 'b' : meow)[2]");
    tester("foo. + 10 - ~meow");

    tester("{}");

    tester("{ arg a; }");
    tester("{ arg a = 1; }");
    tester("{ arg a(1); }");
    tester("{ arg a(1; 2); }");
    tester("{ arg a=((1; 2);4); }");

    tester("{ arg a=1, b(2), c=(d); }");


    tester("{ |a=1| }");
    tester("{ |a(1)| }");
    tester("{ |a 1| }");

    tester("{ |a=1,| }");
    tester("{ |a(1),| }");
    tester("{ |a 1,| }");

    tester("{ |a=1, b= 2, c=(d)| }");

    tester("{ |a=1, b= 2, c=(d)| 1 + 1 }");
    tester("{ |a=1, b= 2, c=(d)| 1 + 1; a; }");


    tester("{ |a=1, b= 2, c=(d)| ^1 + 1; ^a }");
    tester("{  ^1 + 1; ^a }");
    tester("{ |a| ^if(a) {~meow} {~woof} }");


    tester("{ |a=(1:2)| }");
    tester("{ |a (1:2)| }");
    tester("{ |a ()| }");

    tester("{ |a({})| }");


    tester("{ var a; }");
    tester("{ var a = 10; }");
    tester("{ var a = 10, b(20); }");
    tester("{ |b| 1 + 1; var a }");
}


BOOST_AUTO_TEST_CASE(classes) {
    tester_class("Foo {}");
    tester_class("+Foo {}");
    tester_class("+Foo{} Bar{} +Car{}");

    tester_class("Foo : Bar {}");

    tester_class("Foo[obj] : Bar {}");
    tester_class("Foo[obj] {}");


    tester_class("Object { var a; }");
    tester_class("Object { var a = 1; }");
    tester_class("Object { var <a = 1; }");
    tester_class("Object { var >a = 1; }");
    tester_class("Object { var <>a = 1; }");

    tester_class("Object { \n"
                 "   var <a, <>b, >c;\n"
                 "   classvar <a, <>b, >c;\n"
                 "   const <d, <>e, >f;\n"
                 "   foo { |a,b,c| ^a + b + c }\n"
                 "   *bar { |a,b,c| ^a + b + c }\n"
                 "   + { |a,b,c| ^a + b + c }\n"
                 "   - { |a,b,c| ^a + b + c }\n"

                 "   * + { |a,b,c| ^a + b + c }\n"
                 "   * - { |a,b,c| ^a + b + c }\n"
                 "}\n"
                 "\n"
                 "+Foo {}");
}

BOOST_AUTO_TEST_CASE(regions) {
    tester(
        R"%(
(
    1 + 1
)

2
3
)%");

    tester(
        R"%(
(
    1 + 3 +    
)

2
3
)%");

    tester(
        R"%(
3 * 2.pow(2)

(
"hello".postln
)


( 1 + 3 +    )

2
3
)%");


    tester(
        R"%(
a.foo;

(1, a: 10)

1 + 1
)%");


    tester("(+)()");
    tester("foo()");
    tester("(1+1).[]");
    tester("(1+1)[]");

    tester(
        R"%(
(
    1 + 2;
    "hello"
    "meow"
        .postln
)

[1, 2, 3];
)%");


    tester(" 1 + 2 (\n\n 4 * 4;");
    tester(" 1 + 2 (\n\n 4 * 12 );");
}
