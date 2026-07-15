#include "node_graph.hpp"
#include "nodes.hpp"
#include "normalise_source.hpp"
#include "sc_parser.hpp"
#include "text_info.hpp"
#include "text_location.hpp"
#include <memory>
#define BOOST_TEST_MODULE sc_parser_tests
#include <boost/test/included/unit_test.hpp>
#include "parser_error_handler.hpp"


namespace P = sc ::parser;

class PostErrorHandler final : public P::ErrorHandler {
public:
    void operator()(std::shared_ptr<const sc::parser::TextInfo>, sc::lex::TokenType, sc::lex::SourceCodeRange,
                    std::optional<sc::lex::SourceCodeRange>) override {
        std::cout << "Got an error 1" << std::endl;
    }

    // Called from memory error, unlikely.
    void operator()(std::shared_ptr<const sc::parser::TextInfo>, sc::lex::SourceCodeRange,
                    const std::string&) override {
        std::cout << "Got an error 2" << std::endl;
    }

    // Called for parsing error.
    // Note, the 'int' type here is a parser::symbol_kind_type. The parser has good conversion functions.
    void operator()(std::shared_ptr<const sc::parser::TextInfo> t, std::vector<const char*> expected,
                    sc::lex::SourceCodeRange got_location, const char* got_name) override {
        if (!expected.empty())
            for (const char* e : expected)
                std::cout << e << ' ';
        std::cout << "got: " << got_name << " ";
        const auto [ptr, sz] = t->read(got_location);
        std::cout.write(ptr, sz);
        std::cout << "\n";

        std::cout << "Got an error 3" << std::endl;
    }

    PostErrorHandler() {}
    PostErrorHandler(const PostErrorHandler&) = default;
    PostErrorHandler(PostErrorHandler&&) = delete;
    PostErrorHandler& operator=(const PostErrorHandler&) = default;
    PostErrorHandler& operator=(PostErrorHandler&&) = delete;
    virtual ~PostErrorHandler() = default;
};

BOOST_AUTO_TEST_CASE(literals) {
    const auto tester = [](const char* src) {
        auto text_info = std::shared_ptr<P::TextInfo>(new P::TextInfo {
            sc::lex::NormalisedSource(src),
            sc::lex::FileCodeLocation {},
            "test_file",
            false,
        });

        const auto [graph, result] = P::parse(text_info, std::make_shared<PostErrorHandler>());
        BOOST_TEST(result == 0);

        namespace N = P::nodes;

        graph.flat_walk([](sc::lex::SourceCodeRange, const N::Edges& e, const N::NodeVariant& payload, size_t i) {
            std::cout << i << " " << N::NodeCollection::get_name(payload) << ' ' << "parent: " << *e.parent
                      << " first child: " << *e.first_child << " next sibling: " << *e.next_sibling
                      << " last sibling: " << *e.last_sibling << std::endl;
        });
        std::cout << std::endl;


        graph.depth_first_traverse([](sc::lex::SourceCodeRange, const N::Edges& e, const N::NodeVariant& payload,
                                      size_t i, size_t depth, size_t v) {
            for (size_t di { 0 }; di < depth; ++di)
                std::cout << '\t';
            std::cout << N::NodeCollection::get_name(payload);
            std::cout << std::endl;
        });
    };

    tester("\n\n1.2 + 'a'\n\n");
}
