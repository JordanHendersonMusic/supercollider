// What is at a position, from the compiled graphs: hover, definition, references, and outlines. Also the name queries.

#include "sc_query/queries.hpp"
#include "internal.hpp"

#include "sc_compiler/details/class_library_internals.hpp"
#include "sc_compiler/sc_compiler.hpp"
#include "sc_sema/context.hpp"
#include "sc_sema/ir.hpp"
#include "sc_sema/symbol_table.hpp"
#include "sc_source/text_info.hpp"

#include <algorithm>
#include <set>
#include <sstream>

namespace sc::query {

namespace {

const char* member_word(MemberKind k) {
    switch (k) {
    case MemberKind::Instance:
        return "var";
    case MemberKind::Class:
        return "classvar";
    case MemberKind::Constant:
        return "const";
    }
    return "var";
}

std::string lower(std::string_view s) {
    std::string out { s };
    for (auto& c : out)
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return out;
}

// Collects the occurrences of one graph written in one text.
struct Collector {
    const Library& library;
    const ir::IRGraph& g;
    const src::TextInfo* text; // only nodes in this text
    Analysis& out;
    std::set<std::uint32_t> arguments {};

    const compiler::ClassLibraryInternals& internals() const { return *library.compiled().internals; }
    std::string name(ir::Symbol s) const { return std::string { internals().symbols(s) }; }
    bool here(const ir::SourceLocation& l) const { return g.text_info(l.source).get() == text; }
    Location location(const ir::SourceLocation& l) const {
        const auto& t = g.text_info(l.source);
        return { t && t->file_path ? *t->file_path : std::filesystem::path {}, to_range(ir::IRGraph::range(l)) };
    }

    // The class of a literal or collection receiver, and whether it is the class itself.
    std::pair<std::optional<ClassId>, bool> receiver_class(ir::NodeIndex receiver) const {
        const auto& constants = ir::constant_table(*internals().ctx);
        const auto named = [&](std::string_view n) { return std::make_pair(library.find_class(n), false); };
        if (const auto lit = g.as_a<ir::SCLiteral_I>(receiver)) {
            const auto& c = constants(g.payload(*lit).index);
            if (const auto* cls = c.get_if<ir::SymSCClassName>())
                return { library.find_class(name(*cls)), !internals().symbols.is_meta(*cls) };
            if (c.get_if<std::int32_t>())
                return named("Integer");
            if (c.get_if<double>())
                return named("Float");
            if (c.get_if<char>())
                return named("Char");
            if (const auto* b = c.get_if<bool>())
                return named(*b ? "True" : "False");
            if (c.get_if<ir::Nil>())
                return named("Nil");
            if (c.get_if<ir::SymSCSymbol>())
                return named("Symbol");
            if (c.get_if<ir::SymSCString>())
                return named("String");
            if (c.get_if<ir::ConstExprEvent>())
                return named("Event");
            return named("Array");
        }
        if (const auto coll = g.as_a<ir::SCCollection_I>(receiver))
            return named(name(*g.payload(*coll).collection_class));
        if (g.as_a<ir::SCFunction_I>(receiver))
            return named("Function");
        // `Foo.new`, and the other `new` methods, are guessed to make an instance of Foo.
        if (const auto message = g.as_a<ir::SCMessage_I>(receiver)) {
            const auto selector = name(*g.payload(*message).selector);
            const auto [inner, p, k, v, a] = g.children(*message);
            const auto [cls, class_side] = receiver_class(ir::NodeIndex { *inner });
            if (cls && class_side && selector.rfind("new", 0) == 0)
                return { cls, false };
        }
        return { std::nullopt, false };
    }

    void add(Occurrence o) { out.occurrences.push_back(std::move(o)); }

    void local(const ir::SourceLocation& at, ir::LocalIndex l, bool declaration) {
        if (!here(at))
            return;
        const auto& v = g.local(l);
        Occurrence o;
        o.kind = Occurrence::Kind::Local;
        o.range = to_range(ir::IRGraph::range(at));
        o.name = name(v.name);
        o.declaration = declaration;
        o.graph = &g;
        o.local = l;
        o.argument = arguments.count(*l) != 0;
        o.declared = location(v.declaration);
        add(std::move(o));
    }

    void member(const ir::SourceLocation& at, ir::MemberIndex m, MemberKind kind) {
        if (!here(at))
            return;
        const auto& v = g.member(m);
        Occurrence o;
        o.kind = Occurrence::Kind::Member;
        o.range = to_range(ir::IRGraph::range(at));
        o.name = name(v.name);
        auto owner = name(v.owner);
        if (owner.rfind("Meta_", 0) == 0)
            owner.erase(0, 5);
        o.owner = owner;
        o.member_kind = kind;
        o.declared = location(v.declaration);
        add(std::move(o));
    }

    void variable(const ir::SourceLocation& at, ir::SymSCIdentifier n, int kind, ir::OptionalLocalIndex l,
                  ir::OptionalMemberIndex m, bool declaration) {
        using K = ir::SCRead::Kind;
        switch (static_cast<K>(kind)) {
        case K::Local:
            if (l)
                local(at, ir::LocalIndex { *l }, declaration);
            break;
        case K::Instance:
            if (m)
                member(at, ir::MemberIndex { *m }, MemberKind::Instance);
            break;
        case K::Class:
            if (m)
                member(at, ir::MemberIndex { *m }, MemberKind::Class);
            break;
        case K::Environment:
            if (here(at)) {
                Occurrence o;
                o.kind = Occurrence::Kind::Environment;
                o.range = to_range(ir::IRGraph::range(at));
                o.name = name(n);
                o.declaration = declaration;
                add(std::move(o));
            }
            break;
        }
    }

    void run(ir::NodeIndex root) {
        const auto& constants = ir::constant_table(*internals().ctx);
        // Arguments first, so their uses know they are arguments.
        g.depth_first_traverse(root,
                               [&](const ir::IRGraph::Edges&, const ir::IRGraph::Variant& payload,
                                   const ir::SourceLocation&, ir::NodeIndex, size_t) {
                                   if (const auto* a = std::get_if<ir::SCArgumentDeclare>(&payload))
                                       arguments.insert(*a->local);
                               });
        g.depth_first_traverse(
            root,
            [&](const ir::IRGraph::Edges&, const ir::IRGraph::Variant& payload, const ir::SourceLocation& loc,
                ir::NodeIndex i, size_t) {
                if (const auto* r = std::get_if<ir::SCRead>(&payload)) {
                    variable(loc, r->name, static_cast<int>(r->kind), r->local, r->member, false);
                } else if (const auto* a = std::get_if<ir::SCAssign>(&payload)) {
                    variable(a->name.location, *a->name, static_cast<int>(a->kind), a->local, a->member, false);
                } else if (const auto* d = std::get_if<ir::SCVariableDeclare>(&payload)) {
                    local(d->name.location, d->local, true);
                } else if (const auto* arg = std::get_if<ir::SCArgumentDeclare>(&payload)) {
                    local(g.local(arg->local).declaration, arg->local, true);
                } else if (const auto* t = std::get_if<ir::SCPackTarget>(&payload)) {
                    variable(t->name.location, *t->name, static_cast<int>(t->kind), t->local, t->member, false);
                } else if (const auto* q = std::get_if<ir::SCGenQualIterate>(&payload)) {
                    local(q->element.location, q->element_local, true);
                    if (q->index && q->index_local)
                        local(q->index->location, ir::LocalIndex { *q->index_local }, true);
                } else if (const auto* gd = std::get_if<ir::SCGenQualDeclare>(&payload)) {
                    local(gd->name.location, gd->local, true);
                } else if (const auto* lit = std::get_if<ir::SCLiteral>(&payload)) {
                    if (!here(loc))
                        return;
                    const auto& c = constants(lit->index);
                    if (const auto* cls = c.get_if<ir::SymSCClassName>()) {
                        Occurrence o;
                        o.kind = Occurrence::Kind::Class;
                        o.range = to_range(ir::IRGraph::range(loc));
                        o.name = name(*cls);
                        if (o.name.rfind("Meta_", 0) == 0)
                            o.name.erase(0, 5);
                        add(std::move(o));
                    } else if (const auto* sym = c.get_if<ir::SymSCSymbol>()) {
                        Occurrence o;
                        o.kind = Occurrence::Kind::Symbol;
                        o.range = to_range(ir::IRGraph::range(loc));
                        o.name = name(*sym);
                        add(std::move(o));
                    }
                } else if (const auto* coll = std::get_if<ir::SCCollection>(&payload)) {
                    if (coll->collection_class.location && here(*coll->collection_class.location)) {
                        Occurrence o;
                        o.kind = Occurrence::Kind::Class;
                        o.range = to_range(ir::IRGraph::range(*coll->collection_class.location));
                        o.name = name(*coll->collection_class);
                        add(std::move(o));
                    }
                } else if (const auto* m = std::get_if<ir::SCMessage>(&payload)) {
                    const auto message = *g.as_a<ir::SCMessage_I>(i);
                    const auto [receiver, positional, keywords, variadic, adverb] = g.children(message);
                    (void)positional;
                    (void)variadic;
                    (void)adverb;
                    const auto [cls, class_side] = receiver_class(ir::NodeIndex { *receiver });
                    const auto selector = name(*m->selector);
                    if (m->selector.location && here(*m->selector.location)) {
                        Occurrence o;
                        o.kind = Occurrence::Kind::Send;
                        o.range = to_range(ir::IRGraph::range(*m->selector.location));
                        o.name = selector;
                        o.graph = &g;
                        o.receiver = cls;
                        o.class_side = class_side;
                        add(std::move(o));
                    }
                    if (const auto list = g.as_a<ir::SCKwArgList_I>(keywords)) {
                        for (auto it = g.children(*list).iter(); it; ++it) {
                            const auto& kw = g.payload(*it);
                            if (!here(kw.name.location))
                                continue;
                            Occurrence o;
                            o.kind = Occurrence::Kind::Keyword;
                            o.range = to_range(ir::IRGraph::range(kw.name.location));
                            o.name = name(*kw.name);
                            o.graph = &g;
                            o.receiver = cls;
                            o.class_side = class_side;
                            o.selector = selector;
                            add(std::move(o));
                        }
                    }
                } else if (const auto* method = std::get_if<ir::SCMethod>(&payload)) {
                    if (!here(method->selector.location))
                        return;
                    Occurrence o;
                    o.kind = Occurrence::Kind::Method;
                    o.range = to_range(ir::IRGraph::range(method->selector.location));
                    o.name = name(*method->selector);
                    o.declaration = true;
                    // Which MethodInfo: the one declared at this place.
                    for (const auto id : library.implementors(o.name)) {
                        const auto& info = library.get(id);
                        if (info.location.range.begin == o.range.begin && info.location.path == location(loc).path) {
                            o.method = id;
                            o.receiver = info.owner;
                            o.class_side = info.class_method;
                        }
                    }
                    add(std::move(o));
                } else if (const auto* c = std::get_if<ir::SCClass>(&payload)) {
                    if (!here(loc) || internals().symbols.is_meta(c->name))
                        return;
                    Occurrence o;
                    o.kind = Occurrence::Kind::Class;
                    o.name = name(c->name);
                    o.range = name_location(g, loc, o.name).range;
                    o.declaration = true;
                    add(std::move(o));
                } else if (const auto* member_node = std::get_if<ir::SCMember>(&payload)) {
                    if (!here(loc))
                        return;
                    Occurrence o;
                    o.kind = Occurrence::Kind::Member;
                    o.range = to_range(ir::IRGraph::range(loc));
                    o.name = name(member_node->name);
                    auto owner = name(member_node->owning_class);
                    const bool meta = owner.rfind("Meta_", 0) == 0;
                    if (meta)
                        owner.erase(0, 5);
                    o.owner = owner;
                    o.member_kind = !meta       ? MemberKind::Instance
                        : member_node->is_const ? MemberKind::Constant
                                                : MemberKind::Class;
                    o.declaration = true;
                    o.declared = location(loc);
                    add(std::move(o));
                }
            });

        // Locals in scope, for completion.
        for (const auto l : g.locals().indices()) {
            const auto& v = g.local(l);
            if (!here(v.declaration))
                continue;
            const auto scope = to_range(ir::IRGraph::range(g.location(ir::NodeIndex { *v.owner })));
            out.locals.push_back(
                { name(v.name), to_range(ir::IRGraph::range(v.declaration)).begin, scope, arguments.count(*l) != 0 });
        }
    }
};

// Calls f with each class graph of the library.
template <typename F> void each_class_graph(const Library& library, F&& f) {
    library.compiled().internals->compiler.depth_first_do([&](const ir::IRGraph& g) {
        const auto root = g.root();
        if (root && std::holds_alternative<ir::SCClass_I>(*root))
            f(g, std::get<ir::SCClass_I>(*root));
    });
}

void collect_class_file(const Library& library, const src::TextInfo* text, Analysis& out) {
    each_class_graph(library, [&](const ir::IRGraph& g, ir::SCClass_I c) {
        bool uses = false;
        for (const auto& s : g.sources().raw())
            uses = uses || s.get() == text;
        if (!uses)
            return;
        Collector { library, g, text, out }.run(ir::NodeIndex { *c });
    });
}

void sort_occurrences(Analysis& a) {
    std::stable_sort(a.occurrences.begin(), a.occurrences.end(),
                     [](const Occurrence& l, const Occurrence& r) { return l.range.begin < r.range.begin; });
}

std::string code_block(const std::string& code) { return "```supercollider\n" + code + "\n```"; }

std::string class_line(const Library& library, ClassId c) {
    const auto& info = library.get(c);
    std::string out = info.name;
    if (info.superclass)
        out += " : " + library.get(*info.superclass).name;
    return out;
}

std::string class_hover(const Library& library, ClassId c) {
    const auto& info = library.get(c);
    std::string out = code_block(class_line(library, c));
    std::string chain;
    for (const auto s : library.superclasses(c)) {
        if (s == c)
            continue;
        chain += (chain.empty() ? "" : " → ") + library.get(s).name;
    }
    if (!chain.empty())
        out += "\n\nInherits from " + chain + ".";
    out += "\n\n" + std::to_string(info.methods.size()) + " instance methods, "
        + std::to_string(info.class_methods.size()) + " class methods, " + std::to_string(info.subclasses.size())
        + " subclasses.";
    if (info.hook)
        out += "\n\nA language server hook.";
    out += "\n\n" + Library::where(info.location);
    return out;
}

std::string method_hover(const Library& library, MethodId m) {
    const auto& info = library.get(m);
    std::string out = code_block(library.signature(m));
    if (!info.primitive.empty())
        out += "\n\nCalls the primitive `" + info.primitive + "`.";
    if (info.extension)
        out += "\n\nIn a class extension.";
    out += "\n\n" + Library::where(info.location);
    return out;
}

// The methods a send could reach: the one dispatched to when the receiver is known, otherwise every implementor.
std::vector<MethodId> targets(const Library& library, const Occurrence& o, std::string_view selector) {
    if (o.receiver)
        if (const auto m = library.lookup(*o.receiver, o.class_side, selector))
            return { *m };
    return library.implementors(selector);
}

std::string implementors_hover(const Library& library, const std::vector<MethodId>& methods,
                               std::string_view selector) {
    if (methods.empty())
        return code_block(std::string { selector }) + "\n\nNo class implements this.";
    if (methods.size() == 1)
        return method_hover(library, methods.front());
    std::string list;
    constexpr std::size_t shown = 8;
    for (std::size_t i = 0; i < methods.size() && i < shown; ++i)
        list += library.signature(methods[i]) + "\n";
    std::string out = code_block(list.substr(0, list.size() - 1));
    out += "\n\n" + std::to_string(methods.size()) + " implementations";
    if (methods.size() > shown)
        out += ", " + std::to_string(methods.size() - shown) + " not shown";
    return out + ".";
}

std::vector<Location> method_locations(const Library& library, const std::vector<MethodId>& methods) {
    std::vector<Location> out;
    for (const auto m : methods)
        out.push_back(library.get(m).location);
    return out;
}

const MemberInfo* find_member(const Library& library, const std::string& owner, const std::string& name) {
    const auto c = library.find_class(owner);
    if (!c)
        return nullptr;
    for (const auto& m : library.get(*c).members)
        if (m.name == name)
            return &m;
    return nullptr;
}

}

////////////////////////////////////////////////////////////////////////////////
// Analysis
////////////////////////////////////////////////////////////////////////////////

Analysis analyse_names(const Document& document) {
    Analysis out;
    if (document.kind() == DocumentKind::ClassFile) {
        const auto& library = document.library();
        if (!library)
            return out;
        out.library = library;
        if (const auto text = library->text_of(document.path()))
            collect_class_file(*library, text.get(), out);
    } else {
        const auto& g = document.graph();
        const auto& library = document.graph_library();
        if (!g || !library)
            return out;
        out.library = library;
        const auto root = g->root();
        if (root && std::holds_alternative<ir::SCInterpreted_I>(*root))
            Collector { *library, *g, document.graph_text().get(), out }.run(
                ir::NodeIndex { *std::get<ir::SCInterpreted_I>(*root) });
    }
    sort_occurrences(out);
    return out;
}

const Occurrence* occurrence_at(const Analysis& analysis, Position p) {
    const Occurrence* best = nullptr;
    for (const auto& o : analysis.occurrences)
        if (o.range.touches(p) && (!best || o.range.smaller_than(best->range)))
            best = &o;
    return best;
}

////////////////////////////////////////////////////////////////////////////////
// Hover and definition
////////////////////////////////////////////////////////////////////////////////

std::optional<Hover> hover(const Document& document, Position p) {
    const auto analysis = analyse_names(document);
    const auto* o = occurrence_at(analysis, p);
    if (!o || !analysis.library)
        return std::nullopt;
    const auto& library = *analysis.library;
    std::string text;
    switch (o->kind) {
    case Occurrence::Kind::Local:
        text = code_block(std::string { o->argument ? "arg " : "var " } + o->name);
        break;
    case Occurrence::Kind::Member: {
        text = code_block(std::string { member_word(o->member_kind) } + " " + o->name);
        if (const auto* m = find_member(library, o->owner, o->name)) {
            if (!m->value.empty())
                text = code_block(std::string { member_word(o->member_kind) } + " " + o->name + " = " + m->value);
        }
        text += "\n\nDeclared in " + o->owner + ", " + Library::where(o->declared);
        break;
    }
    case Occurrence::Kind::Environment:
        text = code_block("~" + o->name) + "\n\nAn environment variable.";
        break;
    case Occurrence::Kind::Class:
        if (const auto c = library.find_class(o->name))
            text = class_hover(library, *c);
        else
            text = code_block(o->name) + "\n\nNo such class.";
        break;
    case Occurrence::Kind::Send:
        text = implementors_hover(library, targets(library, *o, o->name), o->name);
        break;
    case Occurrence::Kind::Method:
        if (o->method) {
            text = method_hover(library, *o->method);
            const auto& info = library.get(*o->method);
            if (const auto super = library.get(info.owner).superclass)
                if (const auto overridden = library.lookup(*super, info.class_method, info.selector))
                    text += "\n\nOverrides " + library.signature(*overridden) + ".";
        } else {
            text = code_block(o->name);
        }
        break;
    case Occurrence::Kind::Keyword: {
        text = code_block(o->name + ":");
        const auto methods = targets(library, *o, o->selector);
        if (methods.size() == 1)
            text += "\n\nAn argument of " + library.signature(methods.front()) + ".";
        else
            text += "\n\nAn argument of `" + o->selector + "`.";
        break;
    }
    case Occurrence::Kind::Symbol:
        text = code_block("\\" + o->name);
        break;
    }
    return Hover { std::move(text), o->range };
}

std::vector<Location> definition(const Document& document, Position p) {
    const auto analysis = analyse_names(document);
    const auto* o = occurrence_at(analysis, p);
    if (!o || !analysis.library)
        return {};
    const auto& library = *analysis.library;
    switch (o->kind) {
    case Occurrence::Kind::Local:
    case Occurrence::Kind::Member:
        return { o->declared };
    case Occurrence::Kind::Class:
        if (const auto c = library.find_class(o->name))
            return { library.get(*c).location };
        return {};
    case Occurrence::Kind::Send:
        return method_locations(library, targets(library, *o, o->name));
    case Occurrence::Kind::Method:
        if (o->method)
            return { library.get(*o->method).location };
        return {};
    case Occurrence::Kind::Keyword:
        return method_locations(library, targets(library, *o, o->selector));
    case Occurrence::Kind::Environment: {
        std::vector<Location> out;
        for (const auto& other : analysis.occurrences)
            if (other.kind == Occurrence::Kind::Environment && other.name == o->name) {
                out.push_back({ {}, other.range });
                break;
            }
        return out;
    }
    case Occurrence::Kind::Symbol:
        return {};
    }
    return {};
}

////////////////////////////////////////////////////////////////////////////////
// References
////////////////////////////////////////////////////////////////////////////////

namespace {
bool same_target(const Occurrence& target, const Occurrence& o) {
    if (o.name != target.name)
        return false;
    using K = Occurrence::Kind;
    switch (target.kind) {
    case K::Class:
        return o.kind == K::Class;
    case K::Send:
    case K::Method:
        return o.kind == K::Send || o.kind == K::Method;
    case K::Member:
        return o.kind == K::Member && o.owner == target.owner;
    case K::Environment:
        return o.kind == K::Environment;
    case K::Symbol:
        return o.kind == K::Symbol;
    default:
        return false;
    }
}

void library_occurrences(const Library& library, const std::function<void(const Occurrence&, const Location&)>& f) {
    each_class_graph(library, [&](const ir::IRGraph& g, ir::SCClass_I c) {
        for (const auto& text : g.sources().raw()) {
            Analysis a;
            Collector { library, g, text.get(), a }.run(ir::NodeIndex { *c });
            const auto path = text && text->file_path ? *text->file_path : std::filesystem::path {};
            for (const auto& o : a.occurrences)
                f(o, { path, o.range });
        }
    });
}
}

std::vector<Location> references(const Document& document, Position p, bool include_declaration,
                                 const std::vector<const Document*>& others) {
    const auto analysis = analyse_names(document);
    const auto* target = occurrence_at(analysis, p);
    std::vector<Location> out;
    if (!target || !analysis.library)
        return out;
    const auto keep = [&](const Occurrence& o) { return include_declaration || !o.declaration; };
    if (target->kind == Occurrence::Kind::Local) {
        for (const auto& o : analysis.occurrences)
            if (o.kind == Occurrence::Kind::Local && o.graph == target->graph && o.local == target->local && keep(o))
                out.push_back({ {}, o.range });
        return out;
    }
    if (target->kind == Occurrence::Kind::Keyword)
        return out;
    const bool in_library = target->kind != Occurrence::Kind::Environment;
    if (in_library) {
        std::set<std::pair<std::filesystem::path, std::pair<std::uint32_t, std::uint32_t>>> seen;
        library_occurrences(*analysis.library, [&](const Occurrence& o, const Location& l) {
            if (same_target(*target, o) && keep(o)
                && seen.insert({ l.path, { l.range.begin.line, l.range.begin.column } }).second)
                out.push_back(l);
        });
    }
    if (document.kind() == DocumentKind::Interpreted)
        for (const auto& o : analysis.occurrences)
            if (same_target(*target, o) && keep(o))
                out.push_back({ {}, o.range });
    for (const auto* other : others) {
        if (other == &document || other->kind() != DocumentKind::Interpreted)
            continue;
        const auto a = analyse_names(*other);
        for (const auto& o : a.occurrences)
            if (same_target(*target, o) && keep(o))
                out.push_back({ other->path(), o.range });
    }
    return out;
}

////////////////////////////////////////////////////////////////////////////////
// Symbols
////////////////////////////////////////////////////////////////////////////////

namespace {
Range cover(Range a, const Range& b) {
    if (b.begin < a.begin)
        a.begin = b.begin;
    if (a.end < b.end)
        a.end = b.end;
    return a;
}
}

std::vector<SymbolInfo> document_symbols(const Document& document) {
    std::vector<SymbolInfo> out;
    if (document.kind() == DocumentKind::ClassFile) {
        const auto& library = document.library();
        if (!library)
            return out;
        const auto& path = document.path();
        for (const auto c : library->classes().indices()) {
            const auto& info = library->get(c);
            bool here = info.location.path == path;
            std::vector<SymbolInfo> children;
            for (const auto& m : info.members) {
                if (m.location.path != path)
                    continue;
                const auto kind = m.kind == MemberKind::Instance ? SymbolInfo::Kind::Variable
                    : m.kind == MemberKind::Class                ? SymbolInfo::Kind::ClassVariable
                                                                 : SymbolInfo::Kind::Constant;
                children.push_back({ m.name, member_word(m.kind), kind, m.location, m.location.range, {} });
            }
            for (const auto& list : { info.class_methods, info.methods })
                for (const auto id : list) {
                    const auto& m = library->get(id);
                    if (m.location.path != path)
                        continue;
                    children.push_back({ (m.class_method ? "*" : "") + m.selector,
                                         library->signature(id),
                                         m.class_method ? SymbolInfo::Kind::ClassMethod : SymbolInfo::Kind::Method,
                                         m.whole,
                                         m.location.range,
                                         {} });
                }
            if (!here && children.empty())
                continue;
            std::sort(children.begin(), children.end(), [](const SymbolInfo& l, const SymbolInfo& r) {
                return l.location.range.begin < r.location.range.begin;
            });
            SymbolInfo symbol { here ? info.name : "+ " + info.name,
                                class_line(*library, c),
                                SymbolInfo::Kind::Class,
                                { path, here ? info.location.range : children.front().location.range },
                                here ? info.location.range : children.front().selection,
                                std::move(children) };
            for (const auto& child : symbol.children)
                symbol.location.range = cover(symbol.location.range, child.location.range);
            out.push_back(std::move(symbol));
        }
        std::sort(out.begin(), out.end(), [](const SymbolInfo& l, const SymbolInfo& r) {
            return l.location.range.begin < r.location.range.begin;
        });
        return out;
    }
    const auto analysis = analyse_names(document);
    std::set<std::string> seen;
    for (const auto& o : analysis.occurrences)
        if (o.kind == Occurrence::Kind::Environment && seen.insert(o.name).second)
            out.push_back(
                { "~" + o.name, "environment variable", SymbolInfo::Kind::Variable, { {}, o.range }, o.range, {} });
    return out;
}

std::vector<SymbolInfo> workspace_symbols(const Library& library, std::string_view query, std::size_t limit) {
    std::vector<SymbolInfo> out;
    const auto q = lower(query);
    for (const auto c : library.classes().indices()) {
        const auto& info = library.get(c);
        if (lower(info.name).find(q) != std::string::npos && !info.location.path.empty())
            out.push_back(
                { info.name, class_line(library, c), SymbolInfo::Kind::Class, info.location, info.location.range, {} });
        if (out.size() >= limit)
            return out;
    }
    if (q.empty())
        return out;
    for (const auto m : library.methods().indices()) {
        const auto& info = library.get(m);
        if (lower(info.selector).find(q) != std::string::npos)
            out.push_back({ (info.class_method ? "*" : "") + info.selector,
                            library.signature(m),
                            info.class_method ? SymbolInfo::Kind::ClassMethod : SymbolInfo::Kind::Method,
                            info.location,
                            info.location.range,
                            {} });
        if (out.size() >= limit)
            break;
    }
    return out;
}

////////////////////////////////////////////////////////////////////////////////
// Lint
////////////////////////////////////////////////////////////////////////////////

std::vector<DiagnosticInfo> lint(const Document& document) {
    std::vector<DiagnosticInfo> out;
    if (document.stale())
        return out;
    if (document.kind() == DocumentKind::Interpreted) {
        if (!document.graph() || document.graph_text() != document.text_info())
            return out;
    } else {
        const auto& library = document.library();
        const auto text = library ? library->text_of(document.path()) : nullptr;
        if (!text || !document.text_info() || text->source.as_string() != document.text_info()->source.as_string())
            return out;
    }
    const auto analysis = analyse_names(document);
    if (!analysis.library)
        return out;
    for (const auto& o : analysis.occurrences) {
        if (o.kind != Occurrence::Kind::Send || !std::isalpha(static_cast<unsigned char>(o.name.front())))
            continue;
        if (!analysis.library->implementors(o.name).empty())
            continue;
        DiagnosticInfo d;
        d.severity = Severity::Information;
        d.name = "Unknown selector.";
        d.message = "No class implements `" + o.name
            + "`, so this fails unless the receiver handles it in doesNotUnderstand, as an Event does.";
        d.range = o.range;
        out.push_back(std::move(d));
    }
    return out;
}

std::vector<DiagnosticInfo> all_diagnostics(const Document& document) {
    auto out = document.diagnostics();
    for (auto& d : lint(document))
        out.push_back(std::move(d));
    return out;
}

////////////////////////////////////////////////////////////////////////////////
// Name queries
////////////////////////////////////////////////////////////////////////////////

std::string describe_class(const Library& library, std::string_view name, bool inherited) {
    const auto c = library.find_class(name);
    if (!c) {
        std::string out = "No class named " + std::string { name } + ".";
        const auto similar = describe_search(library, name, 10);
        return out + "\n" + similar;
    }
    const auto& info = library.get(*c);
    std::ostringstream out;
    out << "class " << class_line(library, *c) << "\n";
    out << "Declared at " << Library::where(info.location) << "\n";
    std::string chain;
    for (const auto s : library.superclasses(*c))
        if (s != *c)
            chain += (chain.empty() ? "" : " -> ") + library.get(s).name;
    if (!chain.empty())
        out << "Superclasses: " << chain << "\n";
    if (!info.subclasses.empty()) {
        out << "Subclasses:";
        for (const auto s : info.subclasses)
            out << " " << library.get(s).name;
        out << "\n";
    }
    if (info.hook)
        out << "A language server hook (derives from " << hook_base_class << ").\n";
    if (!info.members.empty()) {
        out << "\nVariables:\n";
        for (const auto& m : info.members) {
            out << "  " << member_word(m.kind) << " " << (m.getter ? "<" : "") << (m.setter ? ">" : "") << m.name;
            if (!m.value.empty())
                out << " = " << m.value;
            out << "    " << Library::where(m.location) << "\n";
        }
    }
    const auto list = [&](const char* title, bool class_side) {
        const auto methods =
            inherited ? library.responds_to(*c, class_side) : (class_side ? info.class_methods : info.methods);
        if (methods.empty())
            return;
        std::vector<MethodId> sorted { methods.begin(), methods.end() };
        std::sort(sorted.begin(), sorted.end(),
                  [&](MethodId l, MethodId r) { return library.get(l).selector < library.get(r).selector; });
        out << "\n" << title << ":\n";
        for (const auto m : sorted)
            out << "  " << library.signature(m) << "    " << Library::where(library.get(m).location) << "\n";
    };
    list(inherited ? "Class methods, with inherited ones" : "Class methods", true);
    list(inherited ? "Instance methods, with inherited ones" : "Instance methods", false);
    return out.str();
}

std::string describe_implementors(const Library& library, std::string_view selector) {
    const auto& methods = library.implementors(selector);
    if (methods.empty())
        return "No class implements " + std::string { selector } + ".";
    std::vector<MethodId> sorted { methods.begin(), methods.end() };
    std::sort(sorted.begin(), sorted.end(), [&](MethodId l, MethodId r) {
        return library.get(library.get(l).owner).name < library.get(library.get(r).owner).name;
    });
    std::ostringstream out;
    out << sorted.size() << " implementations of " << selector << ":\n";
    for (const auto m : sorted)
        out << "  " << library.signature(m) << "    " << Library::where(library.get(m).location) << "\n";
    return out.str();
}

std::string describe_definition(const Library& library, std::string_view name) {
    std::string text { name };
    // Foo:bar, Foo.bar and Foo.*bar
    for (const auto* separator : { ".*", ":", "." }) {
        const auto at = text.find(separator);
        if (at == std::string::npos || at == 0)
            continue;
        const auto class_name = text.substr(0, at);
        const auto selector = text.substr(at + std::string_view { separator }.size());
        const auto c = library.find_class(class_name);
        if (!c)
            return "No class named " + class_name + ".";
        const bool class_side = std::string_view { separator } != ":";
        const auto m = library.lookup(*c, class_side, selector);
        if (!m && std::string_view { separator } == ".")
            if (const auto instance = library.lookup(*c, false, selector))
                return library.signature(*instance) + "\n" + Library::where(library.get(*instance).location) + "\n";
        if (!m)
            return class_name + " doesn't respond to " + selector + ".";
        std::string out = library.signature(*m) + "\n" + Library::where(library.get(*m).location) + "\n";
        if (library.get(*m).owner != *c)
            out += "(inherited from " + library.get(library.get(*m).owner).name + ")\n";
        return out;
    }
    if (const auto c = library.find_class(text))
        return "class " + class_line(library, *c) + "\n" + Library::where(library.get(*c).location) + "\n";
    if (!library.implementors(text).empty())
        return describe_implementors(library, text);
    return "Nothing named " + text + " in the class library.\n" + describe_search(library, text, 10);
}

std::string describe_subclasses(const Library& library, std::string_view name, std::size_t depth) {
    const auto c = library.find_class(name);
    if (!c)
        return "No class named " + std::string { name } + ".";
    std::ostringstream out;
    std::size_t count = 0;
    const std::function<void(ClassId, std::size_t)> walk = [&](ClassId at, std::size_t level) {
        out << std::string(level * 2, ' ') << library.get(at).name << "\n";
        ++count;
        if (level >= depth || count > 2000)
            return;
        for (const auto s : library.get(at).subclasses)
            walk(s, level + 1);
    };
    walk(*c, 0);
    return out.str();
}

std::string describe_search(const Library& library, std::string_view query, std::size_t limit) {
    const auto symbols = workspace_symbols(library, query, limit);
    if (symbols.empty())
        return "Nothing matches " + std::string { query } + ".\n";
    std::ostringstream out;
    for (const auto& s : symbols)
        out << "  " << (s.kind == SymbolInfo::Kind::Class ? "class " + s.detail : s.detail) << "    "
            << Library::where(s.location) << "\n";
    if (symbols.size() >= limit)
        out << "  (only the first " << limit << " are shown)\n";
    return out.str();
}

std::string describe_references(const Library& library, std::string_view name, std::size_t limit) {
    Occurrence target;
    target.name = std::string { name };
    const bool is_class = !name.empty() && std::isupper(static_cast<unsigned char>(name.front()));
    target.kind = is_class ? Occurrence::Kind::Class : Occurrence::Kind::Send;
    std::vector<Location> found;
    library_occurrences(library, [&](const Occurrence& o, const Location& l) {
        if (same_target(target, o) && !o.declaration)
            found.push_back(l);
    });
    std::sort(found.begin(), found.end(), [](const Location& l, const Location& r) {
        return std::tie(l.path, l.range.begin.line, l.range.begin.column)
            < std::tie(r.path, r.range.begin.line, r.range.begin.column);
    });
    found.erase(std::unique(found.begin(), found.end(),
                            [](const Location& l, const Location& r) {
                                return l.path == r.path && l.range.begin == r.range.begin;
                            }),
                found.end());
    if (found.empty())
        return "No uses of " + std::string { name } + " in the class library.\n";
    std::ostringstream out;
    out << found.size() << (is_class ? " uses of " : " sends of ") << name << " in the class library:\n";
    for (std::size_t i = 0; i < found.size() && i < limit; ++i)
        out << "  " << Library::where(found[i]) << "\n";
    if (found.size() > limit)
        out << "  (only the first " << limit << " are shown)\n";
    return out.str();
}

std::string describe_diagnostics(const std::vector<DiagnosticInfo>& diagnostics, const std::string& file_name) {
    if (diagnostics.empty())
        return "No problems found.\n";
    std::ostringstream out;
    std::size_t counts[3] = { 0, 0, 0 };
    for (const auto& d : diagnostics)
        ++counts[static_cast<int>(d.severity)];
    const auto count = [](std::size_t n, const char* word) {
        return std::to_string(n) + " " + word + (n == 1 ? "" : "s");
    };
    out << count(counts[0], "error") << ", " << count(counts[1], "warning") << ", " << count(counts[2], "note")
        << ".\n";
    for (const auto& d : diagnostics) {
        out << "\n"
            << file_name << ":" << d.range.begin.line + 1 << ":" << d.range.begin.column + 1 << ": "
            << (d.severity == Severity::Error         ? "error: "
                    : d.severity == Severity::Warning ? "warning: "
                                                      : "note: ")
            << d.message << "\n";
        for (const auto& [where, message] : d.related) {
            if (where.path.empty())
                out << "  " << file_name << ":" << where.range.begin.line + 1 << ":" << where.range.begin.column + 1;
            else
                out << "  " << Library::where(where);
            out << ": " << message << "\n";
        }
    }
    return out.str();
}

}
