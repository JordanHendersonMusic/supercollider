#include "sc_query/library.hpp"
#include "internal.hpp"

#include "sc_compiler/details/class_library_internals.hpp"
#include "sc_compiler/inspect.hpp"
#include "sc_diagnostic/sc_diagnostic.hpp"
#include "sc_sema/context.hpp"
#include "sc_sema/ir.hpp"
#include "sc_sema/symbol_table.hpp"
#include "sc_source/normalise_source.hpp"
#include "sc_source/text_info.hpp"

#include <algorithm>
#include <fstream>
#include <set>
#include <sstream>
#include <thread>

namespace sc::query {

namespace {

////////////////////////////////////////////////////////////////////////////////
// Finding files, as sclang's ClassFileFinder does
////////////////////////////////////////////////////////////////////////////////

std::string lower(std::string s) {
    for (auto& c : s)
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return s;
}

bool non_host_platform(const std::string& name) {
#if defined(__APPLE__)
    return name == "linux" || name == "windows" || name == "iphone" || name == "wasm";
#elif defined(_WIN32)
    return name == "osx" || name == "linux" || name == "iphone" || name == "wasm";
#elif defined(__EMSCRIPTEN__)
    return name == "osx" || name == "windows" || name == "iphone" || name == "linux";
#else
    return name == "osx" || name == "windows" || name == "iphone" || name == "wasm";
#endif
}

bool skipped_directory(const std::filesystem::path& p) {
    const auto name = lower(p.filename().u8string());
    if (name == "help" || name == "ignore" || name == ".svn" || name == ".git" || name == "_darcs")
        return true;
    if (name.rfind("scide_", 0) == 0 && name != "scide_scqt")
        return true;
    return non_host_platform(name);
}

bool excluded(const std::filesystem::path& p, const std::vector<std::filesystem::path>& excludes) {
    for (const auto& e : excludes) {
        const auto rel = p.lexically_relative(e);
        if (!rel.empty() && *rel.begin() != "..")
            return true;
    }
    return false;
}

compiler::ClassFiles find_files(const LibraryConfig& config, std::string& log) {
    compiler::ClassFiles files;
    std::set<std::filesystem::path> seen;
    std::vector<std::filesystem::path> excludes;
    for (const auto& e : config.excludes)
        excludes.push_back(normal_path(e));
    std::uint32_t module = 0;
    for (const auto& m : config.modules) {
        const ir::ModuleIndex index { module++ };
        const auto root = normal_path(m);
        std::error_code ec;
        if (std::filesystem::is_regular_file(root, ec)) {
            if (root.extension() == ".sc" && seen.insert(root).second && !excluded(root, excludes))
                (void)files.push_back({ root, index });
            continue;
        }
        if (!std::filesystem::is_directory(root, ec)) {
            log += "Could not open directory '" + root.u8string() + "'\n";
            continue;
        }
        if (excluded(root, excludes))
            continue;
        std::vector<std::filesystem::path> found;
        std::filesystem::recursive_directory_iterator it { root,
                                                           std::filesystem::directory_options::follow_directory_symlink,
                                                           ec },
            end;
        for (; !ec && it != end; it.increment(ec)) {
            const auto& path = it->path();
            if (it->is_directory(ec)) {
                if (skipped_directory(path) || excluded(normal_path(path), excludes))
                    it.disable_recursion_pending();
                continue;
            }
            if (path.extension() != ".sc" || path.filename().u8string().front() == '.')
                continue;
            auto file = normal_path(path);
            if (!excluded(file, excludes) && seen.insert(file).second)
                found.push_back(std::move(file));
        }
        std::sort(found.begin(), found.end());
        for (auto& f : found)
            (void)files.push_back({ std::move(f), index });
    }
    return files;
}

std::optional<std::string> read_file(const std::filesystem::path& path) {
    std::ifstream in { path, std::ios::binary };
    if (!in.is_open())
        return std::nullopt;
    std::stringstream ss;
    ss << in.rdbuf();
    return std::move(ss).str();
}

Location location_of(const ir::IRGraph& g, const ir::SourceLocation& l) {
    const auto& text = g.text_info(l.source);
    return { text && text->file_path ? *text->file_path : std::filesystem::path {}, to_range(ir::IRGraph::range(l)) };
}

constexpr std::string_view meta_prefix = "Meta_";

}

Location name_location(const ir::IRGraph& g, const ir::SourceLocation& l, std::string_view name) {
    auto out = location_of(g, l);
    const auto text = g.read(l);
    // A class's location is its whole declaration, which starts with its name.
    if (text.substr(0, name.size()) == name)
        out.range.end = { out.range.begin.line, out.range.begin.column + count_codepoints(name) };
    return out;
}

namespace {

}

////////////////////////////////////////////////////////////////////////////////
// Building
////////////////////////////////////////////////////////////////////////////////

Library::Library() = default;
Library::~Library() = default;

std::shared_ptr<Library> build_library(const LibraryConfig& config, const Overlays& overlays,
                                       const ExtraSources& extra) {
    const auto start = std::chrono::steady_clock::now();
    auto library = std::make_shared<Library>();
    library->m_config = config;
    const auto files = find_files(config, library->m_log);

    util::IndexedVector<ir::SourceEntryIndex, compiler::ClassFileSource> sources;
    for (const auto i : files.indices()) {
        const auto& file = files[i];
        FileStamp stamp { file.path };
        std::error_code ec;
        stamp.size = std::filesystem::file_size(file.path, ec);
        stamp.modified = std::filesystem::last_write_time(file.path, ec);
        std::optional<std::string> text;
        if (const auto overlay = overlays.find(file.path); overlay != overlays.end()) {
            text = overlay->second;
            stamp.overlaid = true;
        } else {
            text = read_file(file.path);
        }
        if (!text) {
            library->m_log += "Could not read '" + file.path.u8string() + "'\n";
            text = std::string {};
        }
        std::shared_ptr<const src::TextInfo> info;
        try {
            info = std::make_shared<const src::TextInfo>(src::NormalisedSource { *text }, src::FileCodeLocation {},
                                                         file.path, src::TextKind::ClassFile);
        } catch (const std::length_error&) {
            library->m_log += "'" + file.path.u8string() + "' is too large\n";
            info = std::make_shared<const src::TextInfo>(src::NormalisedSource { std::string {} },
                                                         src::FileCodeLocation {}, file.path, src::TextKind::ClassFile);
        }
        library->m_texts[file.path] = info;
        library->m_files.push_back(std::move(stamp));
        (void)sources.push_back({ std::move(info), file.module });
    }

    // An overlay of a file outside the modules, e.g., a class file open in an editor, is compiled as an extra one.
    auto all_extra = extra;
    for (const auto& [path, text] : overlays)
        if (!library->m_texts.count(path))
            all_extra.emplace_back(path, text);
    const ir::ModuleIndex extra_module { static_cast<std::uint32_t>(config.modules.size()) };
    for (const auto& [path, text] : all_extra) {
        auto info = std::make_shared<const src::TextInfo>(src::NormalisedSource { text }, src::FileCodeLocation {},
                                                          path, src::TextKind::ClassFile);
        library->m_texts[path] = info;
        library->m_files.push_back({ path, 0, {}, true });
        ++library->m_extra_files;
        (void)sources.push_back({ std::move(info), extra_module });
    }

    const auto threads = config.num_threads ? config.num_threads : std::max(1u, std::thread::hardware_concurrency());
    library->m_library = compiler::compile_class_library(sources, threads, {});
    library->m_log += library->m_library->parse_log;
    for (auto& d : library->m_library->parse_diagnostics)
        library->m_diagnostics.push_back(std::move(d));
    for (auto& d : library->m_library->diagnostics)
        library->m_diagnostics.push_back(std::move(d));
    library->m_library->parse_diagnostics.clear();
    library->m_library->diagnostics.clear();
    library->index();
    library->m_build_seconds = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    return library;
}

void Library::index() {
    auto& internals = *m_library->internals;
    const auto& symbols = internals.symbols;
    const auto& constants = ir::constant_table(*internals.ctx);
    const auto text = [&](ir::Symbol s) { return std::string { symbols(s) }; };
    const auto literal = [&](ir::ConstExprIndex c) {
        std::ostringstream out;
        constants(c).to_literal(out, symbols, constants);
        return out.str();
    };
    const auto class_id = [&](const std::string& name) {
        if (const auto fnd = m_class_by_name.find(name); fnd != m_class_by_name.end())
            return fnd->second;
        ClassInfo info;
        info.name = name;
        const auto id = m_classes.push_back(std::move(info));
        m_class_by_name.emplace(name, id);
        return id;
    };

    std::set<std::string> symbol_literals;
    std::unordered_map<std::string, std::string> superclass_names;

    internals.compiler.depth_first_do([&](const ir::IRGraph& g) {
        const auto root = g.root();
        if (!root || !std::holds_alternative<ir::SCClass_I>(*root))
            return;
        const auto class_i = std::get<ir::SCClass_I>(*root);
        const auto& c = g.payload(class_i);
        const auto full_name = text(c.name);
        const bool meta = symbols.is_meta(c.name);
        const auto name = meta ? full_name.substr(meta_prefix.size()) : full_name;
        const auto id = class_id(name);
        if (!meta) {
            m_classes[id].location = name_location(g, g.location(class_i), name);
            if (c.superclass)
                superclass_names[name] = text(*c.superclass);
        }

        const auto [members, methods] = g.children(class_i);
        for (auto it = g.children(members).iter(); it; ++it) {
            const auto& m = g.payload(*it);
            if (m.owning_class != c.name)
                continue;
            MemberInfo info;
            info.name = text(m.name);
            info.kind = !meta ? MemberKind::Instance : m.is_const ? MemberKind::Constant : MemberKind::Class;
            info.getter = ast::has(m.accessor, ast::ReadWriteAccessor::ExternallyReadable);
            info.setter = ast::has(m.accessor, ast::ReadWriteAccessor::ExternallyWriteable);
            if (m.expr)
                info.value = literal(*m.expr);
            info.location = location_of(g, g.location(*it));
            // The getter and setter it declares, which have no method in the graph.
            for (const bool setter : { false, true }) {
                if (!(setter ? info.setter : info.getter) || info.kind == MemberKind::Constant)
                    continue;
                MethodInfo accessor;
                accessor.owner = id;
                accessor.class_method = meta;
                accessor.accessor = true;
                accessor.selector = info.name + (setter ? "_" : "");
                if (setter)
                    accessor.arguments.push_back({ info.name, {} });
                accessor.location = info.location;
                accessor.whole = info.location;
                const auto method_id = m_methods.push_back(std::move(accessor));
                (meta ? m_classes[id].class_methods : m_classes[id].methods).push_back(method_id);
                m_by_selector[m_methods[method_id].selector].push_back(method_id);
            }
            m_classes[id].members.push_back(std::move(info));
        }
        for (auto it = g.children(methods).iter(); it; ++it) {
            const auto method_i = *it;
            const auto& m = g.payload(method_i);
            MethodInfo info;
            info.owner = id;
            info.class_method = meta;
            info.extension = m.extension;
            info.selector = text(*m.selector);
            if (m.primitive)
                info.primitive = text(m.primitive->value);
            info.location = location_of(g, m.selector.location);
            info.whole = location_of(g, g.location(method_i));
            const auto [positional, variadic, body] = g.children(method_i);
            (void)body;
            for (auto a = g.children(positional).iter(); a; ++a) {
                const auto& arg = g.payload(*a);
                ArgumentInfo argument { text(arg.name), {} };
                if (arg.prototype) {
                    argument.default_value = literal(*arg.prototype);
                } else {
                    const auto [value] = g.children(*a);
                    if (!g.is_a<ir::SCMissing_I>(ir::NodeIndex { *value }))
                        argument.default_value = std::string { g.read(g.location(ir::NodeIndex { *value })) };
                }
                if (argument.default_value == "nil")
                    argument.default_value.clear();
                info.arguments.push_back(std::move(argument));
            }
            for (auto a = g.children(variadic).iter(); a; ++a)
                info.variadic = text(g.payload(*a).name);
            const auto method_id = m_methods.push_back(std::move(info));
            (meta ? m_classes[id].class_methods : m_classes[id].methods).push_back(method_id);
            m_by_selector[m_methods[method_id].selector].push_back(method_id);
        }

        g.depth_first_traverse(class_i,
                               [&](const ir::IRGraph::Edges&, const ir::IRGraph::Variant& payload,
                                   const ir::SourceLocation&, ir::NodeIndex, size_t) {
                                   if (const auto* lit = std::get_if<ir::SCLiteral>(&payload))
                                       if (const auto* sym = constants(lit->index).get_if<ir::SymSCSymbol>())
                                           symbol_literals.insert(text(*sym));
                               });
    });

    for (const auto& [name, super] : superclass_names) {
        const auto id = m_class_by_name.at(name);
        if (const auto fnd = m_class_by_name.find(super); fnd != m_class_by_name.end()) {
            m_classes[id].superclass = fnd->second;
            m_classes[fnd->second].subclasses.push_back(id);
        }
    }
    for (auto& c : m_classes.raw())
        std::sort(c.subclasses.begin(), c.subclasses.end(),
                  [&](ClassId l, ClassId r) { return m_classes[l].name < m_classes[r].name; });

    for (const auto& [selector, _] : m_by_selector)
        m_selector_names.push_back(selector);
    std::sort(m_selector_names.begin(), m_selector_names.end());
    m_symbols.assign(symbol_literals.begin(), symbol_literals.end());

    // Hooks: every class deriving from LSPHook, found from the hierarchy alone.
    if (const auto base = find_class(hook_base_class)) {
        for (const auto c : m_classes.indices()) {
            if (c == *base || !is_kind_of(c, *base))
                continue;
            HookInfo hook { c, {}, m_classes[c].location.path, 0 };
            for (const auto m : m_classes[c].class_methods)
                hook.selectors.push_back(m_methods[m].selector);
            std::error_code ec;
            hook.file_size = std::filesystem::file_size(hook.file, ec);
            m_classes[c].hook = m_hooks.push_back(std::move(hook));
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Lookups
////////////////////////////////////////////////////////////////////////////////

std::optional<ClassId> Library::find_class(std::string_view name) const {
    if (name.substr(0, meta_prefix.size()) == meta_prefix)
        name.remove_prefix(meta_prefix.size());
    if (const auto fnd = m_class_by_name.find(std::string { name }); fnd != m_class_by_name.end())
        return fnd->second;
    return std::nullopt;
}

const std::vector<MethodId>& Library::implementors(std::string_view selector) const {
    static const std::vector<MethodId> none;
    if (const auto fnd = m_by_selector.find(std::string { selector }); fnd != m_by_selector.end())
        return fnd->second;
    return none;
}

std::vector<ClassId> Library::superclasses(ClassId c) const {
    std::vector<ClassId> out;
    std::optional<ClassId> at = c;
    while (at && out.size() < m_classes.size()) {
        out.push_back(*at);
        at = m_classes[*at].superclass;
    }
    return out;
}

bool Library::is_kind_of(ClassId c, ClassId ancestor) const {
    for (const auto s : superclasses(c))
        if (s == ancestor)
            return true;
    return false;
}

std::optional<MethodId> Library::lookup(ClassId receiver, bool class_side, std::string_view selector) const {
    for (const auto c : superclasses(receiver))
        for (const auto m : class_side ? m_classes[c].class_methods : m_classes[c].methods)
            if (m_methods[m].selector == selector)
                return m;
    // A meta class's superclasses end in Class, then Object.
    if (class_side)
        if (const auto klass = find_class("Class"))
            return lookup(*klass, false, selector);
    return std::nullopt;
}

std::vector<MethodId> Library::responds_to(ClassId receiver, bool class_side) const {
    std::vector<MethodId> out;
    std::set<std::string_view> seen;
    const auto add = [&](ClassId from, bool side) {
        for (const auto c : superclasses(from))
            for (const auto m : side ? m_classes[c].class_methods : m_classes[c].methods)
                if (seen.insert(m_methods[m].selector).second)
                    out.push_back(m);
    };
    add(receiver, class_side);
    if (class_side)
        if (const auto klass = find_class("Class"))
            add(*klass, false);
    return out;
}

std::shared_ptr<const src::TextInfo> Library::text_of(const std::filesystem::path& normal) const {
    if (const auto fnd = m_texts.find(normal); fnd != m_texts.end())
        return fnd->second;
    return nullptr;
}

bool Library::files_changed() const {
    for (const auto& f : m_files) {
        if (f.overlaid)
            continue;
        std::error_code ec;
        if (std::filesystem::file_size(f.path, ec) != f.size || ec)
            return true;
        if (std::filesystem::last_write_time(f.path, ec) != f.modified || ec)
            return true;
    }
    // A new file in a module.
    std::string log;
    return find_files(m_config, log).size() != m_files.size() - m_extra_files;
}

std::vector<std::filesystem::path> default_modules(const std::filesystem::path& executable) {
    std::vector<std::filesystem::path> out;
    std::error_code ec;
    const auto exists = [&](const std::filesystem::path& p) { return std::filesystem::is_directory(p, ec); };
    const auto dir = executable.parent_path();
    // An install, a macOS bundle, a build tree next to the source, or the source tree itself.
    for (const auto& candidate :
         { dir / ".." / "share" / "SuperCollider" / "SCClassLibrary", dir / "SCClassLibrary",
           dir / ".." / "Resources" / "SCClassLibrary", dir / ".." / ".." / ".." / "SCClassLibrary",
           dir / ".." / ".." / "SCClassLibrary" }) {
        if (exists(candidate)) {
            out.push_back(normal_path(candidate));
            break;
        }
    }
    const auto env = [](const char* name) -> std::filesystem::path {
        const char* v = std::getenv(name);
        return v ? std::filesystem::u8path(v) : std::filesystem::path {};
    };
    std::vector<std::filesystem::path> extensions;
#if defined(__APPLE__)
    if (!env("HOME").empty())
        extensions.push_back(env("HOME") / "Library" / "Application Support" / "SuperCollider" / "Extensions");
    extensions.push_back("/Library/Application Support/SuperCollider/Extensions");
#elif defined(_WIN32)
    if (!env("LOCALAPPDATA").empty())
        extensions.push_back(env("LOCALAPPDATA") / "SuperCollider" / "Extensions");
    if (!env("APPDATA").empty())
        extensions.push_back(env("APPDATA") / "SuperCollider" / "Extensions");
#else
    if (!env("XDG_DATA_HOME").empty())
        extensions.push_back(env("XDG_DATA_HOME") / "SuperCollider" / "Extensions");
    else if (!env("HOME").empty())
        extensions.push_back(env("HOME") / ".local" / "share" / "SuperCollider" / "Extensions");
    extensions.push_back("/usr/share/SuperCollider/Extensions");
    extensions.push_back("/usr/local/share/SuperCollider/Extensions");
#endif
    for (const auto& e : extensions)
        if (exists(e))
            out.push_back(normal_path(e));
    return out;
}

std::filesystem::path executable_path(const char* argv0) {
    std::error_code ec;
#if defined(__linux__)
    const auto self = std::filesystem::read_symlink("/proc/self/exe", ec);
    if (!ec)
        return self;
#endif
    return normal_path(std::filesystem::u8path(argv0 ? argv0 : ""));
}

std::string Library::signature(MethodId id) const {
    const auto& m = m_methods[id];
    std::string out = m_classes[m.owner].name + (m.class_method ? ".*" : ":") + m.selector;
    if (m.arguments.empty() && m.variadic.empty())
        return out;
    out += '(';
    bool first = true;
    for (const auto& a : m.arguments) {
        if (!first)
            out += ", ";
        first = false;
        out += a.name;
        if (!a.default_value.empty())
            out += " = " + a.default_value;
    }
    if (!m.variadic.empty())
        out += std::string { first ? "" : ", " } + "..." + m.variadic;
    return out + ')';
}

std::string Library::where(const Location& l) {
    if (l.path.empty())
        return "(no file)";
    return l.path.u8string() + ":" + std::to_string(l.range.begin.line + 1) + ":"
        + std::to_string(l.range.begin.column + 1);
}

}
