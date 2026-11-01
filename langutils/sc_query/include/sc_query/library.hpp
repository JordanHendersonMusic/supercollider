#pragma once

/// @file
/// @brief A compiled class library, and an index of its classes, methods and symbols for the queries.

#include "sc_query/text.hpp"

#include "sc_util/indexed_vector.hpp"
#include "sc_util/language_version.hpp"
#include "sc_util/strong_index.hpp"

#include <chrono>
#include <cstdint>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "sc_diagnostic/sc_diagnostic.hpp"
#include "sc_source/text_info.hpp"

namespace sc::compiler {
struct ClassLibrary;
}

namespace sc::query {

using ClassId = util::StrongIndex<std::uint32_t, struct ClassId__>;
using MethodId = util::StrongIndex<std::uint32_t, struct MethodId__>;
using HookId = util::StrongIndex<std::uint32_t, struct HookId__>;

class Library;

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief Where the class library is, and how interpreted code is compiled against it.
struct LibraryConfig {
    /// Each is a module: a directory searched for .sc files, or one .sc file.
    std::vector<std::filesystem::path> modules {};
    /// Files and directories left out.
    std::vector<std::filesystem::path> excludes {};
    /// The language version of interpreted code, as sclang's default.
    ast::LanguageVersion interpreted_version { ast::LanguageVersion::V3_16 };
    std::size_t num_threads { 0 }; ///< Zero is one per core.
};

/// @brief The text of class files that differs from the files on disk, e.g., an editor's unsaved text, by normal_path.
using Overlays = std::map<std::filesystem::path, std::string>;

/// @brief Where sclang looks by default: the class library installed beside @p executable, then the user's and the
/// system's Extensions directories, those that exist.
[[nodiscard]] std::vector<std::filesystem::path> default_modules(const std::filesystem::path& executable);
/// @brief The running program's path, from @p argv0 where the system can't say.
[[nodiscard]] std::filesystem::path executable_path(const char* argv0);

/// @brief The name of the class every hook derives from.
inline constexpr std::string_view hook_base_class = "LSPHook";

/// @brief Class files that aren't in the library's modules, e.g., a snippet to check, by path.
using ExtraSources = std::vector<std::pair<std::filesystem::path, std::string>>;

/// @brief Compiles the class library, reading files from disk unless an overlay has their text.
/// @param extra Compiled as one more module, after the configured ones.
[[nodiscard]] std::shared_ptr<Library> build_library(const LibraryConfig& config, const Overlays& overlays = {},
                                                     const ExtraSources& extra = {});

/// @brief An argument of a method.
struct ArgumentInfo {
    std::string name;
    std::string default_value; ///< As written, empty if it has none.
};

/// @brief A method, an instance method or a class method.
struct MethodInfo {
    ClassId owner { 0 }; ///< The class it is in, never a meta class.
    bool class_method { false }; ///< Whether it is a class method, `*new`.
    bool extension { false }; ///< Whether it is in a class extension.
    bool accessor { false }; ///< A getter or setter a variable declares with `<` or `>`.
    std::string selector;
    std::vector<ArgumentInfo> arguments;
    std::string variadic; ///< The variadic argument's name, `...rest`, empty if it has none.
    std::string primitive; ///< The primitive it calls, empty if none.
    Location location; ///< Its name.
    Location whole; ///< The whole method.
};

/// @brief What kind of variable a member is.
enum struct MemberKind : std::uint8_t { Instance, Class, Constant };

/// @brief A variable a class declares.
struct MemberInfo {
    std::string name;
    MemberKind kind { MemberKind::Instance };
    bool getter { false }; ///< Declared with `<`.
    bool setter { false }; ///< Declared with `>`.
    std::string value; ///< The constant it starts as, empty if none or not constant.
    Location location;
};

/// @brief A class, together with its meta class's methods.
struct ClassInfo {
    std::string name;
    std::optional<ClassId> superclass;
    std::vector<ClassId> subclasses; ///< Sorted by name.
    Location location; ///< Its name, where the class is declared.
    std::vector<MethodId> methods; ///< Instance methods, including extensions.
    std::vector<MethodId> class_methods; ///< Class methods, including extensions.
    std::vector<MemberInfo> members; ///< Its own, not inherited ones.
    std::optional<HookId> hook; ///< Set when it derives from LSPHook.
};

/// @brief A class that derives from LSPHook, found from the hierarchy, without running anything.
struct HookInfo {
    ClassId hook_class { 0 };
    std::vector<std::string> selectors; ///< Its own class methods, e.g., `completeSymbol`.
    std::filesystem::path file; ///< The file declaring it.
    std::uintmax_t file_size { 0 }; ///< That file's size on disk, to tell whether sclang compiled the same file.
};

/// @brief A file the library was read from, to tell when it changed.
struct FileStamp {
    std::filesystem::path path;
    std::uintmax_t size { 0 };
    std::filesystem::file_time_type modified {};
    bool overlaid { false }; ///< Its text came from an overlay, not the disk.
};

/// @brief A compiled class library and its index. Only read once it is built, apart from compiling interpreted code
/// against it, which needs it to itself.
class Library {
public:
    Library();
    ~Library();
    Library(const Library&) = delete;
    Library& operator=(const Library&) = delete;

    /// @brief The compiled library, for compiling interpreted code.
    [[nodiscard]] compiler::ClassLibrary& compiled() { return *m_library; }
    [[nodiscard]] const compiler::ClassLibrary& compiled() const { return *m_library; }
    [[nodiscard]] const LibraryConfig& config() const { return m_config; }

    [[nodiscard]] const util::IndexedVector<ClassId, ClassInfo>& classes() const { return m_classes; }
    [[nodiscard]] const ClassInfo& get(ClassId c) const { return m_classes[c]; }
    [[nodiscard]] const util::IndexedVector<MethodId, MethodInfo>& methods() const { return m_methods; }
    [[nodiscard]] const MethodInfo& get(MethodId m) const { return m_methods[m]; }
    [[nodiscard]] const util::IndexedVector<HookId, HookInfo>& hooks() const { return m_hooks; }

    /// @brief The class named @p name, a meta class's name gives its class.
    [[nodiscard]] std::optional<ClassId> find_class(std::string_view name) const;
    /// @brief Every method with this selector, instance and class methods.
    [[nodiscard]] const std::vector<MethodId>& implementors(std::string_view selector) const;
    /// @brief The method a message to an instance of the class, or the class itself, dispatches to.
    [[nodiscard]] std::optional<MethodId> lookup(ClassId receiver, bool class_side, std::string_view selector) const;
    /// @brief Every selector the receiver responds to, inherited ones included, each once.
    [[nodiscard]] std::vector<MethodId> responds_to(ClassId receiver, bool class_side) const;
    /// @brief The class and its superclasses, the class first.
    [[nodiscard]] std::vector<ClassId> superclasses(ClassId c) const;
    /// @brief Whether @p c is @p ancestor or derives from it.
    [[nodiscard]] bool is_kind_of(ClassId c, ClassId ancestor) const;

    /// @brief Every selector, sorted.
    [[nodiscard]] const std::vector<std::string>& selectors() const { return m_selector_names; }
    /// @brief Every symbol literal in the library, sorted.
    [[nodiscard]] const std::vector<std::string>& symbols() const { return m_symbols; }

    /// @brief What parsing and compiling found.
    [[nodiscard]] const std::vector<diag::Diagnostic>& diagnostics() const { return m_diagnostics; }
    /// @brief The text a class file was compiled from, null if it isn't in the library.
    [[nodiscard]] std::shared_ptr<const src::TextInfo> text_of(const std::filesystem::path& normal) const;
    /// @brief Every file read.
    [[nodiscard]] const std::vector<FileStamp>& files() const { return m_files; }
    /// @brief Whether a file on disk changed since it was read, ignoring overlaid ones.
    [[nodiscard]] bool files_changed() const;
    /// @brief Lines from reading the files, e.g., files that couldn't be read.
    [[nodiscard]] const std::string& log() const { return m_log; }
    /// @brief How long it took to build.
    [[nodiscard]] double build_seconds() const { return m_build_seconds; }

    /// @brief `Foo:bar(a, b = 1)` or `Foo.*new(...)` for a method.
    [[nodiscard]] std::string signature(MethodId m) const;
    /// @brief `file:line` of a location, relative to nothing.
    [[nodiscard]] static std::string where(const Location& l);

private:
    friend std::shared_ptr<Library> build_library(const LibraryConfig&, const Overlays&, const ExtraSources&);
    void index();

    LibraryConfig m_config {};
    std::unique_ptr<compiler::ClassLibrary> m_library;
    util::IndexedVector<ClassId, ClassInfo> m_classes {};
    util::IndexedVector<MethodId, MethodInfo> m_methods {};
    util::IndexedVector<HookId, HookInfo> m_hooks {};
    std::unordered_map<std::string, ClassId> m_class_by_name {};
    std::unordered_map<std::string, std::vector<MethodId>> m_by_selector {};
    std::vector<std::string> m_selector_names {};
    std::vector<std::string> m_symbols {};
    std::vector<diag::Diagnostic> m_diagnostics;
    std::map<std::filesystem::path, std::shared_ptr<const src::TextInfo>> m_texts {};
    std::vector<FileStamp> m_files {};
    std::size_t m_extra_files { 0 };
    std::string m_log {};
    double m_build_seconds { 0 };
};

}
