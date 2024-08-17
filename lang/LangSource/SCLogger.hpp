// Created by jordan on 17/08/24.

// This logger should only be used for benchmarking.
// It is for creating flame graphs for supercollider method and primitive calls.
// It isn't fast, it's not perfect.
// To use:
/*
 *
 * Ensure sclogoptions::enable_logger is set to true. This is a compile-time switch.
 *
 * To start a logging session, call:
 * sclog::begin_logging();
 *
 *
 * For an auto scoped call use:
 * {
 *     // This call may be left here as setting enable_logger to false optimizes it away.
 *      auto l = sclog::log_scope("receiver", "method");
 *      ... code to be logged ...
 * }
 *
 * For a manual scope call:
 * sclog::begin_scope("receiver", "method");
 * and
 * sclog::end_scope("receiver", "method");
 *
 *
 *
 * To end a logging session, call:
 * sclog::end_logging();
 *
 * ... later ...
 * sclog::print_to(std::cout);
 *
 */

// Upload results to https://www.speedscope.app/ to see it.

#pragma once
#include <memory>
#include <string_view>
#include <vector>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <chrono>
#include <string>
#include <cstring>

namespace sclogoptions {
static constexpr bool enable_logger = true;
}

namespace sclog {

class LogScope;

static void begin_scope(const char* receiver, const char* method);
static void end_scope(const char* receiver, const char* method);

static void begin_logging();
static void end_logging();


template <typename T> static void print_log(T& t);

namespace details {

class Logger;

struct NullLogger {
    NullLogger(const char* receiver, const char* methodOrPrimitive) {};
    ~NullLogger() = default;
    NullLogger(const NullLogger&) = delete;
    NullLogger& operator=(const NullLogger&) = delete;
    NullLogger(NullLogger&&) = delete;
    NullLogger& operator=(NullLogger&&) = delete;
};

template <typename T> struct TypeWrap { using Type = T; };

template <typename T> auto get_nullable_type() {
    if constexpr (sclogoptions::enable_logger) {
        return TypeWrap<T> {};
    } else {
        return TypeWrap<NullLogger> {};
    }
}

template <typename T> using Nullable = typename decltype(get_nullable_type<T>())::Type;

class Logger {
public:
    static void reset() {
        Logger& l = get();
        l.name_counter = 0;
        l.names.clear();
        l.names_in_order.clear();
        l.entries.clear();
        l.node = nullptr;
        l.start_time = std::chrono::high_resolution_clock::now();
    }

    template <typename T> static void print_to(T& t) {
        Logger& l = get();
        t << R"({"exporter": "speedscope@1.20.0","name":"sclog.txt","activeProfileIndex":0,"$schema":"https://www.speedscope.app/file-format-schema.json","shared": {"frames": [)";

        for (size_t i { 0 }; i < l.names_in_order.size(); ++i) {
            t << R"({"name":")" << l.names_in_order[i] << "\"}";
            if (i != l.names_in_order.size() - 1)
                t << ",\n";
        }
        t << "]},";

        t << R"("profiles":[{"type":"evented","name":"sclog.txt","unit":"none","startValue":)";
        t << (l.entries.empty() ? 0 : l.entries.front().time_point) << ",";
        t << "\"endValue\":";
        t << (l.entries.empty() ? 0 : l.entries.back().time_point) << ",";
        t << "\"events\": [";

        for (size_t i { 0 }; i < l.entries.size(); ++i) {
            const Entry& e = l.entries[i];
            t << "{\"type\":";
            t << (e.type == Entry::Type::Open ? "\"O\"," : "\"C\",");
            t << "\"frame\":" << e.name_index << ",";
            t << "\"at\":" << e.time_point;
            t << "}";
            if (i != l.entries.size() - 1)
                t << ",";
            t << "\n";
        }
        t << "]}]}";
    }

    friend class ::sclog::LogScope;
    friend void ::sclog::begin_scope(const char* receiver, const char* method);
    friend void ::sclog::end_scope(const char* receiver, const char* method);

    template <typename T> friend void ::sclog::print_log(T& t);

    friend void ::sclog::begin_logging();
    friend void ::sclog::end_logging();

private:
    static Logger& get() {
        static Logger l {};
        return l;
    }

    struct Entry {
        enum class Type { Open, Close } type;
        uint32_t time_point;
        uint32_t name_index;
    };


    bool is_enabled = false;
    uint32_t name_counter { 0 };
    std::unordered_map<std::string, uint32_t> names; // owns names
    std::vector<std::string> names_in_order;

    decltype(std::chrono::high_resolution_clock::now()) start_time = std::chrono::high_resolution_clock::now();

    std::vector<Entry> entries;

    uint32_t get_name_index(const char* receiver, const char* method) {
        std::string n = receiver;
        n += "::";
        n += method;
        if (auto f = names.find(n); f != names.end()) {
            return f->second;
        }
        {
            auto index = name_counter;
            ++name_counter;
            names.insert({ n, index }); // Copy name here.
            names_in_order.push_back(std::move(n)); // Move name here.
            return index;
        }
    };


    struct Node {
        uint32_t name_index;
        uint32_t start_point;
        std::shared_ptr<Node> parent;
        std::vector<std::shared_ptr<Node>> children;
    };

    std::shared_ptr<Node> node;


    uint32_t get_now() {
        auto n = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<std::chrono::nanoseconds>(n - start_time).count();
    }

    void close_recur(Node& n) {
        std::for_each(n.children.rbegin(), n.children.rend(), [&](std::shared_ptr<Node>& nn) { close_recur(*nn); });
        n.children.clear();
        entries.push_back(Entry { Entry::Type::Close, get_now(), n.name_index });
    }
    void create(uint32_t name_index, Entry::Type t) {
        if (is_enabled && t == Entry::Type::Open) {
            if (node == nullptr) {
                node = std::make_shared<Node>(Node { name_index, get_now(), std::shared_ptr<Node> { nullptr },
                                                     std::vector<std::shared_ptr<Node>> {} });
            } else {
                node->children.push_back(std::make_shared<Node>(
                    Node { Node { name_index, get_now(), node, std::vector<std::shared_ptr<Node>> {} } }));
                node = node->children.back();
            }
            entries.push_back(Entry { t, node->start_point, name_index });
        } else if (t == Entry::Type::Close && node != nullptr) {
            if (node->name_index == name_index) {
                const auto rm = std::remove(node->parent->children.begin(), node->parent->children.end(), node);
                node->parent->children.erase(rm);
                node = node->parent; // go up a parent
                entries.push_back(Entry { t, get_now(), name_index });
            } else {
                std::shared_ptr<Node> to_close = node;
                while (true) {
                    if (to_close == nullptr)
                        return; // do nothing

                    if (to_close->name_index == name_index) {
                        break;
                    } else {
                        to_close = to_close->parent;
                    }
                }
                close_recur(*to_close);
                node = to_close->parent;
                if (node != nullptr) {
                    const auto rm = std::remove(node->children.begin(), node->children.end(), to_close);
                    node->children.erase(rm);
                }
            }
        }
    }
};

}

template <typename T> static void print_log(T& t) {
    details::Logger::get().print_to(t);
    details::Logger::reset();
}

static details::Nullable<LogScope> log_scope(const char* receiver, const char* methodOrPrimitive);

class LogScope {
public:
    LogScope(const LogScope&) = delete;
    LogScope& operator=(const LogScope&) = delete;
    LogScope(LogScope&&) = delete;
    LogScope& operator=(LogScope&&) = delete;

    friend details::Nullable<LogScope> log_scope(const char* receiver, const char* methodOrPrimitive);
    ~LogScope() { logger.create(name_index, details::Logger::Entry::Type::Close); }

private:
    LogScope(const char* receiver, const char* methodOrPrimitive):
        logger(details::Logger::get()),
        name_index(logger.get_name_index(receiver, methodOrPrimitive)) {
        logger.create(name_index, details::Logger::Entry::Type::Open);
    }

    details::Logger& logger;
    uint32_t name_index;
};

static details::Nullable<LogScope> log_scope(const char* receiver, const char* methodOrPrimitive) {
    return { receiver, methodOrPrimitive };
}

static void begin_scope(const char* receiver, const char* method) {
    if constexpr (sclogoptions::enable_logger) {
        auto& l = details::Logger::get();
        const auto i = l.get_name_index(receiver, method);
        l.create(i, details::Logger::Entry::Type::Open);
    }
}

static void end_scope(const char* receiver, const char* method) {
    if constexpr (sclogoptions::enable_logger) {
        auto& l = details::Logger::get();
        const auto i = l.get_name_index(receiver, method);
        l.create(i, details::Logger::Entry::Type::Close);
    }
}

static void begin_logging() {
    if constexpr (sclogoptions::enable_logger) {
        details::Logger::reset();
        auto& l = details::Logger::get();
        l.is_enabled = true;
        const auto i = l.get_name_index("ROOT", "LOGGER");
        l.create(i, details::Logger::Entry::Type::Open);
    }
}

static void end_logging() {
    if constexpr (sclogoptions::enable_logger) {
        auto& l = details::Logger::get();

        const auto i = l.get_name_index("ROOT", "LOGGER");
        l.create(i, details::Logger::Entry::Type::Close);
        l.is_enabled = false;
    }
}

}
