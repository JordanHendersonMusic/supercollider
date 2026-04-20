/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#include "PyrLexer.h"
#include "PyrSlot.h"
#include "PyrKernel.h"
#include "SC_Version.hpp"
#include "AdvancingAllocPool.h"
#include "SpecialSelectorsOperatorsAndClasses.h"
#include "lexer.hpp"
#include <exception>
#include <type_traits>

using Location = sc::lex::SourceCodeRange;

enum struct ReadWriteAccessor {
    Private,
    Read,
    Write,
    ReadWrite,
};

[[nodiscard]] inline bool has_read(ReadWriteAccessor r) {
    return r == ReadWriteAccessor::Read || r == ReadWriteAccessor::ReadWrite;
}

[[nodiscard]] inline bool has_write(ReadWriteAccessor r) {
    return r == ReadWriteAccessor::Write || r == ReadWriteAccessor::ReadWrite;
}

enum { varInst, varClass, varTemp, varConst, varPseudo, varLocal };

enum struct PyrParseNodeType : char {
    ClassNode,
    ClassExtNode,
    MethodNode,
    BlockNode,
    SlotNode,

    VarListNode,
    VarDefNode,
    DynDictNode,
    DynListNode,
    LitListNode,

    ArgListNode,

    LiteralNode,

    PushLitNode,
    PushNameNode,
    PushKeyArgNode,
    CallNode,
    BinopCallNode,
    DropNode,
    AssignNode,
    MultiAssignNode,
    MultiAssignVarListNode,
    SetterNode,
    CurryArgNode,

    StringLine,
    String,

    ReturnNode,
    BlockReturnNode,
};


// This value count the un-inlined functions, these are not desirable in the class library because they are slow.
// There is a primitive that returns this value so it can be checked in sclang's unit tests.
extern int gNumUninlinedFunctions;

extern AdvancingAllocPool gParseNodePool;

// This is how you create parse nodes, this is the only way to do so.
// It allocates them into gParseNodePool.
template <typename T, typename... ARGS> T* allocParseNode(Location loc, ARGS... args);

struct PyrParseNode {
protected:
    struct Tag {
    private:
        Tag() {};

    public:
        template <typename T, typename... ARGS> friend T* allocParseNode(Location loc, ARGS... args);
    };

    PyrParseNode(Tag, PyrParseNodeType classno, Location loc);

public:
    PyrParseNode() = delete;
    PyrParseNode(PyrParseNode&&) = delete;
    PyrParseNode(const PyrParseNode&) = delete;
    PyrParseNode& operator=(PyrParseNode&&) = delete;
    PyrParseNode& operator=(const PyrParseNode&) = delete;

    // TODO: remove the out arg here, instead call getConstant first.
    virtual void compile(PyrSlot* result) = 0;
    virtual void dump(int level) = 0;

    // TODO: this is currently unimplemented.
    // Can be nullptr, always check.
    virtual PyrSlot* getConstant() { return nullptr; }

    PyrParseNode* mNext;
    PyrParseNode* mTail;
    Location location; // location in the source (not the file)
    PyrParseNodeType mClassno;
    unsigned char mParens { 0 };

    [[nodiscard]] sc::lex::FileCodeRange locationInFile() const;

    template <typename T, typename... ARGS> friend T* allocParseNode(Location loc, ARGS... args);
};

template <typename T> T* node_cast(PyrParseNode* n) {
    static_assert(std::is_base_of_v<PyrParseNode, T>);
    static_assert(std::is_final_v<T>);
    if (!n)
        return nullptr;
    for (const auto t : T::types)
        if (n->mClassno == t)
            return static_cast<T*>(n);
    return nullptr;
}

template <typename T, typename... ARGS> T* allocParseNode(Location loc, ARGS... args) {
    static_assert(std::is_trivially_destructible_v<T>);
    static_assert(std::is_base_of_v<PyrParseNode, T>);

    T* r = new (gParseNodePool.Alloc(sizeof(T))) T { PyrParseNode::Tag {}, loc, std::forward<ARGS>(args)... };
    // Should we throw instead? At this point everything is broken.
    if (!r)
        std::terminate();
    return r;
}


struct PyrSlotNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::SlotNode, PyrParseNodeType::PushLitNode,
                                        PyrParseNodeType::PushNameNode, PyrParseNodeType::LiteralNode };
    PyrSlotNode(Tag t, Location loc, PyrSlot slot, PyrParseNodeType subtype = PyrParseNodeType::SlotNode);

    virtual void compile(PyrSlot* result);
    virtual void compileLiteral(PyrSlot* result);
    virtual void compilePushLit(PyrSlot* result);
    virtual void dump(int level);
    virtual void dumpLiteral(int level);
    virtual void dumpPushLit(int level);

    // Changes the 'type' of the node. Only changes the type flag.
    template <PyrParseNodeType Target> PyrSlotNode* changeLiteralType() {
        static_assert(Target == types[1] || Target == types[2] || Target == types[3]);
        mClassno = Target;
        return this;
    }

    PyrSlot mSlot;
};


// struct PyrLiteralProducerNode : public PyrParseNode {};
// struct PyrLiteralStringLineNode final : public PyrLiteralProducerNode {
//     PyrLiteralStringLineNode(Tag t, Location l): PyrLiteralProducerNode(t, PyrParseNodeType::StringLine, l) {}
// };

// struct PyrLiteralStringNode final : public PyrLiteralProducerNode {
//     PyrLiteralStringNode(Tag t, Location l, PyrLiteralStringLineNode* lines):
//         PyrLiteralProducerNode(t, PyrParseNodeType::String, l),
//         lines(lines) {
//         assert(lines);
//     }
//     PyrLiteralStringLineNode* lines;
// };

// struct PyrLiteralSymbolNode final : public PyrLiteralProducerNode {};
// struct PyrLiteralFloatNode final : public PyrLiteralProducerNode {};
// struct PyrLiteralIntegerNode final : public PyrLiteralProducerNode {};
// struct PyrLiteralASCIINode final : public PyrLiteralProducerNode {};
// struct PyrLiteralBooleanNode final : public PyrLiteralProducerNode {};
// struct PyrLiteralNilNode final : public PyrLiteralProducerNode {};


// // Produces a constant expression by wrapping a literal.
// struct PyrConstantLiteralNode final : public PyrParseNode {};

struct PyrCurryArgNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::CurryArgNode };
    PyrCurryArgNode(Tag t, Location loc);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    int mArgNum { -1 };
};


struct PyrClassExtNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::ClassExtNode };
    PyrClassExtNode(Tag t, Location l, PyrSlotNode* classname, struct PyrMethodNode* methods);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mClassName;
    struct PyrMethodNode* mMethods;
};

struct PyrClassNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::ClassNode };
    PyrClassNode(Tag t, Location l, struct PyrSlotNode* mClassName, struct PyrSlotNode* mSuperClassName,
                 struct PyrSlotNode* mIndexType, struct PyrVarListNode* mVarlists, struct PyrMethodNode* mMethods);

    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mClassName;
    struct PyrSlotNode* mSuperClassName;
    struct PyrSlotNode* mIndexType;
    struct PyrVarListNode* mVarlists;
    struct PyrMethodNode* mMethods;
    int mVarTally[4] = {
        0,
        0,
        0,
        0,
    };
    int mNumSuperInstVars { 0 };
};

struct PyrMethodNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::MethodNode };
    PyrMethodNode(Tag t, Location l, PyrSlotNode* mMethodName, PyrSlotNode* mPrimitiveName,
                  struct PyrArgListNode* mArglist, struct PyrVarListNode* mVarlist, PyrParseNode* mBody,
                  bool mIsClassMethod, bool mExtension = false);

    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mMethodName;
    struct PyrSlotNode* mPrimitiveName;
    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    struct PyrParseNode* mBody;
    int mIsClassMethod; // is class method?
    bool mExtension;
};

struct PyrVarListNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::VarListNode };
    PyrVarListNode(Tag t, Location l, struct PyrVarDefNode* mVarDefs, int mFlags);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrVarDefNode* mVarDefs;
    int mFlags;
};

struct PyrVarDefNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::VarDefNode };
    PyrVarDefNode(Tag t, Location l, PyrSlotNode* mVarName, PyrParseNode* mDefVal, ReadWriteAccessor mFlags);

    virtual void compile(PyrSlot* result);
    virtual void compileArg(PyrSlot* result);
    virtual void dump(int level);
    bool hasExpr(PyrSlot* result);

    struct PyrSlotNode* mVarName;
    PyrParseNode* mDefVal;
    ReadWriteAccessor accessor;
    bool mDrop { true };
};

struct PyrCallNodeBase : public PyrParseNode {
    PyrCallNodeBase(Tag t, Location l, PyrParseNodeType classno);

    virtual void compile(PyrSlot* result);
    virtual void compilePartialApplication(int numCurryArgs, PyrSlot* result);
    virtual void compileCall(PyrSlot* result) = 0;

    virtual int isPartialApplication() = 0;
};

struct PyrCallNodeBase2 : public PyrCallNodeBase {
    PyrCallNodeBase2(Tag tg, Location l, PyrParseNodeType t, PyrSlotNode* mSelector, PyrParseNode* mArglist,
                     PyrParseNode* mKeyarglist);

    PyrSlotNode* mSelector;
    PyrParseNode* mArglist;
    PyrParseNode* mKeyarglist;
    bool mTailCall { false };
};

struct PyrCallNode final : public PyrCallNodeBase2 {
    static constexpr std::array types { PyrParseNodeType::CallNode };
    PyrCallNode(Tag t, Location l, PyrSlotNode* mSelector, PyrParseNode* mArglist, PyrParseNode* mKeyarglist = nullptr);

    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();
};

struct PyrBinopCallNode final : public PyrCallNodeBase2 {
    static constexpr std::array types { PyrParseNodeType::BinopCallNode };
    PyrBinopCallNode(Tag t, Location l, PyrSlotNode* mSelector, PyrParseNode* arglist);

    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();
};

struct PyrSetterNode final : public PyrCallNodeBase {
    static constexpr std::array types { PyrParseNodeType::SetterNode };
    PyrSetterNode(Tag t, Location l, PyrSlotNode* mSelector, PyrParseNode* mExpr1, PyrParseNode* mExpr2);

    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    PyrSlotNode* mSelector;
    PyrParseNode* mExpr1;
    PyrParseNode* mExpr2;
    int mFlags; // is a var def ?
};

struct PyrDynListNode final : public PyrCallNodeBase {
    static constexpr std::array types { PyrParseNodeType::DynListNode };
    PyrDynListNode(Tag t, Location l, PyrParseNode* mClassname, PyrParseNode* mElems);
    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    PyrParseNode* mClassname;
    PyrParseNode* mElems;
};

struct PyrDynDictNode final : public PyrCallNodeBase {
    static constexpr std::array types { PyrParseNodeType::DynDictNode };
    PyrDynDictNode(Tag t, Location l, PyrParseNode* mElems);
    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    PyrParseNode* mElems;
};


struct PyrDropNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::DropNode };
    PyrDropNode(Tag t, Location l, PyrParseNode* e1, PyrParseNode* e2);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrParseNode* mExpr1;
    PyrParseNode* mExpr2;
};

struct PyrPushKeyArgNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::PushKeyArgNode };
    PyrPushKeyArgNode(Tag t, Location l, PyrSlotNode* mSelector, PyrParseNode* mExpr);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrSlotNode* mSelector;
    PyrParseNode* mExpr;
};

struct PyrReturnNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::ReturnNode };
    PyrReturnNode(Tag t, Location l, PyrParseNode* mExpr);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrParseNode* mExpr; // if null, return self
};

struct PyrBlockReturnNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::BlockReturnNode };
    PyrBlockReturnNode(Tag t, Location l, PyrParseNode* mExpr);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrParseNode* mExpr; // if null, return self
};

struct PyrAssignNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::AssignNode };
    PyrAssignNode(Tag t, Location l, PyrSlotNode* mVarName, PyrParseNode* mExpr);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrSlotNode* mVarName;
    PyrParseNode* mExpr;
    bool mDrop { false }; // allow drop
};

struct PyrMultiAssignNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::MultiAssignNode };
    PyrMultiAssignNode(Tag t, Location l, struct PyrMultiAssignVarListNode* mVarList, PyrParseNode* mExpr);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrMultiAssignVarListNode* mVarList;
    PyrParseNode* mExpr;
    bool mDrop { false }; // allow drop
};

struct PyrMultiAssignVarListNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::MultiAssignVarListNode };
    PyrMultiAssignVarListNode(Tag t, Location l, PyrSlotNode* mVarNames, PyrSlotNode* mRest);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrSlotNode* mVarNames;
    PyrSlotNode* mRest;
};

struct PyrBlockNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::BlockNode };
    PyrBlockNode(Tag t, Location l, struct PyrArgListNode* mArglist, struct PyrVarListNode* mVarlist,
                 PyrParseNode* mBody, bool mIsTopLevel);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    PyrParseNode* mBody;
    bool mIsTopLevel;
};

struct PyrArgListNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::ArgListNode };
    PyrArgListNode(Tag t, Location l, struct PyrVarDefNode* mVarDefs, PyrSlotNode* mRest, PyrSlotNode* mKeywordArgs);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrVarDefNode* mVarDefs;
    PyrSlotNode* mRest;
    PyrSlotNode* mKeywordArgs;
};

struct PyrLitListNode final : public PyrParseNode {
    static constexpr std::array types { PyrParseNodeType::LitListNode };
    PyrLitListNode(Tag t, Location l, PyrParseNode* mClassname, PyrParseNode* mElems);
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    PyrParseNode* mClassname;
    PyrParseNode* mElems;
};

extern PyrParseNode* gRootParseNode;
extern bool gIsTailCodeBranch;
extern bool gTailIsMethodReturn;


class SetTailBranch {
    bool mSave;

public:
    SetTailBranch(bool inValue) {
        mSave = gIsTailCodeBranch;
        gIsTailCodeBranch = inValue;
    }
    ~SetTailBranch() { gIsTailCodeBranch = mSave; }
};

inline void compileNode(PyrParseNode* node, PyrSlot* result, bool onTailBranch) {
    SetTailBranch branch(gIsTailCodeBranch && onTailBranch);
    node->compile(result);
}


int nodeListLength(PyrParseNode* node);
bool isSuperObjNode(PyrParseNode* node);

void compileNodeList(PyrParseNode* node, bool onTailBranch);

void initParser();
void initParserPool();
void freeParserPool();

void initSpecialSelectors();
void initSpecialClasses();

void printErrorLine(PyrParseNode* node, const char* short_description = nullptr);

inline PyrParseNode* linkAfterHead(PyrParseNode* a, PyrParseNode* b) {
    b->mNext = a->mNext;
    if (!a->mNext)
        a->mTail = b;
    a->mNext = b;
    return a;
}

template <typename T, typename... NODES> T* linkNodes(T* first, NODES*... nodes) {
    static_assert(std::is_base_of_v<PyrParseNode, T>);
    static_assert((std::is_base_of_v<PyrParseNode, NODES> && ...));
    const auto link = [](PyrParseNode* a, PyrParseNode* b) {
        if (a == nullptr)
            return b;
        if (b) {
            a->mTail->mNext = b;
            a->mTail = b->mTail;
        }
        return a;
    };
    PyrParseNode* rolling { first };
    ((rolling = link(rolling, nodes)), ...);
    return static_cast<T*>(rolling);
}


extern int compileErrors;

extern int numOverwrites;
extern std::string overwriteMsg;

extern PyrSymbol* gSpecialUnarySelectors[opNumUnarySelectors];
extern PyrSymbol* gSpecialBinarySelectors[opNumBinarySelectors];
extern PyrSymbol* gSpecialSelectors[opmNumSpecialSelectors];
extern PyrSymbol* gSpecialClasses[op_NumSpecialClasses];

extern PyrClass* gCurrentClass;
extern PyrClass* gCurrentMetaClass;
extern PyrClass* gCompilingClass;
extern PyrMethod* gCompilingMethod;
extern PyrBlock* gCompilingBlock;

// #define YYSTYPE intptr_t
