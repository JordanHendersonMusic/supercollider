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

#include "ByteCodeArray.h"
#include "PyrLexer.h"
#include "PyrSlot.h"
#include "PyrKernel.h"
#include "SC_Version.hpp"
#include "ByteCodeArray.h"
#include "Opcodes.h"
#include "AdvancingAllocPool.h"
#include "SpecialSelectorsOperatorsAndClasses.h"
#include <cassert>
#include <type_traits>

// TODO: remove these macros.
#define COMPILENODE(node, result, onTailBranch) (compileNode((node), (result), (onTailBranch)))
#define DUMPNODE(node, level)                                                                                          \
    do {                                                                                                               \
        if (node)                                                                                                      \
            (node)->dump(level);                                                                                       \
    } while (false);


enum { rwPrivate = 0, rwReadOnly = 1, rwWriteOnly = 2, rwReadWrite = 3 };

// Strongly typed version of above. Used in grammar to name return type of rules.
enum struct ReadWriteAccessor {
    Private = rwPrivate,
    Read = rwReadOnly,
    Write = rwWriteOnly,
    ReadWrite = rwReadWrite,
};

enum { varInst, varClass, varTemp, varConst, varPseudo, varLocal };

enum ParseNodeEnum : unsigned char {
    /* structural units */
    pn_ClassNode,
    pn_ClassExtNode,
    pn_MethodNode,
    pn_BlockNode,
    pn_SlotNode,

    /* variable declarations */
    pn_VarListNode,
    pn_VarDefNode,
    pn_DynDictNode,
    pn_DynListNode,
    pn_LitListNode,
    pn_LitDictNode,

    pn_StaticVarListNode,
    pn_InstVarListNode,
    pn_PoolVarListNode,
    pn_ArgListNode,
    pn_SlotDefNode,

    /* selectors */
    pn_LiteralNode,

    /* code */
    pn_PushLitNode,
    pn_PushNameNode,
    pn_PushKeyArgNode,
    pn_CallNode,
    pn_BinopCallNode,
    pn_DropNode,
    pn_AssignNode,
    pn_MultiAssignNode,
    pn_MultiAssignVarListNode,
    pn_SetterNode,
    pn_CurryArgNode,

    pn_ReturnNode,
    pn_BlockReturnNode,

    pn_NumTypes
};

// Base class of all parse nodes.
// Because the parser does not do any memory management, we require all parse nodes to be arena/pool allocated.
struct PyrParseNode {
    // Pinned type. Does not move. Pointer stability must be guaranteed.
    PyrParseNode() = delete;
    PyrParseNode(PyrParseNode&&) = delete;
    PyrParseNode(const PyrParseNode&) = delete;
    PyrParseNode& operator=(PyrParseNode&&) = delete;
    PyrParseNode& operator=(const PyrParseNode&) = delete;

    // Currently the virtual destructor is never called due to how the parse node pool allocated works.
    // There is a static_assert in the allocNode function to ensure all derived classes are trivial to destruct.

    virtual void compile(PyrSlot* result) = 0;
    virtual void dump(int level) = 0;

    [[nodiscard]] LocationInSourceCode location() const;

    PyrParseNode* mNext { nullptr };
    PyrParseNode* mTail { this }; // not setting this will cause all sorts of weird bugs.
    sc::lex::SourceCodeLocation mLocation;
    ParseNodeEnum mClassno;
    bool mParens { false }; // parentheses

    // This is how you make a node.
    template <typename T, typename... ARGS> friend T* allocNode(ARGS&&... args);

protected:
    struct TAG {};
    // Only useable constructor.
    PyrParseNode(TAG, sc::lex::SourceCodeLocation location, ParseNodeEnum classno);
};

// The PyrSlotNode supports four 'sub' types, indicated with the ParseNodeEnum: pn_SlotNode, pn_LiteralNode,
// pn_PushLitNode, and pn_PushNameNode. In future these could be their own types, however, the parser often transmutates
// one to the other without reallocating the node. To avoid doing some crazy casting of 'this' (`new (this)
// PushNameName(...)`), the enum is used as a switch.
struct PyrSlotNode : public PyrParseNode {
    PyrSlotNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation location, PyrSlot slot = {},
                ParseNodeEnum classno = pn_SlotNode);

    PyrSlotNode* changeLiteralType(ParseNodeEnum e);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    virtual void compileLiteral(PyrSlot* result);
    virtual void compilePushLit(PyrSlot* result);
    virtual void dumpLiteral(int level);
    virtual void dumpPushLit(int level);

    PyrSlot mSlot;
};

struct PyrCurryArgNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_CurryArgNode };
    PyrCurryArgNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, int argNum = -1);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    int mArgNum;
};

struct PyrClassExtNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_ClassExtNode };
    PyrClassExtNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* classname,
                    struct PyrMethodNode* methods);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mClassName;
    struct PyrMethodNode* mMethods;
};

struct PyrClassNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_ClassNode };
    PyrClassNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* className,
                 PyrSlotNode* superClassName, PyrSlotNode* indexType, struct PyrVarListNode* varlists,
                 struct PyrMethodNode* methods);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mClassName;
    PyrSlotNode* mSuperClassName;
    PyrSlotNode* mIndexType;
    struct PyrVarListNode* mVarlists;
    struct PyrMethodNode* mMethods;

    // This uses the enums varInst, varClass, varTemp, and varConst to count the number of instance variables.
    int mVarTally[4] { 0, 0, 0, 0 };
    int mNumSuperInstVars { 0 };
};

struct PyrMethodNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_MethodNode };
    PyrMethodNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* methodName,
                  PyrSlotNode* primitiveName, struct PyrArgListNode* arglist, struct PyrVarListNode* varlist,
                  PyrParseNode* body, bool isClassMethod);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mMethodName;
    PyrSlotNode* mPrimitiveName;
    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    struct PyrParseNode* mBody;

    bool mIsClassMethod; // is class method?
    bool mExtension { false };
};

struct PyrVarListNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_VarListNode };
    PyrVarListNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, struct PyrVarDefNode* vardef, int flags);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    struct PyrVarDefNode* mVarDefs;
    int mFlags;
};

struct PyrVarDefNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_VarDefNode };

    PyrVarDefNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* varname, PyrParseNode* defVal,
                  ReadWriteAccessor rwAccessor);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    void compileArg(PyrSlot* result);
    bool hasExpr(PyrSlot* result);

    PyrSlotNode* mVarName;
    PyrParseNode* mDefVal;
    int mFlags;
    bool mDrop;
};

struct PyrCallNodeBase : public PyrParseNode {
    PyrCallNodeBase(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, ParseNodeEnum classno);

    void compile(PyrSlot* result) override;

    virtual void compilePartialApplication(int numCurryArgs, PyrSlot* result);
    virtual void compileCall(PyrSlot* result) = 0;
    virtual int isPartialApplication() = 0;
};

struct PyrCallNodeBase2 : public PyrCallNodeBase {
    PyrCallNodeBase2(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, ParseNodeEnum classno, PyrSlotNode* selector,
                     PyrParseNode* arglist, PyrParseNode* keyarglist);

    PyrSlotNode* mSelector;
    PyrParseNode* mArglist;
    PyrParseNode* mKeyarglist;
    bool mTailCall { false };
};

struct PyrCallNode : public PyrCallNodeBase2 {
    static constexpr ParseNodeEnum nodeEnum { pn_CallNode };
    PyrCallNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* selector, PyrParseNode* arglist,
                PyrParseNode* keyarglist);

    void dump(int level) override;

    void compileCall(PyrSlot* result) override;
    int isPartialApplication() override;
};

struct PyrBinopCallNode : public PyrCallNodeBase2 {
    static constexpr ParseNodeEnum nodeEnum { pn_BinopCallNode };
    PyrBinopCallNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* selector, PyrParseNode* arglist);

    void dump(int level) override;

    void compileCall(PyrSlot* result) override;
    int isPartialApplication() override;
};

struct PyrSetterNode : public PyrCallNodeBase {
    static constexpr ParseNodeEnum nodeEnum { pn_SetterNode };
    PyrSetterNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* selector, PyrParseNode* expr1,
                  PyrParseNode* expr2);

    void dump(int level) override;

    void compileCall(PyrSlot* result) override;
    int isPartialApplication() override;

    PyrSlotNode* mSelector;
    PyrParseNode* mExpr1;
    PyrParseNode* mExpr2;
    int mFlags { 0 }; // is a var def ?
};

struct PyrDynListNode : public PyrCallNodeBase {
    static constexpr ParseNodeEnum nodeEnum { pn_DynListNode };
    PyrDynListNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* className, PyrParseNode* elems);

    void dump(int level) override;

    void compileCall(PyrSlot* result) override;
    int isPartialApplication() override;

    PyrSlotNode* mClassname;
    PyrParseNode* mElems;
};

struct PyrDynDictNode : public PyrCallNodeBase {
    static constexpr ParseNodeEnum nodeEnum { pn_DynDictNode };
    PyrDynDictNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrParseNode* elems);

    void dump(int level) override;

    void compileCall(PyrSlot* result) override;
    int isPartialApplication() override;

    PyrParseNode* mElems;
};

struct PyrDropNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_DropNode };
    PyrDropNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrParseNode* expr1, PyrParseNode* expr2);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    struct PyrParseNode* mExpr1;
    struct PyrParseNode* mExpr2;
};

struct PyrPushKeyArgNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_PushKeyArgNode };
    PyrPushKeyArgNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* selector, PyrParseNode* expr);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mSelector;
    PyrParseNode* mExpr;
};

// When refactoring for var in middle of blocks, these return nodes should be unified,
//  and the new PyrBlockNode and PyrMethodNode should inspect them and compile them differently.
struct PyrReturnNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_ReturnNode };
    PyrReturnNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrParseNode* expr);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrParseNode* mExpr; // if null, return self
};

struct PyrBlockReturnNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_BlockReturnNode };
    PyrBlockReturnNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrParseNode* expr);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrParseNode* mExpr; // if null, return self
};

struct PyrAssignNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_AssignNode };
    PyrAssignNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* varName, PyrParseNode* expr);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mVarName;
    PyrParseNode* mExpr;
    bool mDrop { false }; // allow drop
};

struct PyrMultiAssignNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_MultiAssignNode };
    PyrMultiAssignNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, struct PyrMultiAssignVarListNode* varlist,
                       PyrParseNode* expr);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    struct PyrMultiAssignVarListNode* mVarList;
    PyrParseNode* mExpr;
    bool mDrop { false }; // allow drop
};

struct PyrMultiAssignVarListNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_MultiAssignVarListNode };
    PyrMultiAssignVarListNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* varNames,
                              PyrSlotNode* rest);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mVarNames;
    PyrSlotNode* mRest;
};

struct PyrBlockNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_BlockNode };
    PyrBlockNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, struct PyrArgListNode* argList,
                 struct PyrVarListNode* varList, struct PyrParseNode* body, bool topLevel);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    struct PyrParseNode* mBody;
    bool mIsTopLevel;
};

struct PyrArgListNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_ArgListNode };
    PyrArgListNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrVarDefNode* vardefs, PyrSlotNode* varArgName,
                   PyrSlotNode* varKwArgName);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrVarDefNode* mVarDefs;
    PyrSlotNode* mVariableArgumentName;
    PyrSlotNode* mVariableKeywordArgumentName;
};

struct PyrLitListNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_LitListNode };
    PyrLitListNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrSlotNode* classname, PyrParseNode* elems);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrSlotNode* mClassname;
    PyrParseNode* mElems;
};

struct PyrLitDictNode : public PyrParseNode {
    static constexpr ParseNodeEnum nodeEnum { pn_LitDictNode };
    PyrLitDictNode(PyrParseNode::TAG, sc::lex::SourceCodeLocation loc, PyrParseNode* elems);

    void compile(PyrSlot* result) override;
    void dump(int level) override;

    PyrParseNode* mElems;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename T> [[nodiscard]] T* tryCast(PyrParseNode* n) noexcept {
    if (!n)
        return nullptr;

    if constexpr (std::is_same_v<T, PyrSlotNode>) {
        if (n->mClassno == pn_SlotNode || n->mClassno == pn_PushLitNode || n->mClassno == pn_PushNameNode
            || n->mClassno == pn_LiteralNode) {
            return reinterpret_cast<T*>(n);
        } else
            return nullptr;
    } else if (n->mClassno == T::nodeEnum) {
        return reinterpret_cast<T*>(n);
    } else
        return nullptr;
}

template <typename T> T* assertCast(PyrParseNode* n) noexcept {
    if (auto ptr = tryCast<T>(n)) {
        return ptr;
    } else {
        assert(false);
        unreachable();
    }
}

extern AdvancingAllocPool gParseNodePool;

template <typename T, typename... ARGS> T* allocNode(sc::lex::SourceCodeLocation loc, ARGS&&... args) {
    static_assert(std::is_base_of<PyrParseNode, T>::value, "Can only allocate PyrParseNodes through this function.");
    static_assert(std::is_trivially_destructible<T>::value,
                  "Right now the allocator does NOT call the destructors, therefore all the PyrParseNodes MUST be "
                  "trival to destruct.");
    return new (gParseNodePool.Alloc(sizeof(T))) T({}, loc, std::forward<ARGS>(args)...);
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// This value count the un-inlined functions, these are not desirable in the class library because they are slow.
// There is a primitive that returns this value so it can be checked in sclang's unit tests.
extern int gNumUninlinedFunctions;

extern PyrParseNode* gRootParseNode;
extern intptr_t gParserResult;
extern bool gIsTailCodeBranch;
extern bool gTailIsMethodReturn;
extern bool compilingCmdLine;

extern int compileErrors;

struct SetTailBranch {
    SetTailBranch(bool inValue) {
        mSave = gIsTailCodeBranch;
        gIsTailCodeBranch = inValue;
    }
    ~SetTailBranch() { gIsTailCodeBranch = mSave; }

private:
    bool mSave;
};

inline void compileNode(PyrParseNode* node, PyrSlot* result, bool onTailBranch) {
    SetTailBranch branch(gIsTailCodeBranch && onTailBranch);
    node->compile(result);
}

void initParseNodes();

int nodeListLength(PyrParseNode* node);
bool isSuperObjNode(PyrParseNode* node);

void compileNodeList(PyrParseNode* node, bool onTailBranch);

void initParser();
void finiParser();
void initParserPool();
void freeParserPool();

void initSpecialSelectors();
void initSpecialClasses();

void nodePostErrorLine(PyrParseNode* node);

PyrParseNode* linkNextNode(PyrParseNode* a, PyrParseNode* b);
PyrParseNode* linkAfterHead(PyrParseNode* a, PyrParseNode* b);

template <typename T> T* linkNextNode(T* a, T* b) {
    static_assert(std::is_base_of_v<PyrParseNode, T>);
    return static_cast<T*>(linkNextNode(static_cast<PyrParseNode*>(a), static_cast<PyrParseNode*>(b)));
}

template <typename... NODES> PyrParseNode* linkAllNodes(NODES... nodes) {
    PyrParseNode* rolling { nullptr };
    ((rolling = linkNextNode(rolling, nodes)), ...);
    return rolling;
}


/// Creates a compiler error if current version is greater than or equal to 'version'.
/// Otherwise posts a warning informing the user to fix their code before updating.
void emitCompilerErrorFromVersion(SemanticVersion version);

extern int numOverwrites;
extern std::string overwriteMsg;

extern PyrSymbol* ps_newlist;
extern PyrSymbol* gSpecialUnarySelectors[opNumUnarySelectors];
extern PyrSymbol* gSpecialBinarySelectors[opNumBinarySelectors];
extern PyrSymbol* gSpecialSelectors[opmNumSpecialSelectors];
extern PyrSymbol* gSpecialClasses[op_NumSpecialClasses];

extern PyrClass* gCurrentClass;
extern PyrClass* gCurrentMetaClass;
extern PyrClass* gCompilingClass;
extern PyrMethod* gCompilingMethod;
extern PyrFunctionDef* gCompilingBlock;
