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

#include <variant>
#include <array>
#include <memory>
#include "PyrSlot.h"
#include "PyrKernel.h"
#include "AdvancingAllocPool.h"
#include "SpecialSelectorsOperatorsAndClasses.h"

#define COMPILENODE(node, result, onTailBranch) (compileNode((node), (result), (onTailBranch)))
#define DUMPNODE(node, level)                                                                                          \
    do {                                                                                                               \
        if (node)                                                                                                      \
            (node)->dump(level);                                                                                       \
    } while (false);


enum { rwPrivate = 0, rwReadOnly = 1, rwWriteOnly = 2, rwReadWrite = 3 };

enum { varInst, varClass, varTemp, varConst, varPseudo, varLocal };

enum {
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

    pn_ArgumentNode,

    pn_NumTypes
};


// This value count the un-inlined functions, these are not desirable in the class library because they are slow.
// There is a primitive that returns this value so it can be checked in sclang's unit tests.
extern int gNumUninlinedFunctions;


struct PyrParseNode {
    // TODO: should we delete the rest of the constructors?
    PyrParseNode(int classno);
    virtual ~PyrParseNode() {}
    virtual void compile(PyrSlot* result) = 0;
    virtual void dump(int level) = 0;

    struct PyrParseNode* mNext;
    struct PyrParseNode* mTail;
    int mLineno;
    int mCharno;
    unsigned char mClassno;
    unsigned char mParens;
};

struct PyrArgumentNode : public PyrParseNode {
    enum struct ArgumentType { Positional, Keyword, VariablePositional, VariableKeyword };
    PyrArgumentNode(ArgumentType argumentType): PyrParseNode(pn_ArgumentNode), mArgumentType(argumentType) {}
    virtual ~PyrArgumentNode() = default;

    ArgumentType mArgumentType;
    PyrParseNode* mNode {};
    void compile(PyrSlot* result) override {}
    void dump(int level) override {}
};

struct PyrSlotNode : public PyrParseNode {
    PyrSlotNode(int classno, PyrSlot slot): PyrParseNode(classno), mSlot(slot) {}
    virtual ~PyrSlotNode() {}

    virtual void compile(PyrSlot* result);
    virtual void compileLiteral(PyrSlot* result);
    virtual void compilePushLit(PyrSlot* result);
    virtual void dump(int level);
    virtual void dumpLiteral(int level);
    virtual void dumpPushLit(int level);

    PyrSlot mSlot;
};

typedef PyrSlotNode PyrLiteralNode;
typedef PyrSlotNode PyrPushLitNode;
typedef PyrSlotNode PyrPushNameNode;

struct PyrCurryArgNode : public PyrParseNode {
    PyrCurryArgNode(): PyrParseNode(pn_CurryArgNode), mArgNum(-1) {}
    virtual ~PyrCurryArgNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    int mArgNum;
};


struct PyrClassExtNode : public PyrParseNode {
    PyrClassExtNode(struct PyrSlotNode* className, struct PyrMethodNode* methods):
        PyrParseNode(pn_ClassExtNode),
        mClassName(className),
        mMethods(methods) {}
    virtual ~PyrClassExtNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mClassName;
    struct PyrMethodNode* mMethods;
};

struct PyrClassNode : public PyrParseNode {
    PyrClassNode(struct PyrSlotNode* className, struct PyrSlotNode* superClassName, struct PyrSlotNode* indexType,
                 struct PyrVarListNode* varlists, struct PyrMethodNode* methods):
        PyrParseNode(pn_ClassNode),
        mClassName(className),
        mSuperClassName(superClassName),
        mIndexType(indexType),
        mVarlists(varlists),
        mMethods(methods) {}
    virtual ~PyrClassNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mClassName;
    struct PyrSlotNode* mSuperClassName;
    struct PyrSlotNode* mIndexType;
    struct PyrVarListNode* mVarlists;
    struct PyrMethodNode* mMethods;
    int mVarTally[4] { 0, 0, 0, 0 };
    int mNumSuperInstVars {}; // This is not initialised at construction.
};

struct PyrMethodNode : public PyrParseNode {
    PyrMethodNode(struct PyrSlotNode* methodName, struct PyrSlotNode* primitiveName, struct PyrArgListNode* arglist,
                  struct PyrVarListNode* varlist, struct PyrParseNode* body, bool isClassMethod):
        PyrParseNode(pn_MethodNode),
        mMethodName(methodName),
        mPrimitiveName(primitiveName),
        mArglist(arglist),
        mVarlist(varlist),
        mBody(body),
        mIsClassMethod(isClassMethod) {}
    virtual ~PyrMethodNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mMethodName;
    struct PyrSlotNode* mPrimitiveName;
    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    struct PyrParseNode* mBody;
    bool mIsClassMethod;
    bool mExtension {}; // Not initialised at construction.
};

struct PyrVarListNode : public PyrParseNode {
    PyrVarListNode(struct PyrVarDefNode* varDefs, int flags):
        PyrParseNode(pn_VarListNode),
        mVarDefs(varDefs),
        mFlags(flags) {}
    virtual ~PyrVarListNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrVarDefNode* mVarDefs;
    int mFlags;
};

struct PyrVarDefNode : public PyrParseNode {
    PyrVarDefNode(

        struct PyrSlotNode* varName, PyrParseNode* defVal, int flags, bool drop):
        PyrParseNode(pn_VarDefNode),
        mVarName(varName),
        mDefVal(defVal),
        mFlags(flags),
        mDrop(drop)

    {}
    virtual ~PyrVarDefNode() {}
    virtual void compile(PyrSlot* result);
    virtual void compileArg(PyrSlot* result);
    virtual void dump(int level);
    bool hasExpr(PyrSlot* result);

    struct PyrSlotNode* mVarName;
    PyrParseNode* mDefVal;
    int mFlags;
    bool mDrop;
};

struct PyrCallNodeBase : public PyrParseNode {
    PyrCallNodeBase(int classno): PyrParseNode(classno) {}
    virtual ~PyrCallNodeBase() {}

    virtual void compile(PyrSlot* result);
    virtual void compilePartialApplication(int numCurryArgs, PyrSlot* result);
    virtual void compileCall(PyrSlot* result) = 0;

    virtual int isPartialApplication() = 0;
};

struct PyrCallNodeBase2 : public PyrCallNodeBase {
    PyrCallNodeBase2(int classno, struct PyrSlotNode* selector, struct PyrParseNode* arglist,
                     struct PyrParseNode* keyarglist):
        PyrCallNodeBase(classno),
        mSelector(selector),
        mArglist(arglist),
        mKeyarglist(keyarglist) {}
    virtual ~PyrCallNodeBase2() {}

    struct PyrSlotNode* mSelector;
    struct PyrParseNode* mArglist;
    struct PyrParseNode* mKeyarglist;
    // bool mTailCall; appears unused?
};

struct PyrCallNode : public PyrCallNodeBase2 {
    PyrCallNode(struct PyrSlotNode* selector, struct PyrParseNode* arglist, struct PyrParseNode* keyarglist):
        PyrCallNodeBase2(pn_CallNode, selector, arglist, keyarglist) {}
    virtual ~PyrCallNode() {}

    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();
};

struct PyrBinopCallNode : public PyrCallNodeBase2 {
    PyrBinopCallNode(struct PyrSlotNode* selector, struct PyrParseNode* arglist):
        PyrCallNodeBase2(pn_BinopCallNode, selector, arglist, nullptr) {}
    virtual ~PyrBinopCallNode() {}

    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();
};

struct PyrSetterNode : public PyrCallNodeBase {
    PyrSetterNode(struct PyrSlotNode* selector, struct PyrParseNode* expr1, struct PyrParseNode* expr2

                  ):
        PyrCallNodeBase(pn_SetterNode),
        mSelector(selector),
        mExpr1(expr1),
        mExpr2(expr2) {}
    virtual ~PyrSetterNode() {}
    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    struct PyrSlotNode* mSelector;
    struct PyrParseNode* mExpr1;
    struct PyrParseNode* mExpr2;
    // int mFlags; // is a var def ?
};

struct PyrDynListNode : public PyrCallNodeBase {
    PyrDynListNode(PyrParseNode* className, PyrParseNode* elems

                   ):
        PyrCallNodeBase(pn_DynListNode),
        mClassname(className),
        mElems(elems) {}
    virtual ~PyrDynListNode() {}
    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    struct PyrParseNode* mClassname;
    struct PyrParseNode* mElems;
};

struct PyrDynDictNode : public PyrCallNodeBase {
    PyrDynDictNode(PyrParseNode* elems): PyrCallNodeBase(pn_DynDictNode), mElems(elems) {}
    virtual ~PyrDynDictNode() {}
    virtual void compileCall(PyrSlot* result);
    virtual void dump(int level);

    virtual int isPartialApplication();

    struct PyrParseNode* mElems;
};


struct PyrDropNode : public PyrParseNode {
    PyrDropNode(PyrParseNode* expr1, PyrParseNode* expr2): PyrParseNode(pn_DropNode), mExpr1(expr1), mExpr2(expr2) {}
    virtual ~PyrDropNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrParseNode* mExpr1;
    struct PyrParseNode* mExpr2;
};

struct PyrPushKeyArgNode : public PyrParseNode {
    PyrPushKeyArgNode(

        struct PyrSlotNode* selector, struct PyrParseNode* expr):
        PyrParseNode(pn_PushKeyArgNode),
        mSelector(selector),
        mExpr(expr) {}
    virtual ~PyrPushKeyArgNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mSelector;
    struct PyrParseNode* mExpr;
};

struct PyrReturnNode : public PyrParseNode {
    PyrReturnNode(PyrParseNode* expr = nullptr): PyrParseNode(pn_ReturnNode), mExpr(expr) {}
    virtual ~PyrReturnNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrParseNode* mExpr; // if null, return self
};

struct PyrBlockReturnNode : public PyrParseNode {
    PyrBlockReturnNode(PyrParseNode* expr = nullptr): PyrParseNode(pn_BlockReturnNode), mExpr(expr) {}
    virtual ~PyrBlockReturnNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrParseNode* mExpr; // if null, return self
};

struct PyrAssignNode : public PyrParseNode {
    PyrAssignNode(struct PyrSlotNode* varName, struct PyrParseNode* expr, bool drop

                  ):
        PyrParseNode(pn_AssignNode),
        mVarName(varName),
        mExpr(expr),
        mDrop(drop) {}
    virtual ~PyrAssignNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mVarName;
    struct PyrParseNode* mExpr;
    bool mDrop; // allow drop
};

struct PyrMultiAssignNode : public PyrParseNode {
    PyrMultiAssignNode(struct PyrMultiAssignVarListNode* varList, struct PyrParseNode* expr, bool drop):
        PyrParseNode(pn_MultiAssignNode),
        mVarList(varList),
        mExpr(expr),
        mDrop(drop) {}
    virtual ~PyrMultiAssignNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrMultiAssignVarListNode* mVarList;
    struct PyrParseNode* mExpr;
    bool mDrop; // allow drop
};

struct PyrMultiAssignVarListNode : public PyrParseNode {
    PyrMultiAssignVarListNode(struct PyrSlotNode* varNames, struct PyrSlotNode* rest):
        PyrParseNode(pn_MultiAssignVarListNode),
        mVarNames(varNames),
        mRest(rest) {}
    virtual ~PyrMultiAssignVarListNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrSlotNode* mVarNames;
    struct PyrSlotNode* mRest;
};

struct PyrBlockNode : public PyrParseNode {
    PyrBlockNode(

        struct PyrArgListNode* arglist, struct PyrVarListNode* varlist, struct PyrParseNode* body, bool isTopLevel,
        int beginCharNo):
        PyrParseNode(pn_BlockNode),
        mArglist(arglist),
        mVarlist(varlist),
        mBody(body),
        mIsTopLevel(isTopLevel),
        mBeginCharNo(beginCharNo) {}
    virtual ~PyrBlockNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrArgListNode* mArglist;
    struct PyrVarListNode* mVarlist;
    struct PyrParseNode* mBody;
    bool mIsTopLevel;
    int mBeginCharNo;
};

struct PyrArgListNode : public PyrParseNode {
    PyrArgListNode(struct PyrVarDefNode* varDefs, struct PyrSlotNode* rest, struct PyrSlotNode* keywordArgs = nullptr):
        PyrParseNode(pn_ArgListNode),
        mVarDefs(varDefs),
        mRest(rest),
        mKeywordArgs(keywordArgs) {}
    virtual ~PyrArgListNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrVarDefNode* mVarDefs;
    struct PyrSlotNode* mRest;
    struct PyrSlotNode* mKeywordArgs = nullptr;
};

struct PyrLitListNode : public PyrParseNode {
    PyrLitListNode(PyrParseNode* classname, PyrParseNode* elem):
        PyrParseNode(pn_LitListNode),
        mClassname(classname),
        mElems(elem) {}
    virtual ~PyrLitListNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrParseNode* mClassname;
    struct PyrParseNode* mElems;
};

struct PyrLitDictNode : public PyrParseNode {
    PyrLitDictNode(PyrParseNode* elems): PyrParseNode(pn_LitDictNode), mElems(elems) {}
    virtual ~PyrLitDictNode() {}
    virtual void compile(PyrSlot* result);
    virtual void dump(int level);

    struct PyrParseNode* mElems;
};

using ParseNodeVariant =
    std::variant<std::monostate, PyrArgumentNode, PyrSlotNode, PyrCurryArgNode, PyrClassExtNode, PyrClassNode,
                 PyrMethodNode, PyrVarListNode, PyrVarDefNode, PyrCallNode, PyrBinopCallNode, PyrSetterNode,
                 PyrDynListNode, PyrDynDictNode, PyrDropNode, PyrPushKeyArgNode, PyrReturnNode, PyrBlockReturnNode,
                 PyrAssignNode, PyrMultiAssignNode, PyrMultiAssignVarListNode, PyrBlockNode, PyrArgListNode,
                 PyrLitListNode, PyrLitDictNode>;

// All parse node must have a stable address.
struct ParseNodeAllocPool {
    static constexpr auto ChunkParseNodeCount = 2048;
    using Chunk = std::array<ParseNodeVariant, ChunkParseNodeCount>;
    std::vector<std::unique_ptr<Chunk>> chunks {};

    size_t node_size { 0 };

    void clear() { chunks.clear(); }

    template <typename T, typename... Args> T* alloc(Args&&... args) {
        if (node_size >= ChunkParseNodeCount || chunks.empty()) {
            chunks.push_back(std::unique_ptr<Chunk>(new Chunk()));
            node_size = 0;
        }

        return &chunks.back()->operator[](node_size++).emplace<T>(std::forward<Args>(args)...);
    }
};

extern std::optional<ParseNodeAllocPool> gParseNodePool;


extern PyrParseNode* gRootParseNode;
extern intptr_t gParserResult;
extern bool gIsTailCodeBranch;
extern bool gTailIsMethodReturn;

extern bool compilingCmdLine;

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

void initParseNodes();

PyrSlotNode* newPyrSlotNode(PyrSlot* slot);
PyrCurryArgNode* newPyrCurryArgNode();
PyrClassNode* newPyrClassNode(PyrSlotNode* className, PyrSlotNode* superClassName, PyrVarListNode* varlists,
                              PyrMethodNode* methods, PyrSlotNode* indexType);
PyrClassExtNode* newPyrClassExtNode(PyrSlotNode* className, PyrMethodNode* methods);
PyrMethodNode* newPyrMethodNode(PyrSlotNode* methodName, PyrSlotNode* primitiveName, PyrArgListNode* arglist,
                                PyrVarListNode* varlist, PyrParseNode* body, int isClassMethod);
PyrArgListNode* newPyrArgListNode(PyrVarDefNode* varDefs, PyrSlotNode* rest, PyrSlotNode* kwArgs);
PyrVarListNode* newPyrVarListNode(PyrVarDefNode* vardefs, int flags);
PyrVarDefNode* newPyrVarDefNode(PyrSlotNode* varName, PyrParseNode* defVal, int flags);
PyrCallNode* newPyrCallNode(PyrSlotNode* selector, PyrParseNode* arglist, PyrParseNode* keyarglist,
                            PyrParseNode* blocklist);
PyrBinopCallNode* newPyrBinopCallNode(PyrSlotNode* selector, PyrParseNode* arg1, PyrParseNode* arg2,
                                      PyrParseNode* arg3);
PyrDropNode* newPyrDropNode(PyrParseNode* expr1, PyrParseNode* expr2);
PyrPushKeyArgNode* newPyrPushKeyArgNode(PyrSlotNode* selector, PyrParseNode* expr);
PyrPushLitNode* newPyrPushLitNode(PyrSlotNode* literalSlot, PyrParseNode* literalObj);
PyrLiteralNode* newPyrLiteralNode(PyrSlotNode* literalSlot, PyrParseNode* literalObj);
PyrReturnNode* newPyrReturnNode(PyrParseNode* expr);
PyrBlockReturnNode* newPyrBlockReturnNode();
PyrAssignNode* newPyrAssignNode(PyrSlotNode* varName, PyrParseNode* expr, int flags);
PyrSetterNode* newPyrSetterNode(PyrSlotNode* varName, PyrParseNode* expr1, PyrParseNode* expr2);
PyrMultiAssignNode* newPyrMultiAssignNode(PyrMultiAssignVarListNode* varList, PyrParseNode* expr, int flags);
PyrPushNameNode* newPyrPushNameNode(PyrSlotNode* slotNode);
PyrDynDictNode* newPyrDynDictNode(PyrParseNode* elems);
PyrDynListNode* newPyrDynListNode(PyrParseNode* classname, PyrParseNode* elems);
PyrLitListNode* newPyrLitListNode(PyrParseNode* classname, PyrParseNode* elems);
PyrLitDictNode* newPyrLitDictNode(PyrParseNode* elems);
PyrMultiAssignVarListNode* newPyrMultiAssignVarListNode(PyrSlotNode* varNames, PyrSlotNode* rest);
PyrBlockNode* newPyrBlockNode(PyrArgListNode* arglist, PyrVarListNode* varlist, PyrParseNode* body, bool isTopLevel);


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

extern int compileErrors;
extern int numOverwrites;
extern std::string overwriteMsg;

extern intptr_t zzval;
extern PyrSymbol* ps_newlist;
extern PyrSymbol* gSpecialUnarySelectors[opNumUnarySelectors];
extern PyrSymbol* gSpecialBinarySelectors[opNumBinarySelectors];
extern PyrSymbol* gSpecialSelectors[opmNumSpecialSelectors];
extern PyrSymbol* gSpecialClasses[op_NumSpecialClasses];

extern PyrClass* gCurrentClass;
extern PyrClass* gCurrentMetaClass;
extern PyrClass* gCompilingClass;
extern PyrMethod* gCompilingMethod;
extern PyrBlock* gCompilingBlock;

#define YYSTYPE intptr_t
