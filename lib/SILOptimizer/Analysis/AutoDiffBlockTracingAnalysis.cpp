//===--- AutoDiffBlockTracingAnalysis.cpp - TODO -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// TODO
//

#include "swift/SILOptimizer/Analysis/AutoDiffBlockTracingAnalysis.h"
#include "swift/Basic/GraphNodeWorklist.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "sil-autodiff-bta"

llvm::cl::opt<bool> DebugVerbose(
    "sil-autodiff-debug-verbose",
    llvm::cl::desc("Enable verbose logging of AutoDiffBlockTracingAnalysis."),
    llvm::cl::init(false));

using namespace swift;

static void reportMiss(StringRef Msg, SILValue Val = nullptr) {
  LLVM_DEBUG(llvm::dbgs() << "AutoDiff BTA analysis miss: " << Msg
                          << (Val ? ":\n" : "\n"));
  LLVM_DEBUG(if (Val) Val->print(llvm::dbgs()););
}

static void resetBlockContext(AutoDiffBlockContext &Block) {
  Block.Pointer = nullptr;
  Block.Enum = nullptr;
  Block.Derivatives.resize(0);
}

static void resetContext(AutoDiffFunctionContext &F) {
  F.Root = nullptr;
  F.Blocks.resize(0);
}

static bool isPullbackForVJP(SILFunction *PB, SILFunction *VJP) {
  for (SILBasicBlock &BB : llvm::reverse(*VJP)) {
    TermInst *Term = BB.getTerminator();
    ReturnInst *Ret = dyn_cast<ReturnInst>(Term);
    if (!Ret)
      continue;

    TupleInst *RetTuple = dyn_cast<TupleInst>(Ret->getOperand());
    if (!RetTuple) {
      reportMiss("is pullback: not a tuple", Ret->getOperand());
      continue;
    }

    OperandValueArrayRef RetTupleElem = RetTuple->getElements();
    if (RetTupleElem.empty()) {
      reportMiss("is pullback: empty tuple", RetTuple);
      continue;
    }

    SILValue PullbackRet = RetTupleElem.back();
    PartialApplyInst *PartialApplyPB = dyn_cast<PartialApplyInst>(PullbackRet);
    if (!PartialApplyPB) {
      // No miss here: trivial pullbacks and returned values other apply
      // instructions are acceptable.
      continue;
    }

    FunctionRefInst *RetFn =
        dyn_cast<FunctionRefInst>(PartialApplyPB->getCallee());
    if (!RetFn) {
      reportMiss("is pullback: not a direct function", PartialApplyPB);
      continue;
    }

    if (RetFn->getReferencedFunction() == PB)
      return true;
  }
  return false;
}

static BuiltinInst *findRootForVJP(SILFunction *F) {
  SILBasicBlock *Entry = F->getEntryBlock();
  for (SILInstruction &I : *Entry) {
    if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
      llvm::Optional<BuiltinValueKind> Kind = BI->getBuiltinKind();
      if (!Kind ||
          Kind != BuiltinValueKind::AutoDiffCreateLinearMapContextWithType)
        continue;
      return BI;
    }
  }
  return nullptr;
}

static bool findBlockContextStore(SILValue Pointer,
                                  AutoDiffBlockContext &BlockCtx) {
  if (Pointer->use_empty())
    return false;

  BlockCtx.Pointer = Pointer;

  PointerToAddressInst *Addr =
      Pointer->getSingleUserOfType<PointerToAddressInst>();
  if (!Addr) {
    reportMiss("find block context store: no address for pointer", Pointer);
    return false;
  }

  StoreInst *Store = Addr->getSingleUserOfType<StoreInst>();
  if (!Store) {
    reportMiss("find block context store: no store to address", Addr);
    return false;
  }

  TupleInst *Tuple = dyn_cast<TupleInst>(Store->getSrc());
  if (!Tuple) {
    reportMiss("find block context store: stored value is not a tuple",
               Store->getSrc());
    return false;
  }

  OperandValueArrayRef TupleOps = Tuple->getElements();
  assert(TupleOps.size() >= 1 && "unexpected tuple type");

  BlockCtx.Enum = TupleOps[0];

  for (unsigned i = TupleOps.size() - 1; i > 0; --i) {
    BlockCtx.Derivatives.push_back(TupleOps[i]);
  }

  return true;
}

static void computeForVJP(SILFunction *F, AutoDiffFunctionContext &Ctx) {
  BuiltinInst *Root = findRootForVJP(F);
  if (!Root)
    return;

  Ctx.Root = Root;

  // Context elements are allocated (or projected) by
  // AutoDiffAllocateSubcontextWithType (or AutoDiffProjectTopLevelSubcontext).
  // Traverse all instructions to find them.
  for (SILBasicBlock &BB : *F) {
    for (SILInstruction &I : BB) {
      BuiltinInst *BI = dyn_cast<BuiltinInst>(&I);
      if (!BI)
        continue;

      llvm::Optional<BuiltinValueKind> Kind = BI->getBuiltinKind();
      if (!Kind)
        continue;

      SILValue Pointer;
      switch (Kind.value()) {
      case BuiltinValueKind::AutoDiffProjectTopLevelSubcontext:
      case BuiltinValueKind::AutoDiffAllocateSubcontextWithType:
        Pointer = BI;
        break;
      default:
        continue;
      }
      assert(Pointer && "all context builtins should return a pointer");

      AutoDiffBlockContext BlockCtx;
      if (!findBlockContextStore(Pointer, BlockCtx)) {
        reportMiss("failed to a store block for the pointer", Pointer);
        resetContext(Ctx);
        return;
      }
      Ctx.Blocks.push_back(BlockCtx);
    }
  }
}

static BuiltinInst *findRootProjectionForPullback(SILFunction *F) {
  SILBasicBlock *Entry = F->getEntryBlock();
  for (SILInstruction &I : *Entry) {
    if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
      llvm::Optional<BuiltinValueKind> Kind = BI->getBuiltinKind();
      if (!Kind || Kind != BuiltinValueKind::AutoDiffProjectTopLevelSubcontext)
        continue;
      return BI;
    }
  }
  return nullptr;
}

static void findTupleElements(SILValue Tuple, AutoDiffBlockContext &BlockCtx) {
  unsigned TupleSize = Tuple->getType().getNumTupleElements();
  assert(TupleSize >= 1 && "unexpected autodiff block tracing tuple");
  assert(BlockCtx.Derivatives.size() == (TupleSize - 1) &&
         "block context is not initialized");

  for (TupleExtractInst *Elt : Tuple->getUsersOfType<TupleExtractInst>()) {
    unsigned Idx = Elt->getFieldIndex();
    if (Idx == 0) {
      BlockCtx.Enum = Elt->getResult(0);
      continue;
    }
    BlockCtx.Derivatives[Idx - 1] = Elt->getResult(0);
  }
}

static bool findBlockContextLoad(SILValue Pointer,
                                 AutoDiffBlockContext &BlockCtx) {
  if (Pointer->use_empty())
    return false;

  BlockCtx.Pointer = Pointer;

  PointerToAddressInst *TupleAddr =
      Pointer->getSingleUserOfType<PointerToAddressInst>();
  if (!TupleAddr)
    return false; // no miss here: the pointer may be passed to another BB.

  unsigned TupleSize = TupleAddr->getType().getNumTupleElements();
  assert(TupleSize >= 1 && "unexpected autodiff block tracing tuple");

  // First element is always the enum, followed by zero or more derivative
  // functions and thunks.
  BlockCtx.Derivatives.resize(TupleSize - 1);

  // From address, we can either have a `load' followed by `tuple_extract',
  // or `tuple_element_addr' followed by a `load'.
  if (LoadInst *Tuple = TupleAddr->getSingleUserOfType<LoadInst>()) {
    findTupleElements(Tuple, BlockCtx);

    // The tuple can be passed to another block (for example, to a loop header),
    // so we have to follow it there.
    if (BranchInst *Br = Tuple->getSingleUserOfType<BranchInst>()) {
      OperandValueArrayRef Args = Br->getArgs();
      unsigned TupleArgIdx = Args.size();
      for (unsigned i = 0; i < Args.size(); ++i) {
        if (Args[i] == Tuple) {
          TupleArgIdx = i;
          break;
        }
      }
      SILBasicBlock *DestBB = Br->getDestBB();
      ArrayRef<SILArgument *> DestArgs = DestBB->getArguments();
      assert(TupleArgIdx < Args.size() && TupleArgIdx < DestArgs.size() &&
             "pointer argument index is out of range");

      findTupleElements(DestArgs[TupleArgIdx], BlockCtx);
    }
  } else {
    for (TupleElementAddrInst *Elt :
         TupleAddr->getUsersOfType<TupleElementAddrInst>()) {
      LoadInst *EltLoad = Elt->getSingleUserOfType<LoadInst>();
      if (!EltLoad)
        continue;
      unsigned Idx = Elt->getFieldIndex();
      if (Idx == 0) {
        BlockCtx.Enum = EltLoad->getResult(0);
        continue;
      }
      BlockCtx.Derivatives[Idx - 1] = EltLoad->getResult(0);
    }
  }

  // Enum can be optimized away, but derivatives should all be used to compute
  // the result.
  bool FoundAllDerivatives = !llvm::is_contained(BlockCtx.Derivatives, nullptr);
  if (!FoundAllDerivatives) {
    reportMiss("find block context load: missing derivatives", Pointer);
    LLVM_DEBUG(BlockCtx.print(llvm::dbgs()));
    return false;
  }

  return true;
}

static void computeForPullback(SILFunction *F, AutoDiffFunctionContext &Ctx) {
  BuiltinInst *RootProj = findRootProjectionForPullback(F);
  if (!RootProj)
    return;

  assert(RootProj->getNumOperands() == 1 &&
         isa<SILArgument>(RootProj->getOperand(0)) &&
         "malformed builtin autoDiffProjectTopLevelSubcontext");

  Ctx.Root = RootProj->getOperand(0);

  typedef std::pair<SILBasicBlock *, SILValue> BBPointerPair;
  SmallVector<BBPointerPair, 8> WorkList;
  llvm::SmallSet<SILBasicBlock *, 8> Visited;

  SILBasicBlock *RootBB = RootProj->getParent();
  WorkList.push_back({RootBB, RootProj});
  Visited.insert(RootBB);

  while (!WorkList.empty()) {
    SILBasicBlock *BB = nullptr;
    SILValue Pointer;
    std::tie(BB, Pointer) = WorkList.pop_back_val();

    AutoDiffBlockContext BlockCtx;
    while (findBlockContextLoad(Pointer, BlockCtx)) {
      // We found one enum, but this basic block may have others if BBs have
      // been merged, so look for unchecked_enum_data as well. The loop should
      // end at a branch or switch_enum instruction.
      Ctx.Blocks.push_back(BlockCtx);

      if (!BlockCtx.Enum)
        break;

      UncheckedEnumDataInst *Unchecked =
          BlockCtx.Enum->getSingleUserOfType<UncheckedEnumDataInst>();
      if (!Unchecked)
        break;

      Pointer = Unchecked;
      resetBlockContext(BlockCtx);
    }

    if (BranchInst *Br = Pointer->getSingleUserOfType<BranchInst>()) {
      OperandValueArrayRef Args = Br->getArgs();
      unsigned PointerArgIdx = Args.size();
      for (unsigned i = 0; i < Args.size(); ++i) {
        if (Args[i] == Pointer) {
          PointerArgIdx = i;
          break;
        }
      }
      SILBasicBlock *DestBB = Br->getDestBB();
      ArrayRef<SILArgument *> DestArgs = DestBB->getArguments();
      assert(PointerArgIdx < Args.size() && PointerArgIdx < DestArgs.size() &&
             "pointer argument index is out of range");

      if (Visited.insert(DestBB).second)
        WorkList.push_back({DestBB, DestArgs[PointerArgIdx]});
      continue;
    }

    // No enum, therefore no successors of this block access the block tracing
    // context.
    if (!BlockCtx.Enum)
      continue;

    SwitchEnumInst *Switch =
        BlockCtx.Enum->getSingleUserOfType<SwitchEnumInst>();
    if (!Switch || Switch->hasDefault()) {
      reportMiss("cannot find a switch for the enum value", BlockCtx.Enum);
      resetContext(Ctx);
      return;
    }

    for (SILSuccessor &Succ : Switch->getSuccessors()) {
      SILBasicBlock *DestBB = Succ.getBB();
      ArrayRef<SILArgument *> DestArgs = DestBB->getArguments();
      assert(DestArgs.size() == 1 &&
             "switch_enum dest must have one argument (enum value)");

      if (Visited.insert(DestBB).second)
        WorkList.push_back({DestBB, DestArgs[0]});
    }
  }
}

void AutoDiffBlockTracingAnalysis::compute(SILFunction *F,
                                           AutoDiffFunctionContext &Ctx) const {
  for (SILDifferentiabilityWitness &witness :
       F->getModule().getDifferentiabilityWitnesses()) {
    if (witness.isDeclaration())
      continue;
    SILFunction *VJP = witness.getVJP();

    // Thunks have no loops to unroll
    if (VJP->isThunk())
      continue;

    LLVM_DEBUG(if (DebugVerbose) VJP->print(llvm::dbgs()););

    if (F == VJP) {
      computeForVJP(F, Ctx);
      if (Ctx.isValid())
        LLVM_DEBUG(Ctx.print(llvm::dbgs()));
      return;
    }

    if (isPullbackForVJP(F, VJP)) {
      LLVM_DEBUG(if (DebugVerbose) F->print(llvm::dbgs()););

      computeForPullback(F, Ctx);
      Ctx.Derivative = VJP;

      if (Ctx.isValid())
        LLVM_DEBUG(Ctx.print(llvm::dbgs()));
      return;
    }
  }
}

void AutoDiffFunctionContext::print(llvm::raw_ostream &os) const {
  if (!Root) {
    os << "<null>\n";
    return;
  }

  if (SILArgument *RootArg = dyn_cast<SILArgument>(Root)) {
    os << "AutoDiffFunctionContext of pullback: "
       << RootArg->getFunction()->getName() << "\n";
  } else {
    BuiltinInst *RootAlloc = cast<BuiltinInst>(Root);
    os << "AutoDiffFunctionContext of derivative: "
       << RootAlloc->getFunction()->getName() << "\n";
  }

  for (const AutoDiffBlockContext &Block : Blocks) {
    Block.print(os);
  }
}

SILFunction *AutoDiffFunctionContext::getFunction() const {
  assert(isValid() && "invalid function context");
  if (SILInstruction *I = Root->getDefiningInstruction())
    return I->getFunction();
  return cast<SILArgument>(Root)->getFunction();
}

bool AutoDiffFunctionContext::isLinear() const {
  assert(isValid() && "invalid function context");
  SILBasicBlock *EntryBB = getFunction()->getEntryBlock();
  if (SILInstruction *I = Root->getDefiningInstruction()) {
    if (I->getParent() != EntryBB)
      return false;
  } else {
    if (cast<SILArgument>(Root)->getParent() != EntryBB)
      return false;
  }

  for (const AutoDiffBlockContext &BlockCtx : Blocks) {
    if (BlockCtx.getBasicBlock() != EntryBB)
      return false;
  }

  return true;
}

void AutoDiffBlockContext::print(llvm::raw_ostream &os) const {
  if (!Pointer) {
    os << "- <null>\n";
    return;
  }
  os << "- ptr:  ";
  Pointer->print(os);

  os << "  enum: ";
  if (Enum)
    Enum->print(os);
  else
    os << "<optimized out>\n";

  for (unsigned i = 0; i < Derivatives.size(); ++i) {
    os << "  df" << i << ":  ";
    Derivatives[i]->print(os);
  }
}

SILBasicBlock *AutoDiffBlockContext::getBasicBlock() const {
  if (!Pointer)
    return nullptr;

  if (SILInstruction *I = Pointer->getDefiningInstruction())
    return I->getParent();

  return cast<SILArgument>(Pointer)->getParent();
}

EnumElementDecl *AutoDiffBlockContext::getEnumElement() const {
  if (!Enum)
    return nullptr;

  if (EnumInst *EI = dyn_cast<EnumInst>(Enum))
    return EI->getElement();

  if (UncheckedEnumDataInst *UE =
          Enum->getSingleUserOfType<UncheckedEnumDataInst>()) {

    // Ensure that the unchecked_enum_data is always executed for this value
    SILBasicBlock *BB = nullptr;
    if (SILInstruction *I = Enum->getDefiningInstruction())
      BB = I->getParent();
    else
      BB = cast<SILArgument>(Enum)->getParent();
    if (BB != UE->getParent())
      return nullptr;

    return UE->getElement();
  }

  return nullptr;
}
