//===---- AutoDiffPostUnroll.cpp - optimize VJP and Pullback functions ----===//
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
///
/// \file
///
/// TODO: documentation
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-autodiff-post-unroll"

#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/Analysis/AutoDiffBlockTracingAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

static llvm::cl::opt<bool>
    AutoDiffEnablePostUnroll("sil-autodiff-enable-post-unroll",
                             llvm::cl::init(true),
                             llvm::cl::desc("Enable AutoDiffPostUnrol pass."));

STATISTIC(NumPullbacks, "Number of unrolled AutoDiff Pullback functions");
STATISTIC(NumOptimized,
          "Number of unrolled AutoDiff Pullback/VJP interfaces changed");
STATISTIC(NumMissed, "Number of unrolled AutoDiff Pullback/VJP missed");

namespace {

static void reportMiss(StringRef Msg, SILValue Val = nullptr) {
  LLVM_DEBUG(llvm::dbgs() << "AutoDiff PU miss: " << Msg
                          << (Val ? ":\n" : "\n"));
  LLVM_DEBUG(if (Val) Val->print(llvm::dbgs()););
  ++NumMissed;
}

class AutoDiffPostUnrollPass : public SILModuleTransform {
  void run() override {
    if (!AutoDiffEnablePostUnroll)
      return;

    SILModule *M = getModule();
    for (SILDifferentiabilityWitness &witness :
         M->getDifferentiabilityWitnesses()) {
      if (witness.isDeclaration())
        continue;
      SILFunction *VJP = witness.getVJP();
      runOnFunction(*VJP);
    }
  }

  void runOnFunction(SILFunction &F) {
    if (!F.isDefinition())
      return;

    // When the original function has a loop, this loop is cloned into the
    // derivative, and the block tracing context is introduced to store VJPs
    // (partial_apply) for every basic block that we execute.
    //
    // Even when these loops are unrolled, the block tracing context object and
    // calls to builtins still remain as a linear sequence.
    //
    // First, check if we have such sequence in the derivative function.
    AutoDiffBlockTracingAnalysis *BTA =
        PM->getAnalysis<AutoDiffBlockTracingAnalysis>();
    AutoDiffFunctionContext *CtxVJP = BTA->get(&F);
    if (!CtxVJP->isValid() || !CtxVJP->isLinear())
      return;

    // Once all intermediate partial_apply functions are stored into the
    // context, it is passed to the pullback function and captured in another
    // partial_apply.
    PartialApplyInst *PullbackPA =
        CtxVJP->Root->getSingleUserOfType<PartialApplyInst>();
    if (!PullbackPA || PullbackPA->getParent() != F.getEntryBlock()) {
      reportMiss("could not find the pullback apply with the context",
                 CtxVJP->Root);
      return;
    }

    // From this closure, find the pullback function and check if it is also a
    // linear sequence.
    FunctionRefInst *PullbackRef =
        dyn_cast_or_null<FunctionRefInst>(PullbackPA->getCallee());
    SILFunction *Pullback =
        PullbackRef ? PullbackRef->getReferencedFunction() : nullptr;
    if (!Pullback || !Pullback->isDefinition()) {
      reportMiss("could not find the pullback function", PullbackPA);
      return;
    }

    AutoDiffFunctionContext *CtxPB = BTA->get(Pullback);
    if (!CtxPB->isValid() || !CtxPB->isLinear()) {
      reportMiss("derivative function is unrolled, but the pullback is not");
      return;
    }

    ++NumPullbacks;

    // Bail out if the derivative function was inlined. It should be possible to
    // optimize this case as well (by analyzing call-sites).
    if (F.isInlined()) {
      reportMiss("derivative function is inlined");
      return;
    }

    // Now we can match these two sequences and pass intermediate VJPs as
    // parameters from the derivative to the pullback without storing them into
    // the block tracing context.
    SmallVector<SILValue, 32> IntermVJP;
    for (const AutoDiffBlockContext &BlockCtx : CtxVJP->Blocks) {
      for (SILValue DF : BlockCtx.Derivatives) {
        IntermVJP.push_back(DF);
      }
    }

    // The pullback should have an apply instruction for each intermediate VJP
    SmallVector<ApplyInst *, 32> IntermApply;
    if (!findPullbackIntermApply(CtxPB, IntermApply)) {
      reportMiss("found no intermediate derivatives");
      return;
    }

    assert(IntermVJP.size() == IntermApply.size() &&
           "mismatch between stored and loaded intermediate VJPs");

    // Now we change the pullback declaration and add intermediate VJPs as
    // parameters instead of the block tracing context.
    //
    // To do that, we first need to erase all usage of the context parameter
    // from the Pullback.
    undefApply(IntermApply);
    eraseAll(CtxPB->Root);

    // Now we can remove the context parameter completely, add a new parameter
    // for each intermediate VJP.
    SmallVector<SILValue, 32> NewPullbackParams;
    rewritePullbackParams(Pullback, IntermVJP, NewPullbackParams);
    rewritePullbackIntermApply(IntermApply, NewPullbackParams);

    // Change the VJP to pass intermediate VJPs as arguments.
    rewritePartialApply(Pullback, PullbackPA, PullbackRef, IntermVJP);

    // The original loop context in the VJP is now effectively dead.
    eraseAll(CtxVJP->Root);

    // FIXME: this may not be needed
    Pullback->setInlineStrategy(AlwaysInline);
    ++NumOptimized;
  }

  bool findPullbackIntermApply(AutoDiffFunctionContext *Ctx,
                               SmallVectorImpl<ApplyInst *> &IntermApply) {
    for (const AutoDiffBlockContext &BlockCtx : Ctx->Blocks) {
      for (SILValue DF : BlockCtx.Derivatives) {
        ApplyInst *Apply = DF->getSingleUserOfType<ApplyInst>();
        if (!Apply)
          return false;
        IntermApply.push_back(Apply);
      }
    }
    return !IntermApply.empty();
  }

  void undefApply(const SmallVectorImpl<ApplyInst *> &IntermApply) {
    for (ApplyInst *Apply : IntermApply) {
      Operand *Callee = Apply->getCalleeOperand();
      Callee->set(SILUndef::get(Callee->get()));
    }
  }

  void eraseAll(SILValue Root) {
    SmallVector<SILValue, 32> WorkList;
    llvm::SmallSet<SILValue, 32> Visited;
    SmallVector<SILInstruction *, 32> EraseList;
    WorkList.push_back(Root);

    while (!WorkList.empty()) {
      SILValue V = WorkList.pop_back_val();
      if (SILInstruction *I = V->getDefiningInstruction()) {
        EraseList.push_back(I);
      }
      for (SILInstruction *UI : V->getUsers()) {
        for (SILValue UR : UI->getResults()) {
          if (Visited.insert(UR).second)
            WorkList.push_back(UR);
        }
        if (!UI->hasResults())
          EraseList.push_back(UI);
      }
    }

    for (auto I = EraseList.rbegin(), E = EraseList.rend(); I != E; ++I) {
      SILInstruction *Inst = *I;
      if (!Inst->isDeleted())
        Inst->eraseFromParent();
    }
  }

  void rewritePullbackParams(SILFunction *Pullback,
                             ArrayRef<SILValue> IntermVJP,
                             SmallVectorImpl<SILValue> &NewPullbackParams) {
    CanSILFunctionType OrigTy = Pullback->getLoweredFunctionType();
    ArrayRef<SILParameterInfo> OrigParams = OrigTy->getParameters();

    SmallVector<SILParameterInfo, 32> NewParams;
    std::copy(OrigParams.begin(), OrigParams.end(),
              std::back_inserter(NewParams));
    NewParams.pop_back_n(1); // drop the loop context (Builtin.NativeObject)

    // Add new parameters for intermediate partial_apply functions.
    for (SILValue VJP : llvm::reverse(IntermVJP)) {
      NewParams.push_back(SILParameterInfo(
          VJP->getType().getASTType(), ParameterConvention::Direct_Guaranteed));
    }

    auto NewTy = SILFunctionType::get(
        OrigTy->getInvocationGenericSignature(), OrigTy->getExtInfo(),
        OrigTy->getCoroutineKind(), OrigTy->getCalleeConvention(), NewParams,
        OrigTy->getYields(), OrigTy->getResults(),
        OrigTy->getOptionalErrorResult(), OrigTy->getPatternSubstitutions(),
        OrigTy->getInvocationSubstitutions(), Pullback->getASTContext());
    LLVM_DEBUG(llvm::dbgs() << "Changing pullback function signature to\n";
               NewTy->print(llvm::dbgs()); llvm::dbgs() << '\n');

    unsigned ReplaceI = OrigParams.size() - 1; // rewrite the loop context
    unsigned NewArgI = OrigParams.size() - 1;
    Pullback->rewriteLoweredTypeUnsafe(NewTy);

    SILBasicBlock *Entry = Pullback->getEntryBlock();
    for (SILValue VJP : llvm::reverse(IntermVJP)) {
      SILType Ty = VJP->getType();
      ValueOwnershipKind Ownership(*Pullback, Ty,
                                   SILArgumentConvention::Direct_Guaranteed);
      SILValue NewParam =
          ReplaceI == NewArgI
              ? Entry->replaceFunctionArgument(NewArgI, Ty, Ownership)
              : Entry->insertFunctionArgument(NewArgI, Ty, Ownership);
      NewPullbackParams.push_back(NewParam);
      ++NewArgI;
    }
  }

  void
  rewritePullbackIntermApply(const SmallVectorImpl<ApplyInst *> &IntermApply,
                             const SmallVectorImpl<SILValue> &ParamFn) {
    assert(IntermApply.size() == ParamFn.size() &&
           "mismatch between original and replacement values");
    for (size_t i = 0; i < IntermApply.size(); ++i) {
      ApplyInst *Apply = IntermApply[i];
      SILValue NewCallee = ParamFn[i];
      Operand *Callee = Apply->getCalleeOperand();
      Callee->set(NewCallee);
    }
  }

  // Pullback function is changed - change the reference type and partial_apply
  // to match the new function type.
  void rewritePartialApply(SILFunction *Pullback, PartialApplyInst *PullbackPA,
                           FunctionRefInst *PullbackRef,
                           ArrayRef<SILValue> IntermVJP) {
    SILBuilder Builder(PullbackPA);
    FunctionRefInst *NewRef =
        Builder.createFunctionRef(PullbackRef->getLoc(), Pullback);

    // Copy the arguments, but drop the block tracing context
    // (Builtin.NativeObject).
    OperandValueArrayRef OldArgs = PullbackPA->getArguments();
    SmallVector<SILValue, 32> NewArgs;
    std::copy(OldArgs.begin(), OldArgs.end(), std::back_inserter(NewArgs));
    NewArgs.pop_back_n(1);

    // Add a new argument for every intermediate VJP.
    std::copy(IntermVJP.rbegin(), IntermVJP.rend(),
              std::back_inserter(NewArgs));

    PartialApplyInst *NewPA = Builder.createPartialApply(
        PullbackPA->getLoc(), NewRef, PullbackPA->getSubstitutionMap(), NewArgs,
        PullbackPA->getFunctionType()->getCalleeConvention(),
        PullbackPA->isOnStack(), PullbackPA->getSpecializationInfo());

    // Replace and erase the original reference of the pullback function,
    // because the declaration is changed now.
    PullbackPA->replaceAllUsesWith(NewPA);
    eraseAll(PullbackRef);
  }
};

} // namespace

SILTransform *swift::createAutoDiffPostUnroll() {
  return new AutoDiffPostUnrollPass();
}
