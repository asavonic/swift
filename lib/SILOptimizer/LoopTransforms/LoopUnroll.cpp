//===--- LoopUnroll.cpp - Loop unrolling ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loopunroll"

#include "llvm/ADT/DepthFirstIterator.h"

#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/AutoDiffBlockTracingAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Differentiation/Common.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

using namespace swift;
using namespace swift::PatternMatch;

using llvm::DenseMap;
using llvm::MapVector;

STATISTIC(NumAutoDiffPullbacks,
          "Number of AutoDiff pullback functions considered for unrolling");
STATISTIC(NumAutoDiffPullbacksMissed, "Number of AutoDiff unrollable pullback "
                                      "functions missed by the analysis");
STATISTIC(NumAutoDiffPullbacksMissedCost,
          "Number of AutoDiff unrollable pullback functions rejected by the "
          "heuristic");
STATISTIC(NumAutoDiffPullbacksFoundTripCount,
          "Number of AutoDiff pullback with known trip count");
STATISTIC(NumAutoDiffPullbacksUsedUnrollBonus,
          "Number of AutoDiff pullback functions that used the unroll bonus");

llvm::cl::opt<int> SILAutoDiffUnrollThreshold(
    "sil-unroll-autodiff-threshold", llvm::cl::init(-1),
    llvm::cl::desc("Loop unroll threshold for AutoDiff pullback functions."));

static llvm::cl::opt<bool>
    AutoDiffEnableUnroll("sil-autodiff-enable-unroll", llvm::cl::init(true),
                         llvm::cl::desc("Enable AutoDiff LoopUnroll pass."));

namespace {

/// Clone the basic blocks in a loop.
///
/// Currently invalidates the DomTree.
class LoopCloner : public SILCloner<LoopCloner> {
  SILLoop *Loop;

  friend class SILInstructionVisitor<LoopCloner>;
  friend class SILCloner<LoopCloner>;

public:
  LoopCloner(SILLoop *Loop)
      : SILCloner<LoopCloner>(*Loop->getHeader()->getParent()), Loop(Loop) {}

  /// Clone the basic blocks in the loop.
  void cloneLoop();

  void sinkAddressProjections();

  // Update SSA helper.
  void collectLoopLiveOutValues(
      DenseMap<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues);

protected:
  // SILCloner CRTP override.
  SILValue getMappedValue(SILValue V) {
    if (auto *BB = V->getParentBlock()) {
      if (!BBMap.contains(BB))
        return V;
    }
    return SILCloner<LoopCloner>::getMappedValue(V);
  }
  // SILCloner CRTP override.
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    SILCloner<LoopCloner>::postProcess(Orig, Cloned);
  }
};

} // end anonymous namespace

void LoopCloner::sinkAddressProjections() {
  SinkAddressProjections sinkProj;
  for (auto *bb : Loop->getBlocks()) {
    for (auto &inst : *bb) {
      for (auto res : inst.getResults()) {
        if (!res->getType().isAddress()) {
          continue;
        }
        for (auto use : res->getUses()) {
          auto *user = use->getUser();
          if (Loop->contains(user)) {
            continue;
          }
          bool canSink = sinkProj.analyzeAddressProjections(&inst);
          assert(canSink);
          sinkProj.cloneProjections();
        }
      }
    }
  }
}

void LoopCloner::cloneLoop() {
  SmallVector<SILBasicBlock *, 16> ExitBlocks;
  Loop->getExitBlocks(ExitBlocks);

  sinkAddressProjections();
  // Clone the entire loop.
  cloneReachableBlocks(Loop->getHeader(), ExitBlocks,
                       /*insertAfter*/Loop->getLoopLatch());
}

template <typename I1, typename I2>
static bool compareBlockSeq(I1 Begin, I1 End, I2 &It, I2 ItEnd) {
  for (const AutoDiffBlockContext *BlockCtx : llvm::make_range(Begin, End)) {
    if (It == ItEnd) {
      LLVM_DEBUG(llvm::dbgs() << "AutoDiff BTA analysis miss: LoopUnroll: enum "
                                 "sequence stopped early:\n";
                 BlockCtx->print(llvm::dbgs()););
      return false;
    }

    EnumElementDecl *Enum = BlockCtx->getEnumElement();
    EnumElementDecl *EnumIt = It->getEnumElement();
    if (Enum && EnumIt && Enum != EnumIt) {
      LLVM_DEBUG(llvm::dbgs() << "AutoDiff BTA analysis miss: LoopUnroll: enum "
                                 "sequence does not match:\n"
                              << Enum << EnumIt;
                 BlockCtx->print(llvm::dbgs()); It->print(llvm::dbgs()););
      return false;
    }
    ++It;
  }
  return true;
}

static llvm::Optional<uint64_t>
getMaxLoopTripCountForUnrolledVJP(SILLoop *Loop, SILBasicBlock *Preheader,
                                  SILBasicBlock *Header, SILBasicBlock *Latch,
                                  AutoDiffFunctionContext *CtxPB,
                                  AutoDiffFunctionContext *CtxVJP) {

  // For now, only support loops with a header and a simple one-block body.
  if (Loop->getNumBlocks() != 2)
    return llvm::None;

  // Assume that exit block does not continue the block tracing context.
  SmallVector<const AutoDiffBlockContext *, 8> PreheaderCtx;
  SmallVector<const AutoDiffBlockContext *, 8> LoopCtx;

  for (const AutoDiffBlockContext &BlockCtx : CtxPB->Blocks) {
    SILBasicBlock *BB = BlockCtx.getBasicBlock();
    if (BB == Preheader) {
      PreheaderCtx.push_back(&BlockCtx);
      continue;
    }

    if (Loop->contains(BB)) {
      LoopCtx.push_back(&BlockCtx);
      continue;
    }

    return llvm::None; // FIXME: report miss
  }

  // VJP populates a linked list, and the Pullback reads it in reverse order.
  auto UnrollSeqI = CtxVJP->Blocks.rbegin();
  auto UnrollSeqE = CtxVJP->Blocks.rend();

  // First traverse the preheader. It is outside of the loop, so it doesn't
  // increment the trip count.
  if (!compareBlockSeq(PreheaderCtx.begin(), PreheaderCtx.end(), UnrollSeqI,
                       UnrollSeqE))
    return llvm::None;

  int UnrollFactor = 0;
  while (UnrollSeqI != UnrollSeqE) {
    if (!compareBlockSeq(LoopCtx.begin(), LoopCtx.end(), UnrollSeqI,
                         UnrollSeqE))
      return llvm::None;
    ++UnrollFactor;
  }

  // // The body will be copied MaxTripCount-1 times in tryToUnrollLoop, so add
  // 1
  // // to the actual trip count.
  return UnrollFactor + 1;
}

static llvm::Optional<uint64_t>
getMaxLoopTripCountPullback(SILLoop *Loop, SILBasicBlock *Preheader,
                            SILBasicBlock *Header, SILBasicBlock *Latch,
                            AutoDiffBlockTracingAnalysis *BTA) {

  SILFunction *F = Preheader->getFunction();
  if (F->getEntryBlock() != Preheader)
    return llvm::None;

  AutoDiffFunctionContext *CtxPB = BTA->get(F);
  if (!CtxPB->isValid() || !CtxPB->Derivative)
    return llvm::None;

  AutoDiffFunctionContext *CtxVJP = BTA->get(CtxPB->Derivative);

  if (!CtxVJP->isLinear())
    return llvm::None;

  ++NumAutoDiffPullbacks;

  if (llvm::Optional<uint64_t> TripCount = getMaxLoopTripCountForUnrolledVJP(
          Loop, Preheader, Header, Latch, CtxPB, CtxVJP)) {
    LLVM_DEBUG(llvm::dbgs() << "AutoDiff: pullback loop unroll factor: "
                            << TripCount.value() << "\n");
    ++NumAutoDiffPullbacksFoundTripCount;
    return TripCount;
  }

  LLVM_DEBUG(llvm::dbgs() << "AutoDiff: pullback is not unrolled: "
                             "failed to find loop trip count\n");
  ++NumAutoDiffPullbacksMissed;

  return llvm::None;
}

static llvm::Optional<uint64_t>
getMaxLoopTripCountDifferentiation(SILLoop *Loop, SILBasicBlock *Preheader,
                                   SILBasicBlock *Header, SILBasicBlock *Latch,
                                   AutoDiffBlockTracingAnalysis *BTA) {
  if (!AutoDiffEnableUnroll)
    return llvm::None;
  return getMaxLoopTripCountPullback(Loop, Preheader, Header, Latch, BTA);
}

/// Determine the number of iterations the loop is at most executed. The loop
/// might contain early exits so this is the maximum if no early exits are
/// taken.
static llvm::Optional<uint64_t>
getMaxLoopTripCount(SILLoop *Loop, SILBasicBlock *Preheader,
                    SILBasicBlock *Header, SILBasicBlock *Latch,
                    AutoDiffBlockTracingAnalysis *BTA) {

  // Skip a split backedge.
  SILBasicBlock *OrigLatch = Latch;
  if (!Loop->isLoopExiting(Latch) &&
      !(Latch = Latch->getSinglePredecessorBlock()))
    return llvm::None;
  if (!Loop->isLoopExiting(Latch))
    return llvm::None;

 // Get the loop exit condition.
  auto *CondBr = dyn_cast<CondBranchInst>(Latch->getTerminator());
  if (!CondBr) {
    return getMaxLoopTripCountDifferentiation(Loop, Preheader, Header, Latch,
                                              BTA);
  }

  // Match an add 1 recurrence.

  auto *Cmp = dyn_cast<BuiltinInst>(CondBr->getCondition());
  if (!Cmp)
    return llvm::None;

  unsigned Adjust = 0;
  SILBasicBlock *Exit = CondBr->getTrueBB();

  switch (Cmp->getBuiltinInfo().ID) {
    case BuiltinValueKind::ICMP_EQ:
    case BuiltinValueKind::ICMP_SGE:
      break;
    case BuiltinValueKind::ICMP_SGT:
      Adjust = 1;
      break;
    case BuiltinValueKind::ICMP_SLE:
      Exit = CondBr->getFalseBB();
      Adjust = 1;
      break;
    case BuiltinValueKind::ICMP_NE:
    case BuiltinValueKind::ICMP_SLT:
      Exit = CondBr->getFalseBB();
      break;
    default:
      return llvm::None;
  }

  if (Loop->contains(Exit))
    return llvm::None;

  auto *End = dyn_cast<IntegerLiteralInst>(Cmp->getArguments()[1]);
  if (!End)
    return llvm::None;

  SILValue RecNext = Cmp->getArguments()[0];
  SILPhiArgument *RecArg;

  // Match signed add with overflow, unsigned add with overflow and
  // add without overflow.
  if (!match(RecNext, m_TupleExtractOperation(
                          m_ApplyInst(BuiltinValueKind::SAddOver,
                                      m_SILPhiArgument(RecArg), m_One()),
                          0)) &&
      !match(RecNext, m_TupleExtractOperation(
                          m_ApplyInst(BuiltinValueKind::UAddOver,
                                      m_SILPhiArgument(RecArg), m_One()),
                          0)) &&
      !match(RecNext, m_ApplyInst(BuiltinValueKind::Add,
                                      m_SILPhiArgument(RecArg), m_One()))) {
    return llvm::None;
  }

  if (RecArg->getParent() != Header)
    return llvm::None;

  auto *Start = dyn_cast_or_null<IntegerLiteralInst>(
      RecArg->getIncomingPhiValue(Preheader));
  if (!Start)
    return llvm::None;

  if (RecNext != RecArg->getIncomingPhiValue(OrigLatch))
    return llvm::None;

  auto StartVal = Start->getValue();
  auto EndVal = End->getValue();
  if (StartVal.sgt(EndVal))
    return llvm::None;

  auto Dist = EndVal - StartVal;
  if (Dist.getBitWidth() > 64)
    return llvm::None;

  if (Dist == 0)
    return llvm::None;

  return Dist.getZExtValue() + Adjust;
}

static bool isAutoDiffPullbackLoop(SILLoop *Loop,
                                   AutoDiffBlockTracingAnalysis *BTA) {
  AutoDiffFunctionContext *Ctx = BTA->get(Loop->getFunction());
  return Ctx->isValid() && Ctx->Derivative;
}

/// Check whether we can duplicate the instructions in the loop and use a
/// heuristic that looks at the trip count and the cost of the instructions in
/// the loop to determine whether we should unroll this loop.
static bool canAndShouldUnrollLoop(SILLoop *Loop, uint64_t TripCount,
                                   AutoDiffBlockTracingAnalysis *BTA) {
  assert(Loop->getSubLoops().empty() && "Expect innermost loops");
  if (TripCount > 32)
    return false;

  // We can unroll a loop if we can duplicate the instructions it holds.
  uint64_t Cost = 0;
  // Average number of instructions per basic block.
  // It is used to estimate the cost of the callee
  // inside a loop.
  const uint64_t InsnsPerBB = 4;
  // Use command-line threshold for unrolling.
  const uint64_t SILLoopUnrollThreshold = Loop->getBlocks().empty()
                                              ? 0
                                              : (Loop->getBlocks())[0]
                                                    ->getParent()
                                                    ->getModule()
                                                    .getOptions()
                                                    .UnrollThreshold;

  uint64_t Threshold = SILLoopUnrollThreshold;
  bool FoundAutoDiffLoop = isAutoDiffPullbackLoop(Loop, BTA);
  if (FoundAutoDiffLoop) {
    if (SILAutoDiffUnrollThreshold == -1) {
      Threshold = SILLoopUnrollThreshold * 5;
    } else if (SILAutoDiffUnrollThreshold > 0) {
      Threshold = SILAutoDiffUnrollThreshold;
    }
  }
  bool UsedAutoDiffUnrollBonus = false;

  for (auto *BB : Loop->getBlocks()) {
    for (auto &Inst : *BB) {
      if (!canDuplicateLoopInstruction(Loop, &Inst))
        return false;
      if (instructionInlineCost(Inst) != InlineCost::Free)
        ++Cost;
      if (auto AI = FullApplySite::isa(&Inst)) {
        auto Callee = AI.getCalleeFunction();
        if (Callee && getEligibleFunction(AI, InlineSelection::Everything)) {
          // If callee is rather big and potentially inlinable, it may be better
          // not to unroll, so that the body of the callee can be inlined later.
          Cost += Callee->size() * InsnsPerBB;
        }
      }
      if (Cost * TripCount > Threshold) {
        if (FoundAutoDiffLoop) {
          LLVM_DEBUG(llvm::dbgs()
                     << "AutoDiff: pullback is not unrolled: c=" << Cost
                     << ", tc=" << TripCount << ", tr=" << Threshold << "\n");
          ++NumAutoDiffPullbacksMissedCost;
        }
        return false;
      }
      if (Cost * TripCount > SILLoopUnrollThreshold)
        UsedAutoDiffUnrollBonus = true;
  }
  }

  if (UsedAutoDiffUnrollBonus)
    ++NumAutoDiffPullbacksUsedUnrollBonus;

  return true;
}

/// Redirect the terminator of the current loop iteration's latch to the next
/// iterations header or if this is the last iteration remove the backedge to
/// the header.
static void redirectTerminator(SILBasicBlock *Latch, unsigned CurLoopIter,
                               unsigned LastLoopIter, SILBasicBlock *CurrentHeader,
                               SILBasicBlock *NextIterationsHeader) {

  auto *CurrentTerminator = Latch->getTerminator();

  // We can either have a split backedge as our latch terminator.
  //   HeaderBlock:
  //     ...
  //     cond_br %cond, ExitBlock, BackedgeBlock
  //
  //   BackedgeBlock:
  //     br HeaderBlock:
  //
  // Or a conditional branch back to the header.
  //   HeaderBlock:
  //     ...
  //     cond_br %cond, ExitBlock, HeaderBlock
  //
  // Redirect the HeaderBlock target to the unrolled successor. In the
  // unrolled block of the last iteration unconditionally jump to the
  // ExitBlock instead.

  // Handle the split backedge case.
  if (auto *Br = dyn_cast<BranchInst>(CurrentTerminator)) {
    // On the last iteration change the conditional exit to an unconditional
    // one.
    if (CurLoopIter == LastLoopIter) {
      if (auto *LastSwitch = dyn_cast<SwitchEnumInst>(
              Latch->getSinglePredecessorBlock()->getTerminator())) {
        // switch_enum is only supported for autodiff loop unrolling. We check
        // that it has two values in the trip count calculation routine.
        assert(LastSwitch->getNumCases() == 2 && !LastSwitch->hasDefault() &&
               "unsupported switch_enum");
        std::pair<EnumElementDecl *, SILBasicBlock *> NextCase =
            LastSwitch->getCase(0);
        if (NextCase.second == Latch)
          NextCase = LastSwitch->getCase(1);
        SILValue Enum = LastSwitch->getOperand();
        SILValue Arg = SILBuilderWithScope(LastSwitch)
                           .createUncheckedEnumData(LastSwitch->getLoc(), Enum,
                                                    NextCase.first);
        SILBuilderWithScope(LastSwitch)
            .createBranch(LastSwitch->getLoc(), NextCase.second, {Arg});
        LastSwitch->eraseFromParent();
        return;
      }

      auto *CondBr = cast<CondBranchInst>(
          Latch->getSinglePredecessorBlock()->getTerminator());
      if (CondBr->getTrueBB() != Latch)
        SILBuilderWithScope(CondBr).createBranch(
            CondBr->getLoc(), CondBr->getTrueBB(), CondBr->getTrueArgs());
      else
        SILBuilderWithScope(CondBr).createBranch(
            CondBr->getLoc(), CondBr->getFalseBB(), CondBr->getFalseArgs());
      CondBr->eraseFromParent();
      return;
    }

    // Otherwise, branch to the next iteration's header.
    SILBuilderWithScope(Br).createBranch(Br->getLoc(), NextIterationsHeader,
                                         Br->getArgs());
    Br->eraseFromParent();
    return;
  }

  // Otherwise, we have a conditional branch to the header.
  auto *CondBr = cast<CondBranchInst>(CurrentTerminator);
  // On the last iteration change the conditional exit to an unconditional
  // one.
  if (CurLoopIter == LastLoopIter) {
    if (CondBr->getTrueBB() == CurrentHeader) {
      SILBuilderWithScope(CondBr).createBranch(
          CondBr->getLoc(), CondBr->getFalseBB(), CondBr->getFalseArgs());
    } else {
      assert(CondBr->getFalseBB() == CurrentHeader);
      SILBuilderWithScope(CondBr).createBranch(
          CondBr->getLoc(), CondBr->getTrueBB(), CondBr->getTrueArgs());
    }
    CondBr->eraseFromParent();
    return;
  }

  // Otherwise, branch to the next iteration's header.
  if (CondBr->getTrueBB() == CurrentHeader) {
    SILBuilderWithScope(CondBr).createCondBranch(
        CondBr->getLoc(), CondBr->getCondition(), NextIterationsHeader,
        CondBr->getTrueArgs(), CondBr->getFalseBB(), CondBr->getFalseArgs());
  } else {
    assert(CondBr->getFalseBB() == CurrentHeader);
    SILBuilderWithScope(CondBr).createCondBranch(
        CondBr->getLoc(), CondBr->getCondition(), CondBr->getTrueBB(),
        CondBr->getTrueArgs(), NextIterationsHeader, CondBr->getFalseArgs());
  }
  CondBr->eraseFromParent();
}

/// Collect all the loop live out values in the map that maps original live out
/// value to live out value in the cloned loop.
void LoopCloner::collectLoopLiveOutValues(
    DenseMap<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues) {
  for (auto *Block : Loop->getBlocks()) {
    // Look at block arguments.
    for (auto *Arg : Block->getArguments()) {
      for (auto *Op : Arg->getUses()) {
        // Is this use outside the loop?
        if (!Loop->contains(Op->getUser())) {
          auto ArgumentValue = SILValue(Arg);
          if (!LoopLiveOutValues.count(ArgumentValue))
            LoopLiveOutValues[ArgumentValue].push_back(
                getMappedValue(ArgumentValue));
        }
      }
    }
    // And the instructions.
    for (auto &Inst : *Block) {
      for (SILValue result : Inst.getResults()) {
        for (auto *Op : result->getUses()) {
          // Ignore uses inside the loop.
          if (Loop->contains(Op->getUser()))
            continue;

          auto UsedValue = Op->get();
          assert(UsedValue == result && "Instructions must match");

          if (!LoopLiveOutValues.count(UsedValue))
            LoopLiveOutValues[UsedValue].push_back(getMappedValue(result));
        }
      }
    }
  }
}

static void
updateSSA(SILModule &M, SILLoop *Loop,
          DenseMap<SILValue, SmallVector<SILValue, 8>> &LoopLiveOutValues) {
  SILSSAUpdater SSAUp;
  for (auto &MapEntry : LoopLiveOutValues) {
    // Collect out of loop uses of this value.
    auto OrigValue = MapEntry.first;
    SmallVector<UseWrapper, 16> UseList;
    for (auto Use : OrigValue->getUses())
      if (!Loop->contains(Use->getUser()->getParent()))
        UseList.push_back(UseWrapper(Use));
    // Update SSA of use with the available values.
    SSAUp.initialize(OrigValue->getType(), OrigValue->getOwnershipKind());
    SSAUp.addAvailableValue(OrigValue->getParentBlock(), OrigValue);
    for (auto NewValue : MapEntry.second)
      SSAUp.addAvailableValue(NewValue->getParentBlock(), NewValue);
    for (auto U : UseList) {
      Operand *Use = U;
      SSAUp.rewriteUse(*Use);
    }
  }
}

/// Try to fully unroll the loop if we can determine the trip count and the trip
/// count is below a threshold.
static bool tryToUnrollLoop(SILLoop *Loop, AutoDiffBlockTracingAnalysis *BTA) {
  assert(Loop->getSubLoops().empty() && "Expecting innermost loops");

  LLVM_DEBUG(llvm::dbgs() << "Trying to unroll loop : \n" << *Loop);
  auto *Preheader = Loop->getLoopPreheader();
  if (!Preheader)
    return false;
  SILModule &M = Preheader->getParent()->getModule();

  auto *Latch = Loop->getLoopLatch();
  if (!Latch)
    return false;

  auto *Header = Loop->getHeader();

  llvm::Optional<uint64_t> MaxTripCount =
      getMaxLoopTripCount(Loop, Preheader, Header, Latch, BTA);
  if (!MaxTripCount) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, did not find trip count\n");
    return false;
  }

  if (!canAndShouldUnrollLoop(Loop, MaxTripCount.value(), BTA)) {
    LLVM_DEBUG(llvm::dbgs() << "Not unrolling, exceeds cost threshold\n");
    return false;
  }

  // TODO: We need to split edges from non-condbr exits for the SSA updater. For
  // now just don't handle loops containing such exits.
  SmallVector<SILBasicBlock *, 16> ExitingBlocks;
  Loop->getExitingBlocks(ExitingBlocks);
  for (auto &Exit : ExitingBlocks)
    if (!isa<CondBranchInst>(Exit->getTerminator()) &&
        !isa<SwitchEnumInst>(Exit->getTerminator()))
      return false;

  LLVM_DEBUG(llvm::dbgs() << "Unrolling loop in "
                          << Header->getParent()->getName()
                          << " " << *Loop << "\n");

  SmallVector<SILBasicBlock *, 16> Headers;
  Headers.push_back(Header);

  SmallVector<SILBasicBlock *, 16> Latches;
  Latches.push_back(Latch);

  DenseMap<SILValue, SmallVector<SILValue, 8>> LoopLiveOutValues;

  // Copy the body MaxTripCount-1 times.
  for (uint64_t Cnt = 1; Cnt < *MaxTripCount; ++Cnt) {
    // Clone the blocks in the loop.
    LoopCloner cloner(Loop);
    cloner.cloneLoop();
    Headers.push_back(cloner.getOpBasicBlock(Header));
    Latches.push_back(cloner.getOpBasicBlock(Latch));

    // Collect values defined in the loop but used outside. On the first
    // iteration we populate the map from original loop to cloned loop. On
    // subsequent iterations we only need to update this map with the values
    // from the new iteration's clone.
    if (Cnt == 1)
      cloner.collectLoopLiveOutValues(LoopLiveOutValues);
    else {
      for (auto &MapEntry : LoopLiveOutValues) {
        // Look it up in the value map.
        SILValue MappedValue = cloner.getOpValue(MapEntry.first);
        MapEntry.second.push_back(MappedValue);
        assert(MapEntry.second.size() == Cnt);
      }
    }
  }

  // Thread the loop clones by redirecting the loop latches to the successor
  // iteration's header.
  for (unsigned Iteration = 0, End = Latches.size(); Iteration != End;
       ++Iteration) {
    auto *CurrentLatch = Latches[Iteration];
    auto LastIteration = End - 1;
    auto *CurrentHeader = Headers[Iteration];
    auto *NextIterationsHeader =
        Iteration == LastIteration ? nullptr : Headers[Iteration + 1];

    redirectTerminator(CurrentLatch, Iteration, LastIteration, CurrentHeader,
                       NextIterationsHeader);
  }

  // Fixup SSA form for loop values used outside the loop.
  updateSSA(M, Loop, LoopLiveOutValues);
  return true;
}

// =============================================================================
//                                 Driver
// =============================================================================

namespace {

class LoopUnrolling : public SILFunctionTransform {

  void run() override {
    bool Changed = false;
    auto *Fun = getFunction();
    SILLoopInfo *LoopInfo = PM->getAnalysis<SILLoopAnalysis>()->get(Fun);

    LLVM_DEBUG(llvm::dbgs() << "Loop Unroll running on function : "
                            << Fun->getName() << "\n");

    // Collect innermost loops.
    SmallVector<SILLoop *, 16> InnermostLoops;

    for (auto *Loop : *LoopInfo) {
      SmallVector<SILLoop *, 8> Worklist;
      Worklist.push_back(Loop);

      for (unsigned i = 0; i < Worklist.size(); ++i) {
        auto *L = Worklist[i];
        for (auto *SubLoop : *L)
          Worklist.push_back(SubLoop);
        if (L->getSubLoops().empty())
          InnermostLoops.push_back(L);
      }
    }

    if (InnermostLoops.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "No innermost loops\n");
      return;
    }

    AutoDiffBlockTracingAnalysis *BTA =
        PM->getAnalysis<AutoDiffBlockTracingAnalysis>();
    // Try to unroll innermost loops.
    for (auto *Loop : InnermostLoops)
      Changed |= tryToUnrollLoop(Loop, BTA);

    if (Changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopUnroll() {
  return new LoopUnrolling();
}
