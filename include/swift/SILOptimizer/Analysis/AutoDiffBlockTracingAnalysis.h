//===--- AutoDiffUnrollAnalysis.h - detect unrolled loops -------*- C++ -*-===//
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
/// FIXME
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_AUTODIFFBLOCKTRACING_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_AUTODIFFBLOCKTRACING_H

#include "swift/Basic/Compiler.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILBasicBlock;
class SILFunction;

struct AutoDiffBlockContext {
  // Builtin.RawPointer to memory allocation of this context.
  //
  // For Derivative functions, this is either a result of
  // AutoDiffProjectTopLevelSubcontext or AutoDiffAllocateSubcontextWithType
  // builtins.
  //
  // For Pullback functions, the pointer is read from the previous context enum
  // (like a linked list). The value is either a result of an
  // unchecked_enum_data, or it is a BB argument from a switch_enum.
  SILValue Pointer;

  /// Enum value that identifies the block. It may be optimized away when only
  /// Derivatives are used.
  SILValue Enum;

  /// Derivative functions (or closures). Zero or one derivative is generated
  /// for each basic block, and there may be additional functions (thunks) in
  /// case of generics.
  SmallVector<SILValue, 4> Derivatives;

  /// Elements of the block context can appear in multiple BBs (e.g. the pointer
  /// can be loaded in one block, and then passed to another block where the
  /// enum is extracted). However, for optimizations it is often enough to know
  /// the "first" BB that uses this context, and it is always the one that loads
  /// the pointer.
  SILBasicBlock *getBasicBlock() const;

  /// Returns the case declaration for the enum, if one can be determined, or
  /// null otherwise.
  EnumElementDecl *getEnumElement() const;

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

struct AutoDiffFunctionContext {
  /// $Builtin.NativeObject that holds the block tracing context for the
  /// function.
  ///
  /// For derivative functions it is allocated via
  /// autoDiffCreateLinearMapContextWithType builtin.
  ///
  /// For Pullback functions, it is passed as a function argument.
  SILValue Root = nullptr;

  /// For Pullback functions, this is the corresponding Derivative function.
  /// FIXME: move this query into another AutoDiff-specific analysis.
  SILFunction *Derivative = nullptr;

  /// A list of context elements that contain derivatives of basic blocks of the
  /// original function. Values from the same block are sorted in order.
  SmallVector<AutoDiffBlockContext, 8> Blocks;

  /// Return true if the function has a block tracing context.
  bool isValid() const { return Root; }

  /// Return the function this context belongs to.
  SILFunction *getFunction() const;

  // Return true if all block tracing values originate from the same basic
  // block. In that case, if there used to be a loop, it is now unrolled.
  bool isLinear() const;

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

class AutoDiffBlockTracingAnalysis
    : public FunctionAnalysisBase<AutoDiffFunctionContext> {

public:
  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::AutoDiffBlockTracing;
  }
  AutoDiffBlockTracingAnalysis()
      : FunctionAnalysisBase<AutoDiffFunctionContext>(
            SILAnalysisKind::AutoDiffBlockTracing) {}

  AutoDiffBlockTracingAnalysis(const AutoDiffBlockTracingAnalysis &) = delete;

  AutoDiffBlockTracingAnalysis &
  operator=(const AutoDiffBlockTracingAnalysis &) = delete;

protected:
  virtual std::unique_ptr<AutoDiffFunctionContext>
  newFunctionAnalysis(SILFunction *F) override {
    auto Res = std::make_unique<AutoDiffFunctionContext>();
    compute(F, *Res);
    return Res;
  }

  void compute(SILFunction *F, AutoDiffFunctionContext &Ctx) const;

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind kind) override {
    return kind & InvalidationKind::BranchesAndInstructions;
  }
};

} // end namespace swift

#endif
