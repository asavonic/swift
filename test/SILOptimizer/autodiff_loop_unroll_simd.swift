// RUN: %target-swift-frontend -O -emit-ir %s | %FileCheck %s
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Would fail due to unavailability of swift_autoDiffCreateLinearMapContext.

import _Differentiation
import StdlibUnittest

// There are more instructions in processSIMDLoop (vector fmul, extract), but we
// do not check them here to avoid depending on specifics of SIMD lowering.
//
// Only verify the general structure.
//
// CHECK: define{{.*}} float @"{{.*}}processSIMDLoop{{.*}}"
// CHECK:   %[[F0:.*]] = fadd float %{{.*}}, 0.000000e+00
// CHECK:   %[[F1:.*]] = fadd float %{{.*}}, %[[F0]]
// CHECK:   %[[F2:.*]] = fadd float %{{.*}}, %[[F1]]
// CHECK:   %[[F3:.*]] = fadd float %{{.*}}, %[[F2]]
// CHECK:   %[[F4:.*]] = fadd float %{{.*}}, %[[F3]]
// CHECK:   %[[F5:.*]] = fadd float %{{.*}}, %[[F4]]
// CHECK:   %[[F6:.*]] = fadd float %{{.*}}, %[[F5]]
// CHECK:   %[[F7:.*]] = fadd float %24, %[[F6]]
// CHECK:   ret float %[[F7]]
// CHECK: }
//
@differentiable(reverse)
func processSIMDLoop1(_ input: SIMD8<Float>) -> Float {
    var combination: Float = 0.0

    for index in 0..<withoutDerivative(at: input.scalarCount) {
        combination = combination + (input[index] * 2.0)
    }

    return combination
}


// Gradient should be unrolled and optimized to eliminate the loop context (enum
// tracing). After inlining, this should fold completely.
//
// CHECK: define{{.*}} @test_gradient_simd_loop1()
// CHECK: ret
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: float 2.000000e+00
// CHECK-SAME: }
//
@_silgen_name("test_gradient_simd_loop1")
func test_gradient_simd_loop1() -> SIMD8<Float> {
  return gradient(at: SIMD8<Float>(1, 1, 1, 1, 1, 1, 1, 1),
    of: { processSIMDLoop1($0) })
}
