// RUN: %target-swift-frontend -O -emit-ir %s | %FileCheck %s

import _Differentiation

// CHECK: define{{.*}} float @test_float_loop(float %[[ARG:.*]])
// CHECK: %[[MUL:.*]] = fmul float %[[ARG]], %[[ARG]]
// CHECK: ret float %[[MUL]]
//
@_silgen_name("test_float_loop")
func float_loop(_ x: Float) -> Float {
  var result: Float = 1.0
  for _ in 0 ..< 2 {
    result = result * x
  }
  return result
}

// CHECK-LABEL: define{{.*}} float @test_gradient_float_loop()
// CHECK: ret float 2.000000e+01
//
@_silgen_name("test_gradient_float_loop")
func test_gradient_float_loop() -> Float {
  return gradient(at: 10, of: { float_loop($0) })
}
