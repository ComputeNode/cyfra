package io.computenode.cyfra.core.expression

import io.computenode.cyfra.core.Layout
import io.computenode.cyfra.core.expression.types.*
import io.computenode.cyfra.core.expression.types.given
import io.computenode.cyfra.core.memory.{GBuffer, GUniform}
import izumi.reflect.Tag

class LayoutDerivationTest extends munit.FunSuite:

  test("Layout should derive for concrete types"):
    // This test passes - Layout derivation works fine for concrete types
    case class ConcreteLayout(buffer: GBuffer[RuntimeArray[Int32]], uniform: GUniform[Float32])
    
    val layout = Layout[ConcreteLayout]
    assert(layout != null)

  // KNOWN ISSUE: Layout fails to derive for case classes with type parameters
  //
  // When a case class has type parameters, and Layout derivation is attempted
  // from within a generic method, the Scala compiler treats the case class's
  // type parameters as distinct from the method's type parameters, even if they
  // have the same name.
  //
  // Example that fails to compile:
  //   case class TestLayout[T](buffer: GBuffer[RuntimeArray[T]], uniform: GUniform[T])
  //   def useLayout[T: Value]: Layout[TestLayout[T]] = Layout[TestLayout[T]]
  //
  // Compiler error:
  //   Found:    GBuffer[RuntimeArray[T]]
  //   Required: GBuffer[RuntimeArray[T²]]
  //   where: T is a type in class TestLayout
  //          T² is a type in method useLayout
  //
  // This affects:
  //  - GFunctionLayout[G: Value, H: Value, R: Value] in cyfra-foton
  //  - Any layout case class with type parameters used in generic contexts
  //
  // Root cause: The Layout macro uses `summonInline` to defer implicit searches
  // to the call site, but the macro-generated code references the case class's
  // type parameters instead of the applied type's arguments. This requires deeper
  // fixes to how the macro constructs the layoutRef and fromBindings methods.
  //
  // Temporary workaround: Explicitly instantiate layouts with concrete types only,
  // or restructure code to avoid generic layout construction.
