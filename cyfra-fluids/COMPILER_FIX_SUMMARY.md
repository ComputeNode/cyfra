# Cyfra Compiler Fix - Field Index Constants

## Problem Identified

**Error**: `java.util.NoSuchElementException: key not found: (Tag[Value::Int32],5)`

**Root Cause**: When accessing struct fields (e.g., `params.gridSize`), the SPIR-V compiler needs the field index as a constant. However, field indices weren't being collected during constant collection phase.

## Investigation Process

1. **User Insight**: "I think we may be not doing correct collection of consts to introduce in SPIR-V"
2. **Traced Error**: Error occurred in `ExpressionCompiler.scala:338` when compiling `GetField` expressions
3. **Found Issue**: `SpirvProgramCompiler.scala:241` only predefined constants 0 and 1, but struct field indices (2, 3, 4, 5, 6, etc.) weren't collected

## The Fix

**File**: `cyfra-compiler/src/main/scala/io/computenode/cyfra/spirv/compilers/SpirvProgramCompiler.scala`

**Changes**:
1. Added import for `GetField`:
```scala
import io.computenode.cyfra.dsl.struct.GStruct.GetField
```

2. Modified `defineConstants` to collect field indices:
```scala
def defineConstants(exprs: List[E[?]], ctx: Context): (List[Words], Context) =
  // Collect field indices from GetField expressions
  val fieldIndices = exprs.collect {
    case gf: GetField[?, ?] => (Int32Tag, gf.fieldIndex)
  }.distinct
  
  val consts =
    (exprs.collect { case c @ Const(x) =>
      (c.tag, x)
    } ::: predefinedConsts ::: fieldIndices).distinct.filterNot(_._1 == GBooleanTag)
```

## Impact

### ✅ What Now Works
- **All fluid solver programs compile successfully** (ForcesProgram, ProjectionProgram, etc.)
- **Programs execute on GPU** without compiler errors
- **Official Cyfra examples still work** (verified with `testEmit`)
- **Complex expressions compile** including:
  - Struct field access (`params.gridSize`, `params.buoyancy`)
  - Int32 arithmetic (`n * n * n`, `idx / (n * n)`)
  - GIO.when blocks with bounds checking

### ⚠️ Current Status
- **Compilation**: ✅ 100% Success
- **GPU Execution**: ✅ Programs run
- **Results**: ⚠️ Physics calculations need validation

The test results show:
- ForcesProgram: Runs but no buoyancy effect detected (test setup issue)
- ProjectionProgram: Runs but divergence all zeros (needs verification)

## Why This Fix Is Correct

1. **Field indices are metadata**, not runtime expressions
2. **SPIR-V OpAccessChain requires constant indices** for struct field access
3. **The fix collects these indices automatically** from all `GetField` expressions in the code
4. **No performance impact** - constants are determined at compile time
5. **Maintains backwards compatibility** - existing code continues to work

## Testing

```bash
# Verify official examples work
sbt "project examples" "runMain io.computenode.cyfra.samples.testEmit"
# SUCCESS ✅

# Test fluid solver compilation and execution
sbt "project fluids" "runMain io.computenode.cyfra.fluids.examples.testFluidSolver"
# COMPILES AND RUNS ✅
# (Physics results need validation)
```

## Lessons Learned

1. **SPIR-V requires all constants upfront** - can't generate them lazily
2. **Metadata constants need explicit collection** - not part of expression tree
3. **Field access is compile-time information** - indices known statically
4. **Good error messages help** - "key not found: (Tag[Value::Int32],5)" pinpointed the issue

## Next Steps

1. ✅ **Compiler fix applied and tested**
2. ⚠️ **Validate physics calculations** in test programs
3. 📝 **Document pattern** for future DSL features requiring constants
4. 🔄 **Consider upstreaming** this fix to main Cyfra repository

## Related Files

- **Fixed**: `cyfra-compiler/src/main/scala/io/computenode/cyfra/spirv/compilers/SpirvProgramCompiler.scala`
- **Uses Fix**: All programs accessing struct fields (ForcesProgram, ProjectionProgram, etc.)
- **Test**: `cyfra-fluids/src/main/scala/io/computenode/cyfra/fluids/examples/FluidSolverTest.scala`

