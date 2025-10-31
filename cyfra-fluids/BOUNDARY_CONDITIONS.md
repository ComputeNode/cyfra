# Boundary Conditions Guide

This document explains the different boundary condition implementations available in the fluid solver.

## Available Boundary Programs

### 1. **BoundaryProgram** (Original - No-Slip)
**File**: `BoundaryProgram.scala`

**Behavior**:
- Sets velocity to zero at all boundaries and obstacles
- Fluid "sticks" to surfaces (no-slip condition)
- Most physically accurate for very viscous fluids

**Use when**:
- Simulating honey, syrup, or other viscous fluids
- You want fluid to stick to surfaces

**Visual effect**: Fluid accumulates at walls and obstacles

---

### 2. **FreeSlipBoundaryProgram** (Frictionless Walls)
**File**: `FreeSlipBoundaryProgram.scala`

**Behavior**:
- Removes only the normal component of velocity
- Preserves tangential component (fluid slides along surfaces)
- No penetration but free sliding

**Use when**:
- Simulating low-viscosity fluids (water, gas)
- You want fluid to flow around obstacles
- Preventing accumulation at boundaries

**Visual effect**: Fluid flows smoothly along surfaces without sticking

---

### 3. **DissipativeBoundaryProgram** (Fading at Boundaries)
**File**: `DissipativeBoundaryProgram.scala`

**Behavior**:
- Free-slip velocity behavior
- **Plus**: Gradually reduces density and temperature at boundaries (95% retention per frame)
- Prevents density accumulation

**Use when**:
- Simulating smoke or gas that should fade near surfaces
- You want to prevent visible buildup at walls

**Visual effect**: Smoke fades as it approaches walls

**Tuning**:
```scala
val dissipationFactor = 0.95f  // Adjust between 0.8 (fast fade) to 0.99 (slow fade)
```

---

### 4. **OutflowBoundaryProgram** (Smoke Escape) ⭐ **Recommended for Smoke**
**File**: `OutflowBoundaryProgram.scala`

**Behavior**:
- **Top (y = gridSize-1)**: Outflow - smoke can escape freely upward with 90% density retention
- **Sides/Bottom**: Free-slip - fluid slides along domain walls
- **Obstacle Interiors**: Fully cleared (velocity, density, temperature = 0)
- **Obstacle Surfaces**: Free-slip - fluid flows tangentially around obstacles

**Use when**:
- Simulating smoke rising and escaping
- You want realistic open-top simulation
- Preventing pressure buildup from rising fluid

**Visual effect**: 
- Smoke rises naturally and exits through the top
- No accumulation at ceiling
- Flows smoothly around sides and obstacles

**Perfect for**: Rising smoke, steam, hot air

---

## Comparison Table

| Boundary Type | Velocity at Walls | Density Behavior | Best For |
|---------------|-------------------|------------------|----------|
| **No-Slip** | Zero (sticks) | Accumulates | Viscous fluids |
| **Free-Slip** | Tangential only | Accumulates | Water, low-viscosity |
| **Dissipative** | Tangential only | Fades at boundaries | Smoke with wall fade |
| **Outflow** | Exits at top, tangential at sides | Escapes upward | Rising smoke ⭐ |

---

## How to Switch Boundary Conditions

In `FullFluidSimulation.scala`, change the boundary program in the pipeline:

```scala
// Option 1: No-slip (original)
.addProgram(BoundaryProgram.create)(...)

// Option 2: Free-slip (no sticking)
.addProgram(FreeSlipBoundaryProgram.create)(...)

// Option 3: Free-slip with dissipation
.addProgram(DissipativeBoundaryProgram.create)(...)

// Option 4: Outflow at top (recommended for smoke) ⭐
.addProgram(OutflowBoundaryProgram.create)(...)
```

---

## Implementation Details

### Obstacle Surface Handling (All Programs)
All boundary programs now properly handle **fluid cells adjacent to obstacles**:

```scala
// Check all 6 neighbors for obstacles
val adjacentToObstacle = (solidXP || solidXM || solidYP || solidYM || solidZP || solidZM) && !isSolid

// Compute obstacle surface normal (sum of normals from all solid neighbors)
val normal = vec3(
  when(solidXP)(1.0f).otherwise(0.0f) + when(solidXM)(-1.0f).otherwise(0.0f),
  when(solidYP)(1.0f).otherwise(0.0f) + when(solidYM)(-1.0f).otherwise(0.0f),
  when(solidZP)(1.0f).otherwise(0.0f) + when(solidZM)(-1.0f).otherwise(0.0f)
)

// Apply free-slip at obstacle surface
val tangentialVelocity = velocity - normalize(normal) * (velocity dot normalize(normal))
```

This prevents:
- ✅ Velocity gradients causing "stripes" at obstacle boundaries
- ✅ Metastable states where simulation becomes static
- ✅ Artificial pressure buildup around obstacles

### Free-Slip Formula
```scala
// Project velocity onto surface (remove normal component)
val normalComponent = velocity dot normal
val tangentialVelocity = velocity - normal * normalComponent
```

### Outflow Formula
```scala
// Copy from below, allow only upward flow
velocity(top) = velocity(below)
velocity.y = max(velocity.y, 0)  // No downward flow
```

### Dissipation Formula
```scala
density(boundary) = density(boundary) * dissipationFactor
temperature(boundary) = temperature(boundary) * dissipationFactor
```

---

## Tips for Best Results

1. **For realistic smoke simulation**:
   - Use `OutflowBoundaryProgram`
   - Set moderate buoyancy (5.0-10.0)
   - Use low viscosity (0.0001)

2. **To prevent accumulation**:
   - Use any free-slip variant
   - Add dissipation if smoke still builds up
   - Consider outflow boundaries

3. **For water-like fluids**:
   - Use `FreeSlipBoundaryProgram`
   - Higher viscosity (0.001-0.01)
   - No dissipation

4. **For viscous fluids**:
   - Use `BoundaryProgram` (no-slip)
   - High viscosity (0.1+)

---

## Future Enhancements

Possible additions:
- [ ] Inflow boundaries (wind sources)
- [ ] Periodic boundaries (wrap-around)
- [ ] Mixed boundaries (different per wall)
- [ ] Pressure-outlet boundaries
- [ ] Velocity-inlet boundaries

