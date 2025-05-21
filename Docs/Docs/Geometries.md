# Geometries

Geometries, paths, and transformations.

Blend2D offers various geometry records and objects that can be used with either [TBLPath](Reference/Blend2D/classes/TBLPath.md) for path building or [TBLContext](Reference/Blend2D/classes/TBLContext.md) for rendering. In general there are two categories - [TBLPath](Reference/Blend2D/classes/TBLPath.md), which specifies a 2D path composed of path segments, and lightweight geometries such as [TBLRect](Reference/Blend2D/classes/TBLRect.md), [TBLRoundRect](Reference/Blend2D/classes/TBLRoundRect.md), etc... which are only described by a trivial Delphi record.

## Paths

* [TBLPath](Reference/Blend2D/classes/TBLPath.md) - path container.
  * [TBLPathCmd](Reference/Blend2D/types/TBLPathCmd.md) - path command specifies the type of a path segment withing [TBLPath](Reference/Blend2D/classes/TBLPath.md).
  * [TBLPathFlags](Reference/Blend2D/types/TBLPathFlags.md) - flags associated with [TBLPath](Reference/Blend2D/classes/TBLPath.md).
  * [TBLPathReverseMode](Reference/Blend2D/types/TBLPathReverseMode.md) - reverse mode accepted by [TBLPath.AddReversedPath](Reference/Blend2D/classes/TBLPath.md/#AddReversedPath_1).
  * [TBLPathView](Reference/Blend2D/classes/TBLPathView.md) - view providing all necessary variables to inspect and iterate a [TBLPath](Reference/Blend2D/classes/TBLPath.md).

## Path Operations

* [TBLStrokeOptions](Reference/Blend2D/classes/TBLStrokeOptions.md) - holds stroking options.
* [TBLOffsetMode](Reference/Blend2D/types/TBLOffsetMode.md) - path offsetting mode.
* [TBLFlattenMode](Reference/Blend2D/types/TBLFlattenMode.md) - path flattening mode.
* [TBLStrokeCap](Reference/Blend2D/types/TBLStrokeCap.md) - stroke cap option.
* [TBLStrokeCapPosition](Reference/Blend2D/types/TBLStrokeCapPosition.md) - stroke cap position (can be specified separately).
* [TBLStrokeJoin](Reference/Blend2D/types/TBLStrokeJoin.md) - stroke join option.
* [TBLStrokeTransformOrder](Reference/Blend2D/types/TBLStrokeTransformOrder.md) - the order of a transformation when rendering a stroked path or geometry.

## Lightweight Geometries and Records

* [TBLPoint](Reference/Blend2D/classes/TBLPoint.md) - 2D point composed of `[X, Y]` values (64-bit floats).
* [TBLPointI](Reference/Blend2D/classes/TBLPointI.md) - 2D point composed of `[X, Y]` values (32-bit integers).
* [TBLSize](Reference/Blend2D/classes/TBLSize.md) - 2D size composed of `[W, H]` values (64-bit floats).
* [TBLSizeI](Reference/Blend2D/classes/TBLSizeI.md) - 2D size composed of `[W, H]` values (32-bit integers).
* [TBLBox](Reference/Blend2D/classes/TBLBox.md) - 2D rectangular area composed of `[X0, Y0, X1, Y1]` values (64-bit floats).
* [TBLBoxI](Reference/Blend2D/classes/TBLBoxI.md) - 2D rectangular area composed of `[X0, Y0, X1, Y1]` values (32-bit integers).
* [TBLRect](Reference/Blend2D/classes/TBLRect.md) - 2D rectangular area composed of `[X, Y, W, H]` values (64-bit floats).
* [TBLRectI](Reference/Blend2D/classes/TBLRectI.md) - 2D rectangular area composed of `[X, Y, W, H]` values (32-bit integers).
* [TBLRoundRect](Reference/Blend2D/classes/TBLRoundRect.md) - rounded rectangle within `[X, Y, W, H]` with radius `[RX, RY]` (64-bit floats).
* [TBLCircle](Reference/Blend2D/classes/TBLCircle.md) - circle at `[CX, CY]` with radius `R` (64-bit floats).
* [TBLEllipse](Reference/Blend2D/classes/TBLEllipse.md) - ellipse at `[CX, CY]` with radius `[RX, RY]` (64-bit floats).
* [TBLArc](Reference/Blend2D/classes/TBLArc.md) - arc at `[CX, CY]` with radius `[RX, RY]` and `Start` + `Sweep` values (64-bit floats).
* [TBLLine](Reference/Blend2D/classes/TBLLine.md) - line segment from `[X0, Y0]` to `[X1, Y1]` (64-bit floats).
* [TBLTriangle](Reference/Blend2D/classes/TBLTriangle.md) - triangle having `[X0, Y0]`, `[X1, Y1]`, and `[X2, Y2]` vertices (64-bit floats).

## Geometry Types

* [TBLGeometryDirection](Reference/Blend2D/types/TBLGeometryDirection.md) - specifies a direction.
* [TBLGeometryType](Reference/Blend2D/types/TBLGeometryType.md) - specifies a type of a geometry argument (low-level).
* [TBLFillRule](Reference/Blend2D/types/TBLFillRule.md) - specifies a fill rule (used by both [TBLPath](Reference/Blend2D/classes/TBLPath.md) and [TBLContext](Reference/Blend2D/classes/TBLContext.md)).
* [TBLHitTest](Reference/Blend2D/types/TBLHitTest.md) - specifies a result of hit-testing.

## Transformations

* [TBLMatrix2D](Reference/Blend2D/classes/TBLMatrix2D.md) - 2D transformation matrix (affine).
* [TBLTransformOp](Reference/Blend2D/types/TBLTransformOp.md) - transformation operation (low-level).
* [TBLTransformKind](Reference/Blend2D/types/TBLTransformKind.md) - transformation type.
