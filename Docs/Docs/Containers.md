# Containers

Containers and views used by Blend2D.

Blend2D needs certain containers to function, but it cannot use Delphi containers other than `TArray<T>`. In addition, the underlying representation of all classes that inherit from [TBLObjectCore](Reference/Blend2D/classes/TBLObjectCore.md) need to provide reference counting, even for containers, so they can be shared across threads without requiring to do any extra work by Blend2D users.

In addition, Blend2D embraces small data optimization (often described as SSO - small string optimization), which is utilized by [TBLString](Reference/Blend2D/classes/TBLString.md), [TBLArray&lt;T>](Reference/Blend2D/classes/TBLArray_1.md), [TBLBitArray](Reference/Blend2D/classes/TBLBitArray.md) and other specialized containers.

## Views & Common Types

* [TBLArrayView&lt;T>](Reference/Blend2D/classes/TBLArrayView_1.md) - read-only view of an array (any array, not just [TBLArray&lt;T>](Reference/Blend2D/classes/TBLArray_1.md)).
* [TBLStringView](Reference/Blend2D/types/TBLStringView.md) - read-only view of a UTF-8 encoded string.
* [TBLRange](Reference/Blend2D/classes/TBLRange.md) - start/end range, which can be used with sequential containers.

## Sequential Containers

* [TBLArray&lt;T>](Reference/Blend2D/classes/TBLArray_1.md) - growable array of `T` elements. Can hold both primitive types and Blend2D objects.
* [TBLString](Reference/Blend2D/classes/TBLString.md) - 8-bit null terminated string, usually UTF-8 encoded, but it's not a requirement.

## Bit Containers

* [TBLBitArray](Reference/Blend2D/classes/TBLBitArray.md) - a dense bit-array (stores bits starting from 0 to size, sequentially).
