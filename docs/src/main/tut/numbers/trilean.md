---
layout: docs
title:  "Trilean"
section: "numbers"
source: "core/shared/src/main/scala/spire/math/Trilean.scala"
scaladoc: "#spire.math.Trilean"
---

## Trilean

This type resembles `Boolean`, but has three values instead of two:

 - `Trilean.True` (equivalent to true)
 - `Trilean.False` (equivalent to false)
 - `Trilean.Unknown`

Trilean supports the same operations that `Boolean` does, and as long as all
values are `True` or `False`, the results will be the same. However, the truth
tables have to be extended to work with an `Unknown` value:

### Not: !x

| not |   |
|-----|---|
| T   | F |
| U   | U |
| F   | T |

### And: x && y

| and | T | U | F |
|-----|---|---|---|
| T   | T | U | F |
| U   | U | U | F |
| F   | F | F | F |

### Or: x || y

| or | T | U | F |
|----|---|---|---|
| T  | T | T | T |
| U  | T | U | U |
| F  | T | U | F |


Trilean is implemented as a value type, so in most cases it will
only have the overhead of a single Int. However, in some situations
it will be boxed.

