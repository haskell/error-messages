---
title: No record has fields x, y and z
---

The compiler tries to infer the type of the record `r` in the `update` function by looking at the fields that are updated. But there is no record which contains all three fields `x`, `y` and `z`, so the compiler has to emit an error. Once the update to the field `z` is removed, the compiler is able to infer that the record `R1` was meant.

## Error Message

```
messages/GHC-33238/example1/before/Error.hs:8:12: error: [GHC-33238]
    • No type has all these fields: ‘x’, ‘y’, ‘z’
    • In the expression: r {x = 1, y = 2, z = 3}
      In an equation for ‘update’: update r = r {x = 1, y = 2, z = 3}
  |
8 | update r = r { x = 1, y = 2, z = 3 }
  |            ^^^^^^^^^^^^^^^^^^^^^^^^^

```
