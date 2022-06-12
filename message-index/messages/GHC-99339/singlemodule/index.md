---
title: Ambiguous record update
---

Both records `R1` and `R2` share the same field `x`. For this reason, the compiler cannot infer a type for the record update `r { x = 1 }`. But once a type signature is provided, the compiler knows which record and record field is meant.

## Error Message

```
messages/GHC-99339/example1/before/Error.hs:8:12: error: [GHC-99339]
    • Record update is ambiguous, and requires a type signature
    • In the expression: r {x = 1}
      In an equation for ‘update’: update r = r {x = 1}
  |
8 | update r = r { x = 1 }
  |            ^^^^^^^^^^^
```
