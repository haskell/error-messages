---
title: Ambiguous record update (multiple modules)
---

Both records `R1` and `R2` share the same field `x`. For this reason, the compiler cannot infer a type for the record update `r { x = 1 }`.
If the records are defined in separate modules, then the forward compatible way to solve this error is to use fully qualified field names.

## Error Message

```
messages/GHC-99339/singlemodule/before/Error.hs:8:12: error: [GHC-99339]
    • Record update is ambiguous, and requires a type signature
    • In the expression: r {x = 1}
      In an equation for ‘update’: update r = r {x = 1}
  |
8 | update r = r { x = 1 }
  |            ^^^^^^^^^^^
```
