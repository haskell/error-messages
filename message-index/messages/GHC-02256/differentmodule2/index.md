---
title: Ambiguous record update (multiple modules) alternative
---

In this example GHC uses the type signature of `update` to resolve which record the label `x` belongs to. This functionality will be deprecated in the future.
If the records are defined in different modules, then one solution is to use fully qualified field labels. A nice trick is to import the module containing the record qualified as the name of the record.

## Warning

```
GHC-2256/differentmodule2/before/Module2.hs:9:16: warning: [-Wambiguous-fields] [GHC-02256]
    The record update r {x = 1} with type R1 is ambiguous.
    This will not be supported by -XDuplicateRecordFields in future releases of GHC.
  |
9 | update r = r { x = 1 }
  |                ^^^^^
```