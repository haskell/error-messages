---
title: Ambiguous record update
---

In this example GHC uses the type signature of `update` to resolve which record the label `x` belongs to. This functionality will be deprecated in the future.
If both records are defined in the same module, then one solution is to rename one of the record fields to make it unique.

## Warning

```
messages/GHC-2256/example1/before/Example.hs:10:16: warning: [-Wambiguous-fields] [GHC-02256]
    The record update r {x = 1} with type R1 is ambiguous.
    This will not be supported by -XDuplicateRecordFields in future releases of GHC.
   |
10 | update r = r { x = 1 }
   |                ^^^^^
```