---
title: Ambiguous record update (multiple modules)
---

In this example GHC uses the type signature of `update` to resolve which record the label `x` belongs to. This functionality will be deprecated in the future.
If the records are defined in different modules, then one solution is to use fully qualified field labels.

## Warning