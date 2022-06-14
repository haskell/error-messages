---
title: Type equality not in scope
summary: The type equality operator has not been imported into the current module
severity: warning
flag: "-Wcompat"
introduced: 9.6.1
---

In prior versions of GHC, the type equality operator `~` was built-in syntax. In more recent versions, it is an ordinary operator that is part of the `Prelude` and of `Data.Type.Equality`. Restricting imports from `Prelude` can result in `~` not being imported. For now, GHC has a compatibility warning to help migrate old code. In a future version of GHC, this warning will be an error.
