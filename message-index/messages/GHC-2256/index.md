---
title: Ambiguous record update
summary: Deprecated use of type directed name resolution
severity: warning
flag: "-Wambiguous-fields"
extension: "DuplicateRecordFields"
introduced: 9.6.1
---

If multiple records share the same names for record fields, then it isn't always possible to correctly infer the type of a record update. One solution to this problem was to supply a type signature which helps GHC to correctly resolve which record labels belong to which record types. This solution will no longer work in a future version of GHC. 