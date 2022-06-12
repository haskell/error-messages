---
title: Ambiguous record update
summary: Deprecated use of type directed name resolution.
severity: warning
flag: "-Wambiguous-fields"
extension: DuplicateRecordFields
introduced: 9.6.1
---

If multiple records share the same names for record fields, then it isn't always possible to correctly infer the type of a record update. Sometimes GHC can use additional type information to correctly disambiguate the field labels. This solution will no longer work in a future version of GHC. This warning notifies the user about these cases that will no longer work in the future.