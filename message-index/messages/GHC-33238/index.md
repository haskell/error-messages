---
title: Impossible record update
summary: No type contains all the given fields.
severity: error
introduced: 9.6.1
---

When GHC tries to infer the type of a record update, it looks for a record which contains all the given fields. This error is emitted when no such record is found.