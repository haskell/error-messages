---
title: Ambiguous record update
summary: Cannot infer correct type for record update.
severity: error
introduced: 9.6.1
---

Sometimes the type of a record update can be inferred from the names of the fields in the record update. But if multiple records use the same labels, this isn't always possible.
