---
title: Too many type arguments in constructor pattern
summary: Too many type applications to a constructor in a pattern.
severity: error
introduced: 9.6.1
---

Every type application has to be "consumed" by a type variable in the
corresponding type.  It is an error to apply more types than there are
type variables in a type.
