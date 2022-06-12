---
title: Too many type arguments in constructor pattern
---

## Error Message

```
TyArgs.hs:6:4: error: [GHC-01629]
    • Too many type arguments in constructor pattern for ‘Just’
      Expected no more than 1; got 2
    • In the pattern: Just @Int @Bool x
      In an equation for ‘f’: f (Just @Int @Bool x) = x
  |
6 | f (Just @Int @Bool x) = x
  |    ^^^^^^^^^^^^^^^^^
```
