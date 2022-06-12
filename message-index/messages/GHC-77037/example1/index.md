---
title: An import item does not have an explicit import list
---

## Warning Message

```
Warning.hs:5:1: warning: [-Wmissing-import-lists]
    The module ‘Data.Maybe’ does not have an explicit import list
  |
5 | import Data.Maybe
  | ^^^^^^^^^^^^^^^^^

Warning.hs:6:1: warning: [-Wmissing-import-lists] [GHC-77037]
    The import item ‘Maybe(..)’ does not have an explicit import list
  |
6 | import Data.Maybe (Maybe (..))
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
