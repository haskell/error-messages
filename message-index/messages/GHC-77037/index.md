---
title: An import item does not have an explicit import list
summary: A name is imported neither with qualification nor explicitly
severity: warning
introduced: 9.6.1
flag: -Wmissing-import-lists
---

The most basic import statement just names a module, and loads all the
exports from that module into the scope.  This can make it hard to
find the location of the definition of a name, so there is both
explicit imports (with a list of names that you want to load in
parenthesis), and qualified imports (with a name that you have to
prefix to every use of a name, like this:

```
import qualified Data.Yaml as Yaml

fun = BS.putStr (Yaml.encode ...)
```

If this warning is enabled, using qualified or explicit imports only
is encouraged.
