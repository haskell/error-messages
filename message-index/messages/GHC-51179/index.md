---
 title: Missing LambdaCase
 summary:  Missing LambdaCase language extension
 severity: error
 introduced: 9.6.1
 extension: LambdaCase
 ---

[Lambda Case extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html)
is a syntactic sugar for a lambda function which only operation is a pattern matching.
```
\case {"1" -> 1, "2" -> 2}
```
in lambda case notation is equivalent to
```
\x -> case x of {"1" -> 1, "2" -> 2}
```

When the extension is missing, the expression is interpreted as a regular lambda expression, which is incorrectly formatted.