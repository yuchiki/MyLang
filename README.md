# BintLang

言語処理系の習作

## 文法

```txt
expr :: =
  | @
  | (expr, expr)
  | x
  | let x = expr in expr
```
