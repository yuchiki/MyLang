# BintLang

言語処理系の習作

## 文法

```txt
expr :: =
  | @                     // leaf value
  | (expr, expr)          // branch
  | x                     // variable
  | let x = expr in expr  // assignment
  | fun x -> expr         // function
  | expr expr             // function application
```
