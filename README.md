# BintLang

言語処理系の習作

# 使用方法

- dotnet run --project BintLang
- cat <programfile> | dotnet run --project BintLang

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
