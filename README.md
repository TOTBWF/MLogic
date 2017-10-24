# MLogic
MLogic is a parser, type checker, and interpreter for a subset of ML written in Prolog.
This includes HM Type Inference.

The basic syntax is as follows:

### Let Bindings
```ml
let f = 1
```

### Lambda Expressions
```ml
let inc = λx. (x + 1)
```

### Function Application
```ml
let foo = inc $ 2
```

### Recursive Functions
```ml
let or = λx y. (if x then true else (if y then true else false))
let rec fib = (if ((or $ (n == 1)) $ (n == 2)) then 1 else ((fib $ (n - 1)) + (fib $ (n - 2))))
```

### Usage
```mlogic <filepath>```
