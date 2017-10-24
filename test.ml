let or = λx y. (if x then true else (if y then true else false)) 
let rec fib = λn. (if ((or $ (n == 1)) $ (n == 2)) then 1 else ((fib $ (n - 1)) + (fib $ (n - 2))))
fib $ 11