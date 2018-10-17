let rec f x1 =
let x = x1 -(-x1) in
let a = -x in
let k = (a-(-a)) in
let x2 = x + (-k) in
x2 -(-x2) in
print_int ((f 125)+(f 125))
