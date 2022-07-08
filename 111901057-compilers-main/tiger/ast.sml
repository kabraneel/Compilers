structure Ast = struct

datatype BinOp = Plus
                | Minus
                | Mul
                

datatype Expr  = Const of int
               | Var   of string
               | Op    of Expr * BinOp * Expr


datatype Stmt  = Print of Expr
               | Assign of string * Expr
               | For   of string * int * int * Stmt list 



fun plus    a b                                           = Op (a, Plus, b)
fun minus   a b                                           = Op (a, Minus, b)
fun mul     a b                                           = Op (a, Mul, b)
fun assign  a b                                           = Assign (a, b)
fun for     (Var(x)) (Const(from)) (Const(to)) exp        = For (x, from, to, exp)


end