structure Translate = struct

fun   tempToReg 0 = MIPS.T0
    | tempToReg 1 = MIPS.T1
    | tempToReg 2 = MIPS.T2
    | tempToReg 3 = MIPS.T3
    | tempToReg 4 = MIPS.T4
    | tempToReg 5 = MIPS.T5
    | tempToReg 6 = MIPS.T6
    | tempToReg 7 = MIPS.T7
    | tempToReg 8 = MIPS.V0
    | tempToReg 9 = MIPS.A0
    | tempToReg _ = MIPS.Imm(1)

fun  tempToLabel x = MIPS.TempLabel(x)

(*
fun varHandler (env, x, t) = let 
                                val res =  case (HashTable.find env x) of
                                    (SOME (v)) =>  ([MIPS.Inst(MIPS.Li(t, v))], env)
                                    |   NONE =>     let 
                                            val r1 = Temp.newtemp()
                                            val newenv = HashTable.insert env (x, r1)
                                            in
                                                ([MIPS.Inst(MIPS.Move(t, tempToReg(r1)))], newenv)
                                            end  
                            in 
                                res 
                            end
*)


fun compileExpr env  (Ast.Const x) t = ([MIPS.Inst(MIPS.Li(tempToReg(t),MIPS.Imm(x)))], env)
    | compileExpr env (Ast.Op(x1, Ast.Plus, x2)) t  =  let  
                                                        val r1 = Temp.newtemp()
                                                        val r2 = Temp.newtemp()
                                                        val (leftres, newenv)       = compileExpr env x1 r1 
                                                        val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                                    in 
                                                        (leftres@rightres@[MIPS.Inst(MIPS.Add(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_newenv)
                                                    end

    | compileExpr env (Ast.Op(x1, Ast.Minus, x2)) t  =  let  
                                                        val r1 = Temp.newtemp()
                                                        val r2 = Temp.newtemp()
                                                        val (leftres, newenv)       = compileExpr env x1 r1 
                                                        val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                                    in 
                                                        (leftres@rightres@[MIPS.Inst(MIPS.Sub(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_newenv)
                                                    end    

    | compileExpr env (Ast.Op(x1, Ast.Mul, x2)) t  =  let  
                                                        val r1 = Temp.newtemp()
                                                        val r2 = Temp.newtemp()
                                                        val (leftres, newenv)       = compileExpr env x1 r1 
                                                        val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                                    in 
                                                        (leftres@rightres@[MIPS.Inst(MIPS.Mul(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_newenv)
                                                    end                                                

    | compileExpr env (Ast.Var x) t              = let 
                                                    val res =  case (AtomMap.find(env, Atom.atom(x))) of
                                                        (SOME (v)) =>  ([MIPS.Inst(MIPS.Move(tempToReg(t), tempToReg(v)))], env)
                                                        |   NONE =>     
                                                                let 
                                                                val r1 = Temp.newtemp()
                                                                val newenv = (*HashTable.insert env (x, r1)*) AtomMap.insert(env, Atom.atom(x), r1)
                                                                in
                                                                    ([MIPS.Inst(MIPS.Move(tempToReg(t), tempToReg(r1)))], newenv)
                                                                end  
                                                in 
                                                    res 
                                                end



(*fun binopHandler (x, Ast.Plus, y, t)    = let  
                                            val r1 = Temp.newtemp()
                                            val r2 = Temp.newtemp()
                                            val (leftres, newenv)       = compileExpr env x1 r1 
                                            val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                        in 
                                            ([leftres@rightres@MIPS.Inst(MIPS.Add(t, tempToReg(r1), tempToReg(r2)))], new_newenv)
                                        end

    | binopHandler (x, Ast.Sub, y, t)   =  let  
                                            val r1 = Temp.newtemp()
                                            val r2 = Temp.newtemp()
                                            val (leftres, newenv)       = compileExpr env x1 r1 
                                            val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                        in 
                                            ([leftres@rightres@MIPS.Inst(MIPS.Sub(t, tempToReg(r1), tempToReg(r2)))], new_newenv)
                                        end

    | binopHandler (x, Ast.Mul, y, t)  = let  
                                            val r1 = Temp.newtemp()
                                            val r2 = Temp.newtemp()
                                            val (leftres, newenv)       = compileExpr env x1 r1 
                                            val (rightres, new_newenv)  = compileExpr newenv x2 r2
                                        in 
                                            ([leftres@rightres@MIPS.Inst(MIPS.Mul(t, tempToReg(r1), tempToReg(r2)))], new_newenv)
                                        end
*)  

fun forhelper loopStmts looping breaker loopLabel breakLabel x1 x2 =   [   
                                                                        MIPS.Inst(MIPS.Li(tempToReg(looping),MIPS.Imm(x1))), 
                                                                        MIPS.Inst(MIPS.Li(tempToReg(breaker),MIPS.Imm(x2))),
                                                                        MIPS.label(tempToLabel(loopLabel)),
                                                                        MIPS.Inst(MIPS.Beq(tempToReg(looping), tempToReg(breaker), tempToLabel(breakLabel))) (*If the looping variable becomes equal to the ending, break*)
                                                                    ]
                                                                    @loopStmts
                                                                    @[
                                                                        MIPS.Inst(MIPS.Addi(tempToReg(looping),tempToReg(looping),MIPS.Imm(1))), (*Incrementing the loop variable*)
                                                                        MIPS.Inst(MIPS.J(tempToLabel(loopLabel))), 
                                                                        MIPS.label(tempToLabel(breakLabel))
                                                                    ]


fun compileStmt env (Ast.Assign (x,y)) =
                        let 
                            val res1 =  case (*(HashTable.find env x)*) (AtomMap.find(env, Atom.atom(x))) of
                            (SOME (v)) => let 
                                val (ans, newenv) = compileExpr env y v
                                            in (ans, newenv) end 
                            | NONE   => 

                            let
                                val newvar = Temp.newtemp()
                                val newenv = (*HashTable.insert env (x, newvar)*) AtomMap.insert(env, Atom.atom(x), newvar)
                                val (res, new_newenv) = compileExpr newenv y newvar
                            in 
                                (res, new_newenv)
                            end
                        in 
                            res1
                        end

    | compileStmt env (Ast.Print     x) =
                        let 
                            val newvar = Temp.newtemp()
                            val (res, newenv) = compileExpr env x newvar
                            val new_newenv = (*HashTable.insert newenv ("a0", newvar)*) AtomMap.insert(env, Atom.atom("A0"), newvar)

                        in 
                            (res@[MIPS.Inst(MIPS.Move(MIPS.A0, tempToReg(newvar)))]@[MIPS.Inst(MIPS.Li(MIPS.V0, MIPS.Imm(1)))]@[MIPS.Inst(MIPS.Syscall)] , new_newenv)
                        end

    | compileStmt env (Ast.For   (x, x1, x2, stmtList)) = 
                        let
                            val looping     = Temp.newtemp()
                            val breaker     = Temp.newtemp()
                            val loopLabel   = Temp.newlabel()
                            val breakLabel  = Temp.newlabel()
                            val up_Env      = AtomMap.insert(env,Atom.atom(x),looping)
                            fun     Compileloop e []        = []
                                | Compileloop e (x::xs)     = 
                                                            let
                                                                val (res,new_env) = compileStmt e x
                                                            in
                                                            res@Compileloop new_env xs
                                                            end
                            val loopStmts = Compileloop up_Env stmtList
                        in
                            (forhelper loopStmts looping breaker loopLabel breakLabel x1 x2, env)
                        end

fun compileprog env []          = [MIPS.Inst(MIPS.Li(MIPS.V0, MIPS.Imm(10)))]@[MIPS.Inst(MIPS.Syscall)]
    | compileprog env (x :: xs)   = let 
                                    val (res, newenv) = compileStmt env x  
                                in 
                                    res@compileprog newenv xs 
                                end

fun compile prog = [MIPS.Direcs(MIPS.Globl("main")), MIPS.label(MIPS.UserDefined("main"))]@compileprog AtomMap.empty prog

end