fun   RegToTemp MIPS.T0 = 0
    | RegToTemp MIPS.T1 = 1
    | RegToTemp MIPS.T2 = 2
    | RegToTemp MIPS.T3 = 3
    | RegToTemp MIPS.T4 = 4
    | RegToTemp MIPS.T5 = 5
    | RegToTemp MIPS.T6 = 6
    | RegToTemp MIPS.T7 = 7
    | RegToTemp MIPS.V0 = 8
    | RegToTemp MIPS.A0 = 9
    | RegToTemp (MIPS.Imm x) = x
    | RegToTemp _		= 10

exception Bleh


structure Translate1 : sig
	
	type inst1 = (string, Temp.temp) MIPS.inst 
	type inst2 = (MIPS.Label, MIPS.reg) MIPS.inst 
	val convertInst : inst2 -> inst1

end = struct 

	type inst1 = (string, Temp.temp) MIPS.inst 
	type inst2 = (MIPS.Label, MIPS.reg) MIPS.inst 
	(*type prog = stmt list *)

	fun   convertInst 	(MIPS.Seq  (r1, r2, r3))	= MIPS.Seq(RegToTemp r1, RegToTemp r2, RegToTemp r3)      
		  
		  (*Load Instructions*)
			| convertInst (MIPS.La    (r1, l1))			= MIPS.La(RegToTemp r1, MIPS.printlabel(l1))
 		  
		  (*Arithmetic Instructions*)
			| convertInst (MIPS.Add 	(r1,r2,r3))			= MIPS.Add(RegToTemp r1, RegToTemp r2, RegToTemp r3)
			| convertInst (MIPS.Addi 	(r1,r2,r3))			= MIPS.Addi(RegToTemp r1, RegToTemp r2, RegToTemp r3)
			| convertInst (MIPS.Mul   (r1,r2,r3))			= MIPS.Mul(RegToTemp r1, RegToTemp r2, RegToTemp r3)
			| convertInst (MIPS.Sub   (r1,r2,r3))			= MIPS.Sub(RegToTemp r1, RegToTemp r2, RegToTemp r3)
			| convertInst (MIPS.Li    (r1,r2))			= MIPS.Li(RegToTemp r1, RegToTemp r2)   
			| convertInst (MIPS.J    (l1))				= MIPS.J(MIPS.printlabel(l1))


		  (*Data Movement Instructions*)
    		| convertInst (MIPS.Move (r1, r2))        	= MIPS.Move(RegToTemp r1, RegToTemp r2) 

		(*Trap and exceptions*)
			| convertInst (MIPS.Syscall)					= MIPS.Syscall
			| convertInst (MIPS.Bgt(r1, r2, l1))			= MIPS.Bgt (RegToTemp r1, RegToTemp r2, MIPS.printlabel(l1))
			| convertInst (MIPS.Blt(r1, r2, l1))			= MIPS.Blt (RegToTemp r1, RegToTemp r2, MIPS.printlabel(l1))
			| convertInst (MIPS.Beq(r1, r2, l1))			= MIPS.Beq (RegToTemp r1, RegToTemp r2, MIPS.printlabel(l1))



			|  convertInst	_						= raise Bleh


end