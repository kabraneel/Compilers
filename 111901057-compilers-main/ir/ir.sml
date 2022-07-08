structure IR : sig
	type inst = (string, Temp.temp) MIPS.inst
	type inst2 = (MIPS.Label, MIPS.reg) MIPS.inst 

	type stmt = (string, Temp.temp) MIPS.stmt
	type stmt2 = (MIPS.Label, MIPS.reg) MIPS.stmt

	type prog = stmt list 
	type prog2 = stmt2 list 

	val ppInst : inst -> string 
	val ppStmt : stmt2 -> string
	val pp     : prog2 -> string


end = struct 

 
	(* r1, r2, r3   => MIPS.T0, MIPS.V0*)
	(*

		
		fn MIPS.T0 = "T0"
					 "t1" 


	*)

	type inst = (string, Temp.temp) MIPS.inst
	type inst2 = (MIPS.Label, MIPS.reg) MIPS.inst 

	type stmt = (string, Temp.temp) MIPS.stmt
	type stmt2 = (MIPS.Label, MIPS.reg) MIPS.stmt

	type prog = stmt list 
	type prog2 = stmt2 list 


	fun   ppInst 	(MIPS.Seq  (r1, r2, r3))			= "seq "  ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)      
			| ppInst (MIPS.Sge  (r1, r2, r3))		= "sge "  ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sgeu (r1, r2, r3))		= "sgeu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sgt  (r1, r2, r3))		= "sgt "  ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sgtu (r1, r2, r3))		= "sgtu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sle  (r1, r2, r3))		= "sle "  ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sleu (r1, r2, r3))		= "sleu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Slt  (r1, r2, r3))		= "slt "  ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Slti (r1, r2, r3))		= "slti " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sltu (r1, r2, r3))		= "sltu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sltiu (r1, r2, r3))		= "sltui " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sne  (r1, r2, r3))		= "sne " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
		  
		  (*Branch and jump instructions*)
			| ppInst (MIPS.B    (l1))				= "b " ^ l1
			| ppInst (MIPS.Bczt (l1))				= "bczt " ^ l1
			| ppInst (MIPS.Bczf (l1))				= "bczf " ^ l1
			| ppInst (MIPS.Beq  (r1, r2, l1))		= "beq " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Beqz (r1, l1))			= "beqz " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Bge  (r1, r2, l1))		= "bge " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bgeu  (r1, r2, l1))		= "bgeu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bgez (r1, l1))			= "bgez " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Bgezal (r1, l1))			= "bgezal " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Bgt  (r1, r2, l1))		= "bgt " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bgtz (r1, l1))			= "bgtz " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ble  (r1, r2, l1))		= "ble " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bleu  (r1, r2, l1))		= "bleu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Blez (r1, l1))			= "blez " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Bltzal (r1, l1))			= "bltzal " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Blt  (r1, r2, l1))		= "blt " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bltu  (r1, r2, l1))		= "bltu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bltz (r1, l1))			= "bltz " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Bne  (r1, r2, l1))		= "bne " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ l1
			| ppInst (MIPS.Bnez (r1, l1))			= "bnez " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
		  
		  (*Jump instructions*)
			| ppInst (MIPS.J    (l1))				= "j " ^ l1
			| ppInst (MIPS.Jal    (l1))				= "jal " ^ l1
			| ppInst (MIPS.Jalr   (r1))				= "jalr " ^ (Temp.tempToString1 r1)
			| ppInst (MIPS.Jr    (r1))				= "jr " ^ (Temp.tempToString1 r1)
		  
		  (*Load Instructions*)
			| ppInst (MIPS.La    (r1, l1))			= "la " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lb    (r1, l1))			= "lb " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lbu    (r1, l1))			= "lbu " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ld    (r1, l1))			= "ld " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lh    (r1, l1))			= "lh " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lhu    (r1, l1))			= "lhu " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lw    (r1, l1))			= "lw " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lwcz    (r1, l1))			= "lwcz " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lwl    (r1, l1))			= "lwl " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Lwr    (r1, l1))			= "lwr " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
 		  
 		  (*Store Intructions*)
 			| ppInst (MIPS.Sb    (r1, l1))			= "sb " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Sd    (r1, l1))			= "sd " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Sh    (r1, l1))		    = "sh " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Sw    (r1, l1))			= "sw " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Swcz    (r1, l1))			= "swcz " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Swl    (r1, l1))			= "swl " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Swr    (r1, l1))			= "swr " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ulh    (r1, l1))			= "ulh " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ulhu    (r1, l1))			= "ulhu " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ulw    (r1, l1))			= "ulw " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Ush    (r1, l1))			= "ush " ^ (Temp.tempToString1 r1) ^ ", " ^ l1
			| ppInst (MIPS.Usw    (r1, l1))			= "usw " ^ (Temp.tempToString1 r1) ^ ", " ^ l1

		  (*Arithmetic Instructions*)
			| ppInst (MIPS.Abs	(r1,r2))			= "abs " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Add 	(r1,r2,r3))			= "add " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Addi 	(r1,r2,r3))			= "addi " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Int.toString (r3))
			| ppInst (MIPS.Addu  (r1,r2,r3))			= "addu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Addiu (r1,r2,r3))			= "addiu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.And   (r1,r2,r3))			= "and " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Andi  (r1,r2,r3))			= "andi " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Div2  (r1,r2))			= "div " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Divu2 (r1,r2))			= "divu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Div   (r1,r2,r3))			= "div " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Divu  (r1,r2,r3))			= "divu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Mul   (r1,r2,r3))			= "mul " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Mulo  (r1,r2,r3))			= "mulo " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Mulou (r1,r2,r3))			= "mulou " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Mult  (r1,r2))			= "mult " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Multu (r1,r2))			= "multu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Neg   (r1,r2))			= "neg " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Negu  (r1,r2))			= "negu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Nor   (r1,r2,r3))			= "nor " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Not   (r1,r2))			= "not " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2)
			| ppInst (MIPS.Or    (r1,r2,r3))			= "or " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Ori   (r1,r2,r3))			= "ori " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Rem   (r1,r2,r3))			= "rem " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Remu  (r1,r2,r3))			= "remu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Rol   (r1,r2,r3))			= "rol " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Ror   (r1,r2,r3))			= "ror " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sll   (r1,r2,r3))			= "sll " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sllv  (r1,r2,r3))			= "sllv " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sra   (r1,r2,r3))			= "sra " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Srav  (r1,r2,r3))			= "srav " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Srl   (r1,r2,r3))			= "srl " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Srlv  (r1,r2,r3))			= "srlv " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Sub   (r1,r2,r3))			= "sub " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Subu  (r1,r2,r3))			= "subu " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Xor   (r1,r2,r3))			= "xor " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Xori  (r1,r2,r3))			= "xori " ^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) ^ ", " ^ (Temp.tempToString1 r3)
			| ppInst (MIPS.Li    (r1,r2))			= "li " ^ (Temp.tempToString1 r1) ^ ", " ^ (Int.toString (r2))   
			| ppInst (MIPS.Lui   (r1,r2))			= "lui "^ (Temp.tempToString1 r1) ^ ", " ^ (Temp.tempToString1 r2) 

		  (*Data Movement Instructions*)
    		| ppInst (MIPS.Move (r1, r2))        	= "move "^ (Temp.tempToString1 r1)^", "^(Temp.tempToString1 r2) 
			| ppInst (MIPS.Mfhi (l1))				= "mfhi " ^ (Temp.tempToString1 l1)
			| ppInst (MIPS.Mflo (l1))				= "mflo " ^ (Temp.tempToString1 l1)
			| ppInst (MIPS.Mthi (l1))				= "mthi " ^ (Temp.tempToString1 l1)
			| ppInst (MIPS.Mtlo (l1))				= "mtlo " ^ (Temp.tempToString1 l1)
			| ppInst (MIPS.Mfcz (l1, l2))			= "mfcz " ^ (Temp.tempToString1 l1) ^ ", " ^ (Temp.tempToString1 l2)
			| ppInst (MIPS.Mtcz (l1, l2))			= "mtcz " ^ (Temp.tempToString1 l1) ^ ", " ^ (Temp.tempToString1 l2)

		(*Trap and exceptions*)
			| ppInst (MIPS.Rfe)						= "rfe"
			| ppInst (MIPS.Syscall)					= "syscall"
			| ppInst (MIPS.Break (n))				= "break" ^ Int.toString n
			| ppInst (MIPS.Nop)						= "nop"

	fun ppfinalInst x = ppInst (Translate1.convertInst x)

	fun ppDir (MIPS.Globl s) = ".globl " ^ s 
		| ppDir (MIPS.Data(n))  = ".data"
		| ppDir (MIPS.Asciiz s)  = ".asciiz " ^ s
		| ppDir (MIPS.Text s)  = ".text"
		| ppDir _			=	"unknown"
	
	fun 	ppStmt (MIPS.Inst (i)) 			= ppfinalInst i 
        |	ppStmt (MIPS.Direcs (d)) 		= ppDir d
        |   ppStmt (MIPS.label (l))			= MIPS.printlabel(l) ^ ":"

    fun pp [] = ""
		|   pp (x::xs) = (ppStmt x)^"\n"^(pp xs)

end