structure MIPS = struct
		
		(*Registers of the MIPS language*)
		datatype reg = Zero | At | V0 | V1 | A0 | A1 | A2 | A3 
					 | T0   | T1 | T2 | T3 | T4 | T5 | T6 | T7
					 | S0   | S1 | S2 | S3 | S4 | S5 | S6 | S7
					 | T8   | T9 | K0 | K1 | Gp | Sp | Fp | Ra | Imm of int

		(*Function for printing registers*)
		fun printreg Zero		= "$zero"
		  | printreg At			= "$at"
		  | printreg V0			= "$v0"
		  | printreg V1			= "$v1"
		  | printreg A0			= "$a0"
		  | printreg A1			= "$a1"
		  | printreg A2			= "$a2"
		  | printreg A3			= "$a3"
		  | printreg T0			= "$t0"
		  | printreg T1			= "$t1"
		  | printreg T2			= "$t2"
		  | printreg T3			= "$t3"
		  | printreg T4			= "$t4"
		  | printreg T5			= "$t5"
		  | printreg T6			= "$t6"
		  | printreg T7			= "$t7"
		  | printreg S0			= "$s0"
		  | printreg S1			= "$s1"
		  | printreg S2			= "$s2"
		  | printreg S3			= "$s3"
		  | printreg S4			= "$s4"
		  | printreg S5			= "$s5"
		  | printreg S6			= "$s6"
		  | printreg S7			= "$s7"
		  | printreg T8			= "$t8"
		  | printreg T9			= "$t9"
		  | printreg K0			= "$k0"
		  | printreg K1			= "$k1"
		  | printreg Gp			= "$gp"
		  | printreg Sp			= "$sp"
		  | printreg Fp			= "$fp"
		  | printreg Ra			= "$ra"
		  | printreg (Imm i)    = Int.toString i

		(*Label datatype*)
		datatype Label = UserDefined of string  (* main, fib *)
               		   | TempLabel   of int     (* $L0 $L1 ... *)

        (*Function for printing labels*)
        fun printlabel (UserDefined s) =  s
   		  | printlabel (TempLabel i)   =  "label" ^ Int.toString i

   		(*Datatype to capture all instructions*)
        datatype ('l, 't) inst 	= Abs  	of 't * 't (*The Arithmetic and Logical Operations*)
								| Add  	of 't * 't * 't
								| Addi 	of 't * 't * 't
								| Addu  of 't * 't * 't
								| Addiu  of 't * 't * 't
								| And  	of 't * 't * 't
								| Andi  	of 't * 't * 't
								| Div2  	of 't * 't
								| Divu2  of 't * 't
						   		| Div  	of 't * 't * 't
								| Divu  	of 't * 't * 't
								| Mul  	of 't * 't * 't
								| Mulo  	of 't * 't * 't
								| Mulou  of 't * 't * 't
								| Mult  	of 't * 't
								| Multu  of 't * 't
								| Neg  	of 't * 't
								| Negu  	of 't * 't
								| Nor  	of 't * 't * 't
								| Not  	of 't * 't
								| Or  	of 't * 't * 't
								| Ori 	of 't * 't * 't
								| Rem  	of 't * 't * 't
								| Remu  	of 't * 't * 't
								| Rol  	of 't * 't * 't
								| Ror  	of 't * 't * 't
								| Sll  	of 't * 't * 't
								| Sllv  	of 't * 't * 't
								| Sra 	of 't * 't * 't
								| Srav  	of 't * 't * 't
								| Srl  	of 't * 't * 't
								| Srlv  	of 't * 't * 't
								| Sub  	of 't * 't * 't
								| Subu  	of 't * 't * 't
								| Xor  	of 't * 't * 't
								| Xori 	of 't * 't * 't

								(* Constant-Manipulating Instructions *)
								| Li 	of 't * 't             
								| Lui 	of 't * 't
		                       
		                       (* Comparison Instructions *)
								| Seq 	of 't * 't * 't       
								| Sge 	of 't * 't * 't
								| Sgeu 	of 't * 't * 't
								| Sgt 	of 't * 't * 't
								| Sgtu 	of 't * 't * 't
								| Sle 	of 't * 't * 't
								| Sleu 	of 't * 't * 't
								| Slt 	of 't * 't * 't
								| Slti 	of 't * 't * 't
								| Sltu 	of 't * 't * 't
								| Sltiu 	of 't * 't * 't
								| Sne 	of 't * 't * 't

								(* Branch and Jump Instructions *)
								| B 		of 'l                   
								| Bczt 	of 'l
								| Bczf 	of 'l
								| Beq 	of 't * 't * 'l
								| Beqz 	of 't * 'l
								| Bge  	of 't * 't * 'l
								| Bgeu 	of 't * 't * 'l 
								| Bgez 	of 't * 'l
								| Bgezal of 't * 'l
								| Bgt 	of 't * 't * 'l
								| Bgtz 	of 't * 'l
								| Ble 	of 't * 't * 'l
								| Bleu 	of 't * 't * 'l
								| Blez 	of 't * 'l
								| Bltzal of 't * 'l
								| Blt 	of 't * 't * 'l
								| Bltu 	of 't * 't * 'l
								| Bltz 	of 't * 'l
								| Bne 	of 't * 't * 'l
								| Bnez 	of 't * 'l
								| J 		of 'l
								| Jal 	of 'l
								| Jalr 	of 't
								| Jr 	of 't

								(* Load Instructions *)
								| La 	of 't * 'l             
								| Lb 	of 't * 'l
								| Lbu 	of 't * 'l
								| Ld 	of 't * 'l
								| Lh 	of 't * 'l
								| Lhu 	of 't * 'l
								| Lw 	of 't * 'l
								| Lwcz 	of 't * 'l
								| Lwl 	of 't * 'l
								| Lwr 	of 't * 'l

								(* Store Instructions *)
								| Sb 	of 't * 'l             
								| Sd 	of 't * 'l
								| Sh 	of 't * 'l
								| Sw 	of 't * 'l
								| Swcz 	of 't * 'l
								| Swl 	of 't * 'l
								| Swr 	of 't * 'l
								| Ulh 	of 't * 'l
								| Ulhu 	of 't * 'l
								| Ulw 	of 't * 'l
								| Ush 	of 't * 'l
								| Usw 	of 't * 'l

								(* Traps and exceptions *)
								| Rfe
								| Syscall
								| Break of int
								| Nop

								(*Data Movement*)
                        		| Move of 't * 't
								| Mfhi of 't
								| Mflo of 't
								| Mthi of 't
								| Mtlo of 't
								| Mfcz of 't * 't
								| Mtcz of 't * 't

		(*Function to print instructions*)
		fun   prInst (Seq  (r1, r2, r3))		= "seq "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)      
			| prInst (Sge  (r1, r2, r3))		= "sge "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sgeu (r1, r2, r3))		= "sgeu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sgt  (r1, r2, r3))		= "sgt "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sgtu (r1, r2, r3))		= "sgtu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sle  (r1, r2, r3))		= "sle "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sleu (r1, r2, r3))		= "sleu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Slt  (r1, r2, r3))		= "slt "  ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Slti (r1, r2, r3))		= "slti " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sltu (r1, r2, r3))		= "sltu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sltiu (r1, r2, r3))		= "sltui " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sne  (r1, r2, r3))		= "sne " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
		  
		  (*Branch and jump instructions*)
			| prInst (B    (l1))				= "b " ^ printlabel(l1)
			| prInst (Bczt (l1))				= "bczt " ^ printlabel(l1)
			| prInst (Bczf (l1))				= "bczf " ^ printlabel(l1)
			| prInst (Beq  (r1, r2, l1))		= "beq " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Beqz (r1, l1))			= "beqz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Bge  (r1, r2, l1))		= "bge " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bgeu  (r1, r2, l1))		= "bgeu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bgez (r1, l1))			= "bgez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Bgezal (r1, l1))			= "bgezal " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Bgt  (r1, r2, l1))		= "bgt " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bgtz (r1, l1))			= "bgtz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ble  (r1, r2, l1))		= "ble " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bleu  (r1, r2, l1))		= "bleu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Blez (r1, l1))			= "blez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Bltzal (r1, l1))			= "bltzal " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Blt  (r1, r2, l1))		= "blt " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bltu  (r1, r2, l1))		= "bltu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bltz (r1, l1))			= "bltz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Bne  (r1, r2, l1))		= "bne " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printlabel(l1)
			| prInst (Bnez (r1, l1))			= "bnez " ^ printreg(r1) ^ ", " ^ printlabel(l1)
		  
		  (*Jump instructions*)
			| prInst (J    (l1))				= "j " ^ printlabel(l1)
			| prInst (Jal    (l1))				= "jal " ^ printlabel(l1)
			| prInst (Jalr   (r1))				= "jalr " ^ printreg(r1)
			| prInst (Jr    (r1))				= "jr " ^ printreg(r1)
		  
		  (*Load Instructions*)
			| prInst (La    (r1, l1))			= "la " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lb    (r1, l1))			= "lb " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lbu    (r1, l1))			= "lbu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ld    (r1, l1))			= "ld " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lh    (r1, l1))			= "lh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lhu    (r1, l1))			= "lhu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lw    (r1, l1))			= "lw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lwcz    (r1, l1))			= "lwcz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lwl    (r1, l1))			= "lwl " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Lwr    (r1, l1))			= "lwr " ^ printreg(r1) ^ ", " ^ printlabel(l1)
 		  
 		  (*Store Intructions*)
 			| prInst (Sb    (r1, l1))			= "sb " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Sd    (r1, l1))			= "sd " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Sh    (r1, l1))		    = "sh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Sw    (r1, l1))			= "sw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Swcz    (r1, l1))			= "swcz " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Swl    (r1, l1))			= "swl " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Swr    (r1, l1))			= "swr " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ulh    (r1, l1))			= "ulh " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ulhu    (r1, l1))			= "ulhu " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ulw    (r1, l1))			= "ulw " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Ush    (r1, l1))			= "ush " ^ printreg(r1) ^ ", " ^ printlabel(l1)
			| prInst (Usw    (r1, l1))			= "usw " ^ printreg(r1) ^ ", " ^ printlabel(l1)

		  (*Arithmetic Instructions*)
			| prInst (Abs	(r1,r2))			= "abs " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Add 	(r1,r2,r3))			= "add " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Addi 	(r1,r2,r3))			= "addi " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Addu  (r1,r2,r3))			= "addu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Addiu (r1,r2,r3))			= "addiu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (And   (r1,r2,r3))			= "and " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Andi  (r1,r2,r3))			= "andi " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Div2  (r1,r2))			= "div " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Divu2 (r1,r2))			= "divu " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Div   (r1,r2,r3))			= "div " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Divu  (r1,r2,r3))			= "divu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Mul   (r1,r2,r3))			= "mul " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Mulo  (r1,r2,r3))			= "mulo " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Mulou (r1,r2,r3))			= "mulou " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Mult  (r1,r2))			= "mult " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Multu (r1,r2))			= "multu " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Neg   (r1,r2))			= "neg " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Negu  (r1,r2))			= "negu " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Nor   (r1,r2,r3))			= "nor " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r2)
			| prInst (Not   (r1,r2))			= "not " ^ printreg(r1) ^ ", " ^ printreg(r2)
			| prInst (Or    (r1,r2,r3))			= "or " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Ori   (r1,r2,r3))			= "ori " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Rem   (r1,r2,r3))			= "rem " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Remu  (r1,r2,r3))			= "remu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Rol   (r1,r2,r3))			= "rol " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Ror   (r1,r2,r3))			= "ror " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sll   (r1,r2,r3))			= "sll " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sllv  (r1,r2,r3))			= "sllv " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sra   (r1,r2,r3))			= "sra " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Srav  (r1,r2,r3))			= "srav " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Srl   (r1,r2,r3))			= "srl " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Srlv  (r1,r2,r3))			= "srlv " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Sub   (r1,r2,r3))			= "sub " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Subu  (r1,r2,r3))			= "subu " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Xor   (r1,r2,r3))			= "xor " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Xori  (r1,r2,r3))			= "xori " ^ printreg(r1) ^ ", " ^ printreg(r2) ^ ", " ^ printreg(r3)
			| prInst (Li    (r1,r2))			= "li " ^ printreg(r1) ^ ", " ^ printreg(r2)   
			| prInst (Lui   (r1,r2))			= "lui "^ printreg(r1) ^ ", " ^ printreg(r2) 

		  (*Data Movement Instructions*)
    		| prInst (Move (r1, r2))        	= "move "^ printreg(r1)^", "^printreg(r2) 
			| prInst (Mfhi (l1))				= "mfhi " ^ printreg(l1)
			| prInst (Mflo (l1))				= "mflo " ^ printreg(l1)
			| prInst (Mthi (l1))				= "mthi " ^ printreg(l1)
			| prInst (Mtlo (l1))				= "mtlo " ^ printreg(l1)
			| prInst (Mfcz (l1, l2))			= "mfcz " ^ printreg(l1) ^ ", " ^ printreg(l2)
			| prInst (Mtcz (l1, l2))			= "mtcz " ^ printreg(l1) ^ ", " ^ printreg(l2)

		(*Trap and exceptions*)
			| prInst (Rfe)						= "rfe"
			| prInst (Syscall)					= "syscall"
			| prInst (Break (n))				= "break" ^ Int.toString n
			| prInst (Nop)						= "nop"

		datatype directives = Align of int 
							| Ascii of string
							| Asciiz of string
							| Byte of int list	
							| Data of string
							| Extern of string * int 
							| Globl of string
							| Half of int list
							| Kdata of string 
							| Ktext of string
							| Space of int
							| Text of string
							| Word of string list
		
		(*function to convert int list to string*)
		fun listToString [] = ""
			| listToString (x :: xs) = (Int.toString x) ^ (listToString xs)

		(*function to convert string list to string *)
		fun strlistToString [] = ""
			| strlistToString (x :: xs) = (x) ^ (strlistToString xs)
		
		(*Function to print directives*)
		fun prDir   (Align (n))		=   ".align " ^ Int.toString n
			| prDir (Ascii (n))  	=   ".ascii " ^ n
			| prDir (Asciiz(n))		=   ".asciiz " ^ n
			| prDir (Byte(n))		=   ".byte " ^ listToString(n)
			| prDir (Data(n))		=   ".data " ^ n
			| prDir (Extern(s,n))	=   ".extern " ^ s ^ " " ^ Int.toString n
			| prDir (Globl(s))		=   ".globl " ^ s
			| prDir (Half(x))		=   ".half " ^ listToString(x)
			| prDir (Kdata(s))		=   ".kdata " ^ s
			| prDir (Ktext(s))		=   ".ktext " ^ s
			| prDir (Space(n))		=   ".space " ^ Int.toString n
			| prDir (Text(s))		=   ".text " ^ s
			| prDir (Word(x))		=   ".word " ^ strlistToString(x)

		(*A statement can either be an instruction or a directive*)
		datatype ('l,'t) stmt = Inst of ('l, 't) inst
                			| Direcs  of directives
                			| label of Label

        (*Function to print a statement*)
        fun 	prStmt (Inst (i)) 			= prInst i 
        	|	prStmt (Direcs (d)) 		= prDir d
        	|   prStmt (label (l))			= (printlabel l) ^ ":"


        fun printProg [] = ""
		|   printProg (x::xs) = (prStmt x)^"\n"^(printProg xs)

end