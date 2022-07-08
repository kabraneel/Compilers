signature INST = 
sig
	type t 
	val isJumpLike 		: 	t -> bool
	val isTarget		: 	t -> bool
end

structure MIPSInst : INST = struct
	
	type t = (MIPS.Label, MIPS.reg) MIPS.stmt
	
	fun 	isJumpLike (MIPS.label(_)) = false
		|	isJumpLike (MIPS.Direcs(_)) = false
	
		| 	isJumpLike (MIPS.Inst(x)) = case x of
			MIPS.B    (_)						=> true
			| MIPS.Bczt (_)						=> true
			| MIPS.Bczf (_)						=> true
			| MIPS.Beq  (_, _, _)				=> true
			| MIPS.Beqz (_, _)					=> true
			| MIPS.Bge  (_, _, _)				=> true
			| MIPS.Bgeu  (_, _, _)				=> true
			| MIPS.Bgez (_, _)					=> true
			| MIPS.Bgezal (_, _)				=> true
			| MIPS.Bgt  (_, _, _)				=> true
			| MIPS.Bgtz (_, _)					=> true
			| MIPS.Ble  (_, _, _)				=> true
			| MIPS.Bleu  (_, _, _)				=> true
			| MIPS.Blez (_, _)					=> true
			| MIPS.Bltzal (_, _)				=> true
			| MIPS.Blt  (_, _, _)				=> true
			| MIPS.Bltu  (_, _, _)				=> true
			| MIPS.Bltz (_, _)					=> true
			| MIPS.Bne  (_, _, _)				=> true
			| MIPS.Bnez (_, _)					=> true
		  
		  (*Jump instructions*)
			| MIPS.J    (_)						=>	true
			| MIPS.Jal    (_)					=> 	true
			| MIPS.Jalr   (_)					=> 	true
			| MIPS.Jr    (_)					=> 	true
			| _									=> false


	fun isTarget (MIPS.label(_))				= true 
		| isTarget _							= false


end

functor BasicBlocks (I : INST) = struct

	structure Inst = I

	type block = I.t list 

	fun 	helperbasicBlocks ([], b, ans)			= 	ans@[b]
		| 	helperbasicBlocks (i :: xs, b, ans)		= 	case (I.isJumpLike(i), I.isTarget(i)) of 
														(true, false) 	=> helperbasicBlocks (xs, [], ans@[(b@[i])])
													|  	(false, false) 	=> helperbasicBlocks (xs, b@[i], ans)
													| 	(false, true)  	=> helperbasicBlocks (xs, [i], ans@[b])
													|	_				=> helperbasicBlocks (xs, [], ans@[b]@[[i]])

	fun basicBlocks x = helperbasicBlocks (x, [], [])

	fun printblock [] = ""
		| printblock (x :: xs) = (MIPS.printProg x) ^ "\n" ^ (printblock xs) 

end



structure MIPSBasicBlock = BasicBlocks (MIPSInst)
