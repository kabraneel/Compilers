signature TEMP =
  sig
     
     type temp = int
     type label = int
     val newlabel   : unit -> label
     val newtemp  : unit -> temp
     val labelToString : label -> string
     val tempToString : temp -> string
     val tempToString1 : int -> string


  end

structure Temp :> TEMP = struct

   exception outofregs
   type temp  = int
   type label = int

   val nextTemp   = ref 0
   val nextLabel  = ref 0


   fun newtemp _ = (if(!nextTemp > 7) then (raise outofregs) else (nextTemp := !nextTemp + 1); !nextTemp) 
   fun newlabel _ = (nextLabel := !nextLabel + 1; !nextLabel)


   fun tempToString t = "t" ^ Int.toString(!nextTemp)
   fun tempToString1 t = "t" ^ Int.toString(t)

   fun labelToString t = "l" ^ Int.toString(!nextTemp)


end
