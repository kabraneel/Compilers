signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function.
*)
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

(* If the data structure was supposed to be persistent the definition
   of new will not be of this form

   addNode : graph -> graph * node
   addEdge : graph -> node * node -> graph
*)


val addEdge : 'a graph -> (node * node) -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(* you might want functions that go over all the nodes

maps, folds etc
*)
 

end

exception Bleh

functor MkGraph () :> GRAPH = struct 

	type node = word

	structure NodeHashKey : HASH_KEY = struct 
		type hash_key = node 
		fun hashVal w = w
		fun sameKey(w1, w2) = w1 = w2
	end 

    structure NodeSet = HashSetFn (NodeHashKey)

    type nodeSet = NodeSet.set


	type 'a graph = { 
		labels : (node, 'a)  HashTable.hash_table,
			(* edges *)
		successors   : (node, nodeSet) HashTable.hash_table,
		predecessors : (node, nodeSet) HashTable.hash_table,
			(*next node*)
		nextNode : node ref
	}

    fun empty () = { labels = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Bleh),
		successors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Bleh),
		predecessors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, Bleh),
		nextNode   = ref (Word.fromInt 0)
	}

	fun newNode (g : 't graph) a = let 

		val node = !(#nextNode g)
		val temp = HashTable.insert (#labels g) (node, a)
		
		(*we need empty sets*)
		val x1 = NodeSet.mkEmpty 0
		val x2 = NodeSet.mkEmpty 0

		val _ = HashTable.insert (#successors g) (node, x1)
		val _ = HashTable.insert (#predecessors g) (node, x2)

		val _ = (#nextNode g) := node + (Word.fromInt 1)

	in 
		node 
	end 

	fun addEdge(g : 't graph) (n1, n2) = let 
		val succsetn1 = HashTable.lookup (#successors g) n1 
		val _         = NodeSet.add (succsetn1,n2) 
		val predsetn2 = HashTable.lookup (#predecessors g) n2 
		val _         = NodeSet.add (predsetn2,n1) 
	in 
		()
	end

	fun succ (g : 't graph) (n) = let
		val succset = HashTable.lookup (#successors g) n
	in
		NodeSet.listItems succset
	end

	fun pred (g: 't graph) (n) = let 
		val predset = HashTable.lookup (#predecessors g) n 
	in
		NodeSet.listItems predset
	end 

	fun label (g : 't graph) n = let 
		val 
			x = HashTable.lookup (#labels g) n 
		in
			x
	end 

	fun clear (g : 'a graph) = let 
		val _ = HashTable.clear (#labels g)
		val _ = HashTable.clear (#successors g)
		val _ = HashTable.clear (#predecessors g)
		val _ = (#nextNode g) := Word.fromInt 0
	in
		()
	end 

	fun all (g : 't graph) = let 
		val x = HashTable.listItemsi (#labels g)
		fun f (y : (node * 'a)) = (#1 y)
	in 
		map f x
	end 

end


structure A = MkGraph() 
val g 		= A.empty() : int A.graph
val n1 		=  A.newNode g 5
val n2 		=  A.newNode g 9
val n3 		=  A.newNode g 10
val n4 		=  A.newNode g 10
val n5 		=  A.newNode g 10

val x1		= A.addEdge g (n1, n2)
val x2		= A.addEdge g (n1, n4)
val x3		= A.addEdge g (n1, n5)

val t1 		= A.succ g n1 
val t2		= A.pred g n2
val t3		= A.label g n5


structure B = MkGraph() 
val g 		= B.empty() : int list B.graph
val n1 		=  B.newNode g [5]
val n2 		=  B.newNode g [9, 11]
val n3 		=  B.newNode g [10, 10]
val n4 		=  B.newNode g [10, 15]
val n5 		=  B.newNode g [10]

val x1		= B.addEdge g (n1, n2)
val x2		= B.addEdge g (n1, n4)
val x3		= B.addEdge g (n1, n5)

val t1 		= B.succ g n1 
val t2		= B.pred g n2
val t3		= B.label g n2

