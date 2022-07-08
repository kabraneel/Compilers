# Changelog

### LAB 1

1. Added functioning for the Division('/') operator in reverse-polish. This changes the abstract and concrete syntax, so I had to change the `ast.sml` file, grammar file `expr.grm` and lex files `rp.lex` and `expr.lex`.
2. Added functioning for brackets. This only causes a change in the concrete syntax and no changes in the abstract syntax or the tree. Therefore, I only changed the grammar file `expr.grm` and the lex files `expr.lex`.

### LAB 2

1. Added support for the MIPs AST in the file `target\mips.sml`.
2. Needed to make datatypes to capture the registers, instructions, directives and statements of the MIPs Assembly language.
3. Added support for 32 registers present in the MIPs assembly language.
4. Wrote respective print functions for all the registers, instructions, directives and statements to make clear printing operations along with the proper MIPs syntax (with commas seperated values and `$` signs).

### LAB 3

1. Implemented the `ast.sml` file in the tiger directory to capture the AST of the tiger language.
2. Changed the `tiger.lex` file and the `tiger.grm` files from the `expr.lex` and `expr.grm` from the reverse-polish directory to make it suitable to use with tiger.
3. Implemented the `temp.sml` file which takes care of the temporary variable allocation.
4. Made the `ir.mlb` and the `tc.mlb` files (from the rp directory)
5. Made the `translate.sml` file.

### LAB 4 

1. Put all files in one folder called `target` so that using the Makefile would be easier. Made the Makefile similar to the one found in rp.
2. In the `translate.sml` added functions for compiling expressions, statements, programs. The expressions included the Plus, Minus and Multiplication binary operations, along with the `Const` and `Var`.
3. Debugged the `translate.sml` file, took a long time. Replaced Hashtables with AtomMaps, since they do not give the new map on insertion and return unit instead.
4. Tested the compiler without for loops. Worked fine.
5. Updated the `ast.sml` file and added functionality for for loops in the `stmt` datatype.
6. Updated the `expr.grm` and `expr.lex` files to add the functionality for for loops
7. Updated the `translate.sml` file. In this file, we just needed to add the definition for `for` in the CompileStmt function. The other statements were already done. Made the function accordingly.
8. Created a small test case to check the functionality.

### LAB 5

1. Made the `basicblocks.sml` file inside the tiger folder.
2. In the basicblocks file, made a structure which captures the jump instructions and labels of any languages.
3. Then made a functor that has functions to divide the program into blocks, which are a set of statements that do not contain any looping instructions.
4. Made appropriate functions to pretty print the blocks.
5. Made a structure called `MIPSBasicBlocks` which captures the jump and label instructions of the MIPS language.
6. Added the support for separating into blocks and printing the blocks in the Makefile and `ec.sml`

### LAB 6

1. Made the `graph.sml` file in the `src` directory.
2. Implemented a `Mkgraph()` functor that can implement graphs of any type along with some basic functionalities.
3. Implemented the basic graph functions like adding nodes, edges, printing labels, etc.