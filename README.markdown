Tigpiler
========
*An SML based Tiger to MIPS compiler*

Parts
=====

1. Lexer (9/19)
---------------
To Fix:  

+ <strike> A little too aggressive illegal escape char checker. e.g. \\ is a regular backslash. Currently your lexer reporting error </strike>  
+ <del> Currently you seem to be checking unclosed comments by the use of escape characters. There's a better implementation. </del>
+ Need to handle illegal integers, e.g. var a:=5a
+ <del> There is a bug in your lexer, such that strings ending with f are not being processed </del>

2. Parser (10/15)
----------------

3. Semantic Analysis (10/31)
----------------------------

4. Frame Analysis and Intermediate Representation (11/14)
---------------------------------------------------------

5. Instruction Selection (11/21)
--------------------------------

6. Register Allocation (12/5)
-----------------------------

7. Working compiler, produces assembly (12/9)
---------------------------------------------