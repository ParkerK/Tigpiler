Tigpiler
========
*An SML based Tiger to MIPS compiler*

Requirements  
------------
+ [SML/NJ 110.73](http://www.smlnj.org/dist/working/110.73/index.html)

Setup
-----
+ Clone repo `git clone git@github.com:ParkerK/Tigpiler.git`
+ Change to the program directory `cd Tigpiler/RegisterAllocation`

Testing
-------
+ Run the test program to compile and run the binary `sml test.sml`

Status / Updates
----------------
We believe that this compiler is fully functional (though a tad dumb at times). In regards to
changes on past parts, we have changed quite a bit. On the Lexer, we went through and fixed all
of the issues pointed out. In addition, we found one where it could not handle 'then'. It still
won't handle illegal integers (e.g. var:=5a) but being that this was optional we simply encourage
any users to try to make illegal integers. 

For the Parser, the only thing wrong was the excluded documentation. You are reading the documentation
at this very moment, which proves that we now have documentation.

For Frame Analysis and IR we had full credit, but we still went though and fixed a few issues with
things like declarations.

We also fixed Instruction Selection to handle MOVES correctly, added a missing procEntryExit1 funtion,
and changed the psuedo assembly to valid MIPS.

In regards to register allocation our compiler will correctly create a control flow graph, liveness
analysis, and interference graphs. It will even color registers and will spill. It's not very bright
(we still love it though) though and will give up as soon as it detects something wrong.

Upon running `sml test.sml` the script will spit out the assembly representation of all of the test programs
that we have collected. You can run `sml test.sml` in any directory to see the output of that stage (e.g., running
`sml test.sml` in the IRTranslation directory will save the IR represnetation in the output directory). 



Parts
=====
1.Lexer (9/19)
---------------
To Fix:  

+ <del> A little too aggressive illegal escape char checker. e.g. \\ is a regular backslash. Currently your lexer reporting error </del>  
+ <del> Currently you seem to be checking unclosed comments by the use of escape characters. There's a better implementation. </del>
+ Need to handle illegal integers, e.g. var a:=5a
+ <del> There is a bug in your lexer, such that strings ending with f are not being processed </del>

2.Parser (10/17)
----------------
To Fix:  

+ <del> Missing documentation </del>

3.Semantic Analysis (10/31)
---------------------------
To Fix:  

+ If, while, ... expressions are returning errors when not supposed to, e.g. "if(5>3)..." returns integer required error.
+ Certain expressions should not return any value at all (see page 516). For example a:=(if (5>4) then 3) should return an error
+ For "if then else" expressions the types of "then" and "else" values must be the same
+ Need to handle recursive declarations: At the moment not throwing errors. (See page 120)
+ How are you handling cases where types have the same name as variables? Should it return an error?

4.Frame Analysis and Intermediate Representation (11/14)  
--------------------------------------------------------
To Fix:  

+ None

5.Instruction Selection (11/21)  
--------------------------------
To Fix:  

+ <del> Incorrect instructions used, ie 'LOAD' instead of 'lw' </del>

6. Register Allocation (12/5)  
-----------------------------

7. Working compiler, produces assembly (12/9)  
---------------------------------------------
