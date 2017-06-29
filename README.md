For expression:
I have CstI, Var, Prim, Call, NewArray, ReadElt, WriteElt

For Statement:
I have Asgn, If-else statement, for loop, while loop, block, print, printString, return statement

I also have function and Procedure. And inside Procedure, I have main.

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

I implement: 
function(method)
procedure
local variable.
literal strings.
function

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

In the compiler code, I have the Astract Syntax tree, parser, pretty print, code generation, and testing.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

For the testing,
I pass:
(print100 folder)print the numbers between 1 and 100
(fact folder)calculate and print the factorial of numbers between 1 and 100

I can not pass:
(div folder)
print the numbers (in order) between 1 and 100 that are divisible by neither 3 nor 5
becasue the mod doesn't work. I have error message about all 4 movzbl mismatch the opprent.

(printNum folder) it could print negative and positive number and it works correctly.
(expression folder) it tests add, minus, mult, and div. And all of these work correctly.
(printStrArray folder) it print String of words by using array(type expr).




