#JC Language

##Description
JC is funtional, dynamical typing language that has syntaxt like C and functionality similar to JavaScript. In case of statements like ``if else``, ```for```, ```{...}```, etc. it returns ```nil``` (like ```undefined``` in JavaScript). Other expressions like variable, functions, etc. return their own value. User declares variables or functions using ```var``` or ```fun``` term. It is required to include ```main``` function to execute program from file (like in C), but one can use interpreter in interact mode to make computations after each expression. We can assign function to variable, we can pass anonymous function to another function and get function as a result of execution of function. I tried to make JC maximally crash resistant: all types could be treated like boolean, getting uninitialized variables returns nil (including uninitialized element of array), passing to function more parameters than is declared is still ok!

###Data types
1. Basic
	- nil
	- boolean
	- integer
	- string
2. Complex
	- array (polymorfic)
	- function

###Expressions
1. Logic: ||, &&, (works with all data types)
2. Arithmetic: ==, !=, >, <
3. Mathematic: +, -, *, /
4. C++ like assign: =, +=, *=, /=, ++, --
5. Unary: -, +, !

###Declarations
1. Variables: ```var``` (with assign or no)
2. Functions: ```fun name(arg) {...}``` / anonymous: ```fun (arg) {...}```

###Statements
1. Assignment,
2. If, if-else, 
3. While, for-loop, 
4. Return