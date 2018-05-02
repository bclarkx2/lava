## Interpreter

interpreter project for EECS 345, Spring 2018

# Contributors

Brian Clark (bpc30)
Danny Miles (djm230)
Kaius Reed  (kmr117)

#Naming Convention
All state functions will be prefixed as state.x
All value functions will be prefixed as value.y

#Abstraction
All functional logic, unless explictly self-documenting, will be performed in clearly-named helper functions.
i.e. PLS NO CAR(CDR(CAR(CDR(LIS)))) IN MAIN FUNCTIONS THANK YOU

#Grammar
	#stmt-list -> <stmt> | <stmt><stmt-list>

#Interface Defintions

	#value: value(stmt, state) -> (value)
	the value function will call helper functions, which are defined for each operator
	may or may not have typechecking
	ONLY RETURN STATEMENTS HAVE VALUE
	
	
	#Expressions
	All expressions return either booleans or values; the only things that care about that are other expressions,
	so they do not need to be treated separately.

	#State Logic
	
	#interpret(file-name) -> the value of the file; the parse tree will evaluate to a single value
		calls value(parse_tree, s) with s = empty
		
	#state: state(stmt, state) -> state
		returns a new state with the stmt-list applied to that state
		
	
#GENERAL FLOW
1. Pass filename to interpret
2. Interpret passes parse tree to value
3. Value does
	null? '()
	lis? (car lis) -> is value(car lis, s) null? 
			if so, value(cdr lis, state(car lis, s))
			if not, return value
	at this point, we are in a single sublist. therefore, check:
	assign?
	declare?
	if?
	while?
	else it's an expression, which is evaluated by a helper function. 
4. State does stuff. do your magic
	
