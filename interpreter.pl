/***
A skeleton for Assignment 3 on PROP HT2017 at DSV/SU.
Peter Idestam-Almquist, 2017-12-14.
***/

/*** 
If you choose to use the tokenizer, uncomment the following line of code:*/
:- [tokenizer].


/***
If you choose to use the tokenizer then call run1/2 as for example:
?- run1('program1.txt','myparsetree1.txt').
***/
run1(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	output_result(OutputFile,ParseTree,VariablesOut).

/***
If you choose to NOT use the tokenizer then call run2/2, as for example:
?- run2([a,=,1,*,2,+,'(',3,-,4,')',/,5,;],'myparsetree1.txt').
***/
run2(Program,OutputFile):-
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	output_result(OutputFile,ParseTree,VariablesOut).

output_result(OutputFile,ParseTree,Variables):- 
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'), 
	nl(OutputStream), 
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream), 
	write(OutputStream,'EVALUATION:'), 
	nl(OutputStream), 
	write_list(OutputStream,Variables), 
	close(OutputStream).
	
writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).
	
writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg), 
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).
	
write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term), 
	nl(Stream).
	
write_list(_Stream,[]). 
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value), 
	nl(Stream), 
	write_list(Stream,Vars).
	
	
/*
	DEBUG [c=4-(3-(1*2+(3-4)/5+6/(5/2)))+(1*2+(3-4)/5), b=4-(3-(1*2+(3-4)/5+6/(5/2))), a=1*2+(3-4)/5]
*/
test:-
	tokenize('program2.txt',Tokens), 
	parse(Tree, Tokens, []), 
	db-output('outTree.txt',Tree), 
	open('outTokens.txt',write,Outstream), 
	write(Outstream, Tokens),
	write(Outstream, '\n'),
	write(Outstream, Tree),
	close(Outstream).
	
test_eval1(Tokens, Tree, Eval):-
	tokenize('E:/code/prolog/prop.assign3/program1.txt',Tokens), 
	assignment(Tree, Tokens, []), 
	evaluate_assign(Tree,[],Eval).

test_eval2(Tree, VariablesOut):-
	parse(Tree, [ '{',a, =, 1, *, 2, +, '(', 3, -, 4, ')', /, 5, ;, b, =, 4, -, 3, -, a, +, 6, /, 5, /, 2, ;, c, =, b, +, a, ;,'}' ] , []), 
	evaluate(Tree,[], VariablesOut).	
	
write-out(Program,OutputFile):-
	block(ParseTree,Program,[]),
	db-output(OutputFile,ParseTree).
	
db-output(OutputFile, ParseTree) :-
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'), 
	nl(OutputStream), 
	writeln_term(OutputStream,0,ParseTree),
	close(OutputStream).
	
/***
parse(-ParseTree)-->
	A grammar defining our programming language,
	and returning a parse tree.
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */
parse(ParseTree) -->
	block(ParseTree).

block(block(LC, S, RC)) -->
	left_curly(LC),
	statements(S),
	right_curly(RC).
statements(statements) -->		
	[].
statements(statements(A, S)) --> 	
	assignment(A), 
	statements(S).
assignment(assignment(I, AO, E, Semi)) --> 	
	ident(I), 
	assign_op(AO), 
	expression(E), 
	semicolon(Semi).
expression(expression(T)) -->		
	term(T).
expression(expression(T, AO, E))	-->		
	term(T), 
	add_op(AO), 
	expression(E).
expression(expression(T, SO, E))	-->		
	term(T), 
	sub_op(SO), 
	expression(E).
term(term(F)) -->		
	factor(F).
term(term(F, MO,T)) -->		
	factor(F), 
	mult_op(MO), 
	term(T).
term(term(F, DO,T)) -->		
	factor(F), 
	div_op(DO), 
	term(T).
factor(factor(N)) -->		
	int(N).
factor(factor(I)) -->		
	ident(I).
factor(factor(LP, E, RP)) -->		
	left_paren(LP), 
	expression(E), 
	right_paren(RP).
int(int(I)) -->		
	[I], 
	{integer(I)}.
ident(ident(I)) -->		
	[I], 
	{atom(I)}.
assign_op(assign_op) --> [=].
add_op(add_op) --> [+].
sub_op(sub_op) --> [-].
mult_op(mult_op) --> [*].
div_op(div_op) --> [/].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].
semicolon(semicolon) --> [;].
/***

	
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */	

evaluate(ParseTree, VariablesIn, VariablesOut):-
    eval_block(ParseTree, VariablesIn, VariablesOut).

eval_block(block(left_curly,Stmts,right_curly), VariablesIn, VariablesOut) :-
    eval_stmts(Stmts, VariablesIn, VariablesOut).

	
%%%%%% STATEMENTS %%%%%%	
eval_stmts(statements, Variables, Variables).

eval_stmts(statements(Assign,Stmts), VariablesIn, VariablesOut) :-
    eval_assign(Assign, VariablesIn, VariablesOutTemp),
    eval_stmts(Stmts, VariablesOutTemp, VariablesOut).


	
eval_assign(assignment(ident(Ident),assign_op,Expr,semicolon), VariablesIn, VariablesOut) :-
    eval_expr(Expr, VariablesIn, ExprValue),
	add_ident_list(Ident = ExprValue, VariablesIn, VariablesOut).

	
%%%%%% EXPRESSION %%%%%%
eval_expr(expression(_,Operator,Expr), Value, VariablesIn, ValueOut) :-
    next_expr_value(Expr, VariablesIn, NextVal),
    eval(Value, Operator, NextVal, Result),
    eval_expr(Expr, Result, VariablesIn, ValueOut).


eval_expr(_Node, Value, _VariablesIn, Value).


eval_expr(expression(Term,Operator,Expr), VariablesIn, ValueOut) :-
    eval_term(Term, VariablesIn, Value),
    next_expr_value(Expr, VariablesIn, NextVal),
    eval(Value, Operator, NextVal, Result),
    eval_expr(Expr, Result, VariablesIn, ValueOut).


eval_expr(expression(Term), VariablesIn, ValueOut) :-
    eval_term(Term, VariablesIn, ValueOut).

	
next_expr_value(expression(Term), VariablesIn, ValueOut) :-
    eval_term(Term, VariablesIn, ValueOut).
next_expr_value(expression(Term,_,_), VariablesIn, ValueOut) :-
    eval_term(Term, VariablesIn, ValueOut).

	
	
%%%%%% TERM %%%%%%
eval_term(term(_,Operator,Term), Value, VariablesIn, ValueOut) :-
    next_term_value(Term, VariablesIn, NextVal),
    eval(Value, Operator, NextVal, Result),
    eval_term(Term, Result, VariablesIn, ValueOut).


eval_term(term(factor(_)), Value, _VariablesIn, Value).


eval_term(term(Factor,Operator,Term), VariablesIn, Value) :-
    eval_factor(Factor, VariablesIn, FactorValue),
    next_term_value(Term, VariablesIn, NextVal),
    eval(FactorValue, Operator, NextVal, Result),
    eval_term(Term, Result, VariablesIn, Value).


eval_term(term(Factor), VariablesIn, Value) :-
    eval_factor(Factor, VariablesIn, Value).

	
next_term_value(term(Factor), VariablesIn, Value) :-
    eval_factor(Factor, VariablesIn, Value).
next_term_value(term(Factor,_,_), VariablesIn, Value) :-
    eval_factor(Factor, VariablesIn, Value).

	

%%%%%% FACTOR %%%%%%
eval_factor(factor(left_paren,Expr,right_paren), VariablesIn, Value) :-
    eval_expr(Expr, VariablesIn, Value).


eval_factor(factor(int(Int)), _VariablesIn, Int).

eval_factor(factor(ident(Ident)), VariablesIn, Value) :-
    ident_from_list(Ident, VariablesIn, Value).

	
%%%%%% HELPERS %%%%%%	
add_ident_list(Ident, VariablesIn, [Ident|VariablesIn]).

ident_from_list(_,[],0).
ident_from_list(Ident, [Ident = Value |_], Value):- !. %% Red cut
ident_from_list(Ident, [_|RestList], Value) :-
	ident_from_list(Ident, RestList, Value).
	
	
eval(Value1, add_op, Value2, Value):-	Value is Value1 + Value2.
eval(Value1, sub_op, Value2, Value):-	Value is Value1 - Value2.
eval(Value1, mult_op, Value2, Value):-	Value is Value1 * Value2.
eval(Value1, div_op, Value2, Value):-	Value is Value1 / Value2.