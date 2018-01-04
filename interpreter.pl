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
	DEBUG
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

evaluate(block(left_curly,Statements,right_curly),+VariablesIn,-VariablesOut):-
	evaluate_statements(Statements,VariablesIn,VariablesOut).

%% Empty statement -> finished	
evaluate_statements(statements,+VariablesIn,-VariablesIn).

evaluate_statements(statements(Assign,Statements),VariablesIn,VariablesOut):-
	evaluate_assign(Assign,VariablesIn,VariablesNew),
	evaluate_statements(Statements,VariablesNew,VariablesOut).

evaluate_assign(assignment(ident(Ident),assign_op,Expr,_),VariablesIn,[Ident = Value | VariablesIn]):-
	evaluate_expr(Expr,nil,0,Value,VariablesIn).

evaluate_expr(expression(Term),Op,Prec,Res,VariablesIn):-
	evaluate_term(Term,nil,0,Res2,VariablesIn),
	eval(Prec,Res2,Op,Res).
evaluate_expr(expression(Term,Op1,Expr),Op2,Prec,Res,VariablesIn):-
	evaluate_term(Term,nil,0,Res2,VariablesIn),
	evaluate_expr(Expr,Op1,Res3,Res,VariablesIn),
	eval(Prec,Res2,Op2,Res3).

evaluate_term(term(Factor),Op,Prec,Res,VariablesIn):-
	evaluate_factor(Factor,Res2,VariablesIn),
	eval(Prec,Res2,Op,Res).
evaluate_term(term(Factor,Op1,Term),Op2,Prec,Res,VariablesIn):-
	evaluate_factor(Factor,Res2,VariablesIn),
	evaluate_term(Term,Op1,Res3,Res,VariablesIn),
	eval(Prec,Res2,Op2,Res3).

evaluate_factor(factor(int(Int)),Int,_).
evaluate_factor(factor(ident(Ident)),Res,VariablesIn):-
	evaluate_ident(Ident,VariablesIn,Res).
evaluate_factor(factor(_,Expr,_), Res,VariablesIn):-
	evaluate_expr(Expr,nil,0,Res,VariablesIn).

eval(X,Y,add_op,X+Y).
eval(X,Y,sub_op,X-Y).
eval(X,Y,mult_op,X*Y).
eval(X,Y,div_op,X/Y).
eval(_,Y,nil,Y).
	
	
evaluate_ident([],_,0).
	
evaluate_ident(Ident,[Ident = Value | _], Value).

evaluate_ident(+Ident,+[_ = _|Rest],-Value):-
	evaluate_ident(+Ident, Rest, -Value).
