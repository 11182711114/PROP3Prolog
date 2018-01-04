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

block(block('left_curly', S, 'right_curly')) -->
	['{'],
	stmts(S),
	['}'].
stmts(statements) -->		
	[].
stmts(statements(A, S)) --> 	
	assign(A), 
	stmts(S).
assign(assignment(I, 'assign_op', E, 'semicolon')) --> 	
	id(I), 
	[=], 
	expr(E), 
	[;].
expr(expression(T)) -->		
	term(T).
expr(expression(T, 'add_op', E))	-->		
	term(T), 
	[+], 
	expr(E).
expr(expression(T, 'sub_op', E))	-->		
	term(T), 
	[-], 
	expr(E).
term(term(F)) -->		
	factor(F).
term(term(F, 'mult_op',T)) -->		
	factor(F), 
	[*], 
	term(T).
term(term(F, 'div_op',T)) -->		
	factor(F), 
	[/], 
	term(T).
factor(factor(N)) -->		
	num(N).
factor(factor(I)) -->		
		id(I).
factor(factor('left_paren', E, 'right_paren')) -->		
	['('], 
	expr(E), 
	[')'].
num(int(I)) -->		
	[I], 
	{integer(I)}.
id(ident(I)) -->		
	[I], 
	{atom(I)}.

/***

	
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/
	
/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
evaluate(+block(_,Stmts,_),+VariablesIn,-VariablesOut):-
	evaluate_stmts(Stmts,VariablesIn,VariablesOut).

%% Empty statement -> finished	
evaluate_stmts(+statements,+VariablesIn,-VariablesIn).

evaluate_stmts(statements(Assign,Stmts),VariablesIn,VariablesOut):-
	evaluate_assign(Assign,VariablesIn,VariablesNew),
	evaluate_stmts(Stmts,VariablesNew,VariablesOut).

evaluate_assign(assignment(ident(Ident),'assign_op',Expr,_),VariablesIn,[Ident = Value | VariablesIn]):-
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

eval(X,Y,'add_op',X+Y).
eval(X,Y,'sub_op',X-Y).
eval(X,Y,'mult_op',X*Y).
eval(X,Y,'div_op',X/Y).
eval(_,Y,nil,Y).
	
	
evaluate_ident([],_,0).
	
evaluate_ident(Ident,[Ident = Value | _], Value).

evaluate_ident(+Ident,+[_ = _|Rest],-Value):-
	evaluate_ident(+Ident, Rest, -Value).
