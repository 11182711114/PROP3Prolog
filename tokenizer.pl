/***
A scanner and tokenizer in SICStus Prolog, which needs to be modified 
to work in other Prolog systems.
Peter Idestam-Almquist, 2015-12-09.
***/

tokenize(File,Tokens):-
	open(File,read,InputStream),
	read_from_file(InputStream,Codes1),
	remove_leading_whitespace(Codes1,Codes2),
	tokenize_codelist(Codes2,CodeLists),
	tokens_from_codelists(CodeLists,Tokens),
	close(InputStream).

read_from_file(InputStream,Lines2):-
	read_line(InputStream,Line), 
	Line \= end_of_file, !,
	read_from_file(InputStream,Lines1), 
	my_append(Line,[32|Lines1],Lines2). /* 32 = ' ' */
read_from_file(_,[]).

remove_leading_whitespace([Code|Cs1],Cs2):-
	whitespace_code(Code), !,
	remove_leading_whitespace(Cs1,Cs2).
remove_leading_whitespace([Code|Cs],[Code|Cs]).
remove_leading_whitespace([],[]).

whitespace_code(9). /* '\t' */
whitespace_code(32). /* ' ' */

tokenize_codelist([],[]).
tokenize_codelist([Code|Cs1],CLs):- 
	whitespace_code(Code), !,
	remove_leading_whitespace(Cs1,Cs2),
	tokenize_codelist(Cs2,CLs).
tokenize_codelist([Code|Cs],[[Code]|CLs]):-
	symbol_code(Code), !,
	tokenize_codelist(Cs,CLs).
tokenize_codelist([Code|Cs1],[[Code|CL]|CLs]):-
	digit_code(Code), !,
	tokenize_codelist_digit(Cs1,CL,Cs2), 
	tokenize_codelist(Cs2,CLs).
tokenize_codelist([Code|Cs1],[[Code|CL]|CLs]):-
	letter_code(Code), !,
	tokenize_codelist_letter(Cs1,CL,Cs2), 
	tokenize_codelist(Cs2,CLs).

tokenize_codelist_digit([Code|Cs1],[Code|CL],Cs2):-
	digit_code(Code), !, 
	tokenize_codelist_digit(Cs1,CL,Cs2).
tokenize_codelist_digit([Code|Cs],[],[Code|Cs]).

tokenize_codelist_letter([Code|Cs1],[Code|CL],Cs2):-
	letter_code(Code), !, 
	tokenize_codelist_letter(Cs1,CL,Cs2).
tokenize_codelist_letter([Code|Cs],[],[Code|Cs]).

symbol_code(40). /* '(' */
symbol_code(41). /* ')' */
symbol_code(42). /* '*' */
symbol_code(43). /* '+' */
symbol_code(45). /* '-' */
symbol_code(47). /* '/' */
symbol_code(59). /* ';' */
symbol_code(61). /* '=' */
symbol_code(123). /* '{' */
symbol_code(125). /* '}' */

digit_code(Code):-
	Code >= 48, /* 48 = '0' */ 
	Code =< 57. /* 57 = '9' */

letter_code(Code):-
	Code >= 97, /* 97 = 'a' */
	Code =< 122. /* 122 = 'z' */

tokens_from_codelists([CodeList|CLs],[Token|Ts]):-
	create_token(CodeList,Token), 
	tokens_from_codelists(CLs,Ts).
tokens_from_codelists([],[]).

create_token([Code|Codes],Number):-
	digit_code(Code), 
	number_codes(Number,[Code|Codes]).
create_token([Code|Codes],Atom):-
	\+ digit_code(Code), 
	atom_codes(Atom,[Code|Codes]).

/***
We use the name my_append/3 so it will not clash 
with any append/3 built-in predicate.
***/
my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]):- my_append(Xs,Ys,Zs).
