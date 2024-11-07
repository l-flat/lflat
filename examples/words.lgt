
:- object(bits2,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression([0,1]).

:- end_object.



:- object(decimal2,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(bits2 + [2,3,4,5,6,7,8,9]).

:- end_object.



:- object(hex2,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(decimal2 + [a,b,c,d,e,f]).

:- end_object.



:- object(letters2,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(hex2 - decimal2).

:- end_object.



:- object(up,
	instantiates(order)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits2).

	sequence([0,1]).

:- end_object.



:- object(tests_with_words).

	:- initialization((
		write('*** Tests with words ***'), nl, nl,
		test1,
		test2,
		test3,
		test4,
		test5,
		test6,
		test7,
		test8,
		test9
   )).

	test1 :-
		write('** Symbols of hex2:'), nl,
		hex2::symbols(Symbols),
		write(Symbols), nl, nl.

	test2 :-
		write('** compute_word([0,1]^2 * [1,1,0]^(-1)):'), nl,
		word::compute_word([0,1]^2 * [1,1,0]^(-1), Word),
		write(Word), nl, nl.

	test3 :-
		test3x.
	test3 :-
		nl.

	test3x :-
		write('** Some words over the alphabet bits2:'), nl,
		word::word_alphabet(Word, bits2),
		write(Word), nl,
		list::length(Word, N),
		(	N > 4 ->
			!, fail
		;	fail
		).

	test4 :-
		write('** Words less than [1,1,1] in the lexical order:'), nl,
		word::lexically_ordered(Word, [1,1,1], up),
		write(Word), nl,
		fail.
	test4 :- nl.

	test5 :-
		write('** Words less than [1,1,1] in the mixed order:'), nl,
		word::mixed_ordered(Word, [1,1,1], up),
		write(Word), nl,
		fail.
	test5 :- nl.

	test6 :-
		write('** Prefixes of [1,2,3,4,5]:'), nl,
		word::prefix(Word, [1,2,3,4,5]),
		write(Word), nl,
		fail.
	test6 :- nl.

	test7 :-
		write('** Suffixes of [1,2,3,4,5]:'), nl,
		word::suffix(Word, [1,2,3,4,5]),
		write(Word), nl,
		fail.
	test7 :- nl.

	test8 :-
		write('** Subwords of [1,2,3,4,5]:'), nl,
		word::subword(Word, [1,2,3,4,5]),
		write(Word), nl,
		fail.
	test8 :- nl.

	test9 :-
		write('** Word after [1,1,1] in alphabet bits using the up order:'), nl,
		word::next_word([1,1,1], Word, up),
		write(Word), nl,
		fail.
	test9 :- nl.

:- end_object.
