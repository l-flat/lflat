
:- object(bits,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression([0,1]).

:- end_object.



:- object(decimal,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(bits + [2,3,4,5,6,7,8,9]).

:- end_object.



:- object(hex,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(decimal + [a,b,c,d,e,f]).

:- end_object.



:- object(letters,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(hex - decimal).

:- end_object.



:- object(up,
	instantiates(order)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits).

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
		write('** Symbols of hex:'), nl,
		hex::symbols(Symbols),
		write(Symbols), nl, nl.

	test2 :-
		write('** compute_word([0,1]^2 * [1,1,0]^(-1)):'), nl,
		word::compute_word([0,1]^2 * [1,1,0]^(-1), Word),
		write(Word), nl, nl.

	test3 :-
		test3x; nl.
	test3x :-
		write('** Some words over the alphabet bits:'), nl,
		word::word_alphabet(Word, bits),
		write(Word), nl,
		list::length(Word, N), (N > 4 -> !),
		fail.

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
