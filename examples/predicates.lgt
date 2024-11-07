
:- object(evenP1,
	instantiates(predicate)).

	:- uses(word, [occurs/3]).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits1).

	accept(Word) :-
		word::word_alphabet(Word, bits1),
		!,
		occurs(1, Word, N), 0 =:= N mod 2.

:- end_object.


:- object(oddP,
	instantiates(predicate)).

	:- uses(word, [occurs/3]).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits1).

	accept(Word) :-
		word::word_alphabet(Word, bits1),
		!,
		occurs(1, Word, N), 0 =\= N mod 2.

:- end_object.
