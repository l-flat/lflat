
:- object(evenP,
	instantiates(predicate)).

	:- uses(word, [occurs/3]).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits).

	accept(Word) :-
		word::word_alphabet(Word, bits) ->
		occurs(1, Word, N), 0 =:= N mod 2.

:- end_object.


:- object(oddP,
	instantiates(predicate)).

	:- uses(word, [occurs/3]).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet(bits).

	accept(Word) :-
		word::word_alphabet(Word, bits) ->
		occurs(1, Word, N), 0 =\= N mod 2.

:- end_object.
