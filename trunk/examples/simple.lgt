
% This example introduces a language using positive and negative unit-tests.
% After that, 6 diferent mechanisms (recognizers and generators) are
% defined for that language and are checked againt it.



:- object(evenL,	% All sequences of as and bs with an even number of bs
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics,
		nl, nl
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,a,a]).
	positive([b,b]).
	positive([a,b,b]).
	positive([b,b,a,a]).
	positive([b,a,a,a,b,b,a,b]).

	negative([b]).
	negative([b,b,b]).
	negative([b,a,a]).
	negative([a,a,b]).
	negative([b,a,a,b,b,a,a]).

:- end_object.



:- object(evenP,			% Predicate for evenL
	instantiates(predicate)).

	:- uses(word, [occurs/3]).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenP),
		nl, nl
 
	)).

	alphabet([a,b]).

	accept(Word) :-
		word::word_alphabet(Word, [a,b]) ->
		occurs(b, Word, N), 0 =:= N mod 2.

:- end_object.



:- object(evenRE,			% Regular expression for evenL
	instantiates(re)).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenRE),
		nl, nl
	)).

	expression((a + b * a^* * b)^*).

:- end_object.



:- object(evenFA,			% Finite automaton for evenL
	instantiates(fa)).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenFA),
		nl, nl
	)).

	initial(1).
	transitions([1/a/1, 1/b/2, 2/a/2, 2/b/1]).
	finals([1]).

:- end_object.



:- object(evenCFG,			% Context-free grammar for evenL
	instantiates(cfg)).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenCFG),
		nl, nl
	)).

	start_symbol('S').
	rules([
		('S'->['a','S']),
		('S'->['b','S','b']),
		('S'->['S','S']),
		('S'->[])
	]).

:- end_object.




:- object(evenPDA,			% Pushdown automaton for evenL
	instantiates(pda)).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenPDA),
		nl, nl
	)).

	initial(p).
	initial_stack_symbol(z).
	transitions([
		p/z/a/p/[z],
		p/z/b/q/[z],
		q/z/a/q/[z],
		q/z/b/p/[z]
	]).
	finals([p]).

:- end_object.




:- object(evenTM,			% Turing machine for evenL
	instantiates(tm)).

	:- initialization((
		::show,
		::diagnostics,
		evenL::test_mechanism(evenTM),
		nl, nl
	)).

	initial(q0).
	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/a/'R'/q1, q1/b/b/'R'/q2, q1/'B'/'B'/'R'/q3,
		q2/a/a/'R'/q2, q2/b/b/'R'/q1, q2/'B'/'B'/'R'/q4
	]).
	finals([q3]).

:- end_object.
