% Slide 82: a non-deterministic automaton with lambda transitions

:- object(abL82,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b]).

	positive([a]).
	positive([b]).
	positive([a,a]).
	positive([a,a,b,b]).
	positive([a,b]).
	positive([b,b]).
	positive([b,b,a]).

	negative([]).
	negative([b,a]).

	negative([b,b,b,b,b]).	% this should give an error on the test mechanism of abFA82 below


:- end_object.


:- object(abFA82,
	instantiates(fa)).

	:- initialization((
		write('*** Non-deterministic finite automaton of slide 82 ***'), nl,
		::show,
		::diagnostics,
		abL82::test_mechanism(abFA82)
	)).

	initial(1).

	transitions([
		1/a/1,
		1/a/2,
		1/b/2,
		2/b/2,
		2/b/1,
		3/a/2,
		3/b/2,
		3/a/1,
		1/[]/3
	]).

	finals([2]).

:- end_object.
