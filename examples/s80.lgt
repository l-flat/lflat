
% Slide 80: a non-deterministic automaton

:- object(abL,
	instantiates(language)).

	:- initialization((
		 ::show,
		 ::diagnostics
	)).

	alphabet([a,b]).

	positive([a]).
	positive([a,a]).
	positive([a,a,b,b]).
	positive([a,a,a,b]).

	negative([]).
	negative([b,a]).

:- end_object.


:- object(abFA,
	instantiates(fa)).

	:- initialization((
		write('*** Non-deterministic finite automaton of slide 80 ***'), nl,
		::show,
		::diagnostics,
		abL::test_mechanism(abFA)
	)).

	initial(1).

	transitions([
		1/a/1, 1/a/2, 1/b/2,
		2/b/2, 2/b/1
	]).

	finals([2]).

:- end_object.
