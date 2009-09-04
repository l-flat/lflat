
:- object(evenFA,
    instantiates(fa)).

	initial(1).

	transitions([1/a/1, 1/b/2, 2/a/2, 2/b/1]).

	finals([1]).

:- end_object.



:- object(evenL,
    instantiates(language)).

    :- initialization((
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(fa, evenFA),
		contests::diagnostics(evenFA),
		contests::check_deterministic(evenFA),
		contests::test_mechanism(evenL, evenFA),
		contests::finish_checking
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
