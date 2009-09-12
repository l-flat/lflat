
:- object(evenCFG,
	instantiates(cfg)).

	start_symbol('S').

	rules([
		('S'->['a','S']),
		('S'->['b','S','b']),
		('S'->['S','S']),
		('S'->[])
	]).

:- end_object.



:- object(evenL,
    instantiates(language)).

	:- initialization((
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(cfg, evenCFG),
		contests::diagnostics(evenCFG),
		contests::test_mechanism(evenL, evenCFG),
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
