
:- object(anbnplusCFG,
	instantiates(cfg)).

	start_symbol('S').

	rules([
		('S'->['T']),
		('S'->['A']),
		('S'->['B']),
		('T'->[a,b]),
		('T'->[a,'T',b]),
		('A'->[]),
		('A'->[a, 'A']),
		('B'->[]),
		('B'->[b, 'B'])
	]).

:- end_object.



:- object(anbnplusL,
	instantiates(language)).

	:- initialization((
		contests::setup,
		contests::diagnostics(anbnplusL),
		contests::check_definition(cfg, anbnplusCFG),
		contests::diagnostics(anbnplusCFG),
		contests::test_mechanism(anbnplusL, anbnplusCFG),
		contests::finish_checking
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,b]).
	positive([a,a,b,b]).
	positive([a,a,a,b,b,b]).
	positive([a]).
	positive([a,a,a,a,a]).
	positive([b,b,b,b,b,b,b]).

	negative([b,a]).
	negative([a,b,a]).
	negative([a,a,b]).
	negative([a,b,b]).

:- end_object.
