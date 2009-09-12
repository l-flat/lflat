
:- object(firstANS,
	instantiates(ans)).

	answer([12, 1, false, true, true]).

:- end_object.



:- object(solANS,
	instantiates(ans)).

	:- initialization((
		contests::setup,
		contests::diagnostics(solANS),
		contests::check_definition(ans, firstANS),
		contests::diagnostics(firstANS),
		contests::test_mechanism(solANS, firstANS),
		contests::finish_checking
	)).

	answer([12, 1, false, true, true]).

:- end_object.
