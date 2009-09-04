
:- object(priANS,
	instantiates(ans)).

	answer([12, 1, false, true, true]).

:- end_object.



:- object(solANS,
	instantiates(ans)).

	:- initialization((
		contests::setup,
		contests::diagnostics(solANS),
		contests::check_definition(ans, priANS),
		contests::diagnostics(priANS),
		contests::test_mechanism(solANS, priANS),
		contests::finish_checking
	)).

	answer([12, 1, false, true, true]).

:- end_object.
