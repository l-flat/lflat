
% Several manipulations involving REs and FAa

% The starting point of these examples is the second RE of slide 100,
%  but the actual manipulations performed in this file are not in the slides. 

:- object(aL,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a]).

	positive([a]).

	negative([]).
	negative([a,a]).
	negative([a,a,a]).

:- end_object.



:- object(aRE,
	instantiates(re)).

	:- initialization((
		write('*** Simple regular expression ***'), nl,
		::show,
		::diagnostics,
		aL::test_mechanism(aRE),

		write('*** Simplify RE ***'), nl,
		::simplify(SRE),
		SRE::show,
		SRE::diagnostics,
		aL::test_mechanism(SRE),

		write('*** Convert RE to minimized finite automata ***'), nl,
		::fa(NDFA),
		NDFA::show,
		write('*** determine ***'), nl,
		NDFA::determine(DFA),
		DFA::show,
		write('*** minimise ***'), nl,
		DFA::minimise(MFA),	
		MFA::show,
		write('*** rename ***'), nl,
		MFA::rename(RMFA),
		RMFA::show,
		RMFA::diagnostics,
		aL::test_mechanism(RMFA)

	)).

	expression({}^* * [] * a * [] * (a * {})^*).

:- end_object.
