
:- object(evenPDA,
	instantiates(pda)).

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



:- object(evenL,
	instantiates(language)).

	:- initialization((
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(pda, evenPDA),
		contests::diagnostics(evenPDA),
		contests::check_deterministic(evenPDA),
		contests::test_mechanism(evenL, evenPDA),
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
