
:- object(aibjambnPDA,
	instantiates(pda)).

	initial(p1).

	initial_stack_symbol(z).

	transitions([
		p1/z/a/p2/[a,z],
		p2/a/a/p2/[a,a], p2/a/b/p3/[],
		p3/a/b/p3/[], p3/z/b/p4/[b,z], p3/z/a/p6/[a,z],
		p4/b/b/p4/[b,b], p4/b/a/p5/[],
		p5/b/a/p5/[], p5/z/a/p6/[a,z],
		p6/a/a/p6/[a,a], p6/a/b/p7/[],
		p7/a/b/p7/[], p7/z/[]/p8/[z]
	]).

	finals([p8]).

:- end_object.



:- object(aibjambnL,
	instantiates(language)).

	:- initialization((
		contests::setup,
		contests::diagnostics(aibjambnL),
		contests::check_definition(pda, aibjambnPDA),
		contests::diagnostics(aibjambnPDA),
		contests::check_deterministic(aibjambnPDA),
		contests::test_mechanism(aibjambnL, aibjambnPDA),
		contests::finish_checking
	)).

	alphabet([a,b]).

	positive([a,b,a,b]).
	positive([a,a,b,b,a,a,b,b]).
	positive([a,b,b,a,a,b]).
	positive([a,b,b,b,a,a,a,b]).
	positive([a,a,b,b,b,a,a,a,b,b]).

	negative([]).
	negative([a]).
	negative([b]).
	negative([a,a,b,a,b,b]).
	negative([b,a,b,a]).

:- end_object.
