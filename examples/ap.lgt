
% Slide 207: deterministic PDA that accepts anbn1L by final state

:- object(ab,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 207 ***'), nl,
		::show,
		::diagnostics,
		::tracing([a,b]),
		::tracing([a,a,b])
	)).

	initial(1).

	initial_stack_symbol(z).

	transitions([
		1/z/a/2/[z],
		2/z/b/3/[z]
	]).

	finals([3]).

:- end_object.
