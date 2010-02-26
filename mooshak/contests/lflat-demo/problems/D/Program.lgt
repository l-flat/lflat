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


