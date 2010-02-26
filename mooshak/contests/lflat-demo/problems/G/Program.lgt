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
