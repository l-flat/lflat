

% language a^n * b^n with n >= 0

:- object(anbn0L3,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,b]).
	positive([a,a,a,b,b,b]).

	negative([a]).
	negative([b]).
	negative([b,a]).
	negative([b,b]).
	negative([a,b,b]).
	negative([a,a,a,a,b,b,b]).

:- end_object.




% Slide 220: non-deterministic PDA that accepts anbn0L by empty stack

:- object(anbn0_non_det_stack,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 220 ***'), nl,
		::show,
		::diagnostics,
		anbn0L3::test_mechanism(anbn0_non_det_stack)
	)).

	initial(p).

	initial_stack_symbol(z).

	transitions([
		p/z/a/p/[a,z],   % if at least one a, push it
		p/z/[]/p/[],	 % empty stack to accept empty word
		p/a/a/p/[a,a],   % read and push a's
		p/a/b/q/[],	  % the first b, start popping a's
		q/a/b/q/[],	  % pop one a for each read b
		q/z/[]/q/[]	  % end of stack
		]).

	finals([]).

:- end_object.
