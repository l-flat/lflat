
% language a^n * b^n with n >= 0

:- object(anbn0L2,
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


% Slide 214: deterministic PDA that accepts anbn0L by final state

:- object(anbn0_det_final,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 214 ***'), nl,
		::show,
		::diagnostics,
		anbn0L2::test_mechanism(anbn0_det_final)
	)).

	initial(i).

	initial_stack_symbol(z).

	transitions([
		i/z/a/p/[a,z],  % if at least one a, push it
		p/a/a/p/[a,a],  % read and push a's
		p/a/b/q/[],	 % the first b, start popping a's
		q/a/b/q/[],	 % pop one a for each read b
		q/z/[]/t/[]	 % no more b's, go to final state				 
	]).

	finals([i,t]).

:- end_object.
