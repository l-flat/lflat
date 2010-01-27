
% Slide 218: non-deterministic PDA that accepts w * w^-1, with w in (a+b)^*

:- object(revL,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,a]).
	positive([a,b,a,a,b,a]).

	negative([a]).
	negative([b]).
	negative([b,a]).
	negative([a,b]).
	negative([a,b,b]).
	negative([a,b,a,b,b,a]).

:- end_object.



:- object(pda218,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 218 ***'), nl,
		::show,
		::diagnostics,
		revL::test_mechanism(pda218)
	)).

	initial(i).

	initial_stack_symbol(z).

	transitions([
		i/z/a/p/[a,z], i/z/b/p/[b,z], % start reading w
		p/a/a/p/[a,a], p/a/b/p/[b,a], % read and push a's and b's
		p/b/a/p/[a,b], p/b/b/p/[b,b], % read and push a's and b's
		p/a/a/q/[], p/b/b/q/[],		  % the symbol read may be the start of w^-1
		q/a/a/q/[], q/b/b/q/[],		  % match the symbols of w^-1 and w
		q/z/[]/t/[]					  % end of stack, go to final state
	]).

	finals([i,t]).

:- end_object.
