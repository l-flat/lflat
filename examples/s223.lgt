

% language w * c * w^-1 with w in {a,b}*

:- object(wcw1L,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b,c]).

	positive([c]).
	positive([a,c,a]).
	positive([b,c,b]).
	positive([a,b,c,b,a]).
	positive([b,a,c,a,b]).

	negative([]).
	negative([a]).
	negative([b]).
	negative([b,c,a]).
	negative([a,c,b]).
	negative([b,a]).
	negative([b,b]).
	negative([a,b,b]).
	negative([a,a,a,a,b,b,b]).

:- end_object.


% Slide 223: change criterion in PDA

:- object(wcw1PDA,
	instantiates(pda)).

	:- initialization((
		write('*** First pushdown automaton of slide 223 ***'), nl,
		::show,
		::diagnostics,
		wcw1L::test_mechanism(wcw1PDA),
		write('*** Changed criterion. New PDA ***'), nl,
		::change_criterion(Wcw1PDA2),
		Wcw1PDA2::show,
		Wcw1PDA2::diagnostics,
		wcw1L::test_mechanism(Wcw1PDA2),
		write('*** Changed the criterion again. New PDA ***'), nl,
		Wcw1PDA2::change_criterion(Wcw1PDA3),
		Wcw1PDA3::show,
		Wcw1PDA3::diagnostics,
		wcw1L::test_mechanism(Wcw1PDA3)
	)).

	initial(i).

	initial_stack_symbol(z).

	transitions([
		i/z/a/i/[a,z],
		i/z/b/i/[b,z],
		i/z/c/q/[z],
		i/a/a/i/[a,a],
		i/a/b/i/[b,a],
		i/a/c/q/[a],
		i/b/a/i/[a,b],
		i/b/b/i/[b,b],
		i/b/c/q/[b],
		q/a/a/q/[],
		q/b/b/q/[],
		q/z/[]/q/[]
		]).

	finals([]).

:- end_object.
