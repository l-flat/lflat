
% language a^n * b^n with n >= 0

:- object(anbn0L,
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


% language a^n * b^n with n > 0

:- object(anbn1L,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b]).

	positive(Word) :-
		anbn0L::positive(Word),
		Word \== [].

	negative([]).
	negative(Word) :-
		anbn0L::negative(Word),
		Word \== [].

:- end_object.


% Slide 207: deterministic PDA that accepts anbn1L by final state

:- object(anbn_det_final,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 207 ***'), nl,
		::show,
		::diagnostics,
		anbn1L::test_mechanism(anbn_det_final)
	)).

	initial(p).

	initial_stack_symbol(z).

	transitions([
		p/z/a/p/[a,z], p/a/a/p/[a,a], % read and push a's
		p/a/b/q/[],					  % the first b, start popping a's
		q/a/b/q/[],					  % pop one a for each read b
		q/z/[]/t/[]					  % no more b's, go to final state
	]).

	finals([t]).

:- end_object.

/*
test_anbn(W) :-
	pda_accept(anbn(det,final), W).
*/
