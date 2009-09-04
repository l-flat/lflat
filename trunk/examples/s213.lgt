
% language a^n * b^n with n > 0

:- object(anbn1L2,
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


% Slide 213: deterministic PDA that accepts anbn1L by empty stack

:- object(anbn_det_stack,
	instantiates(pda)).

	:- initialization((
		write('*** Pushdown automaton of slide 213 ***'), nl,
		::show,
		::diagnostics,
		anbn1L2::test_mechanism(anbn_det_stack)
	)).

	initial(p).

	initial_stack_symbol(z).

	transitions([
		p/z/a/p/[a,z], p/a/a/p/[a,a], % read and push a's
		p/a/b/q/[],				   % the first b, start popping a's
		q/a/b/q/[],				   % pop one a for each read b
		q/z/[]/q/[]				   % no more b's, empty stack
	]).

	finals([]).

:- end_object.
