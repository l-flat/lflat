
% Slide 311: TM that swaps a's and b's and has no final states

:- object(swapabTM,
	instantiates(tm)).

	:- initialization((
		write('*** Turing machine of slide 311 ***'), nl,
		::show,
		::diagnostics,
		::tracing([a,a,a,b,b,b,a])
	)).

	initial(q0).

	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/b/'R'/q1,	 q1/b/a/'R'/q1,	q1/'B'/'B'/'L'/q2,
		q2/a/a/'L'/q2,	 q2/b/b/'L'/q2
	]).

	finals([]).

:- end_object.
