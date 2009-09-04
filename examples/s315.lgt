
% Slide 315: TM that that accepts a^i * b^i * c^i with i >= 0

:- object(aibiciL,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b,c]).

	positive([]).
	positive([a,b,c]).
	positive([a,a,b,b,c,c]).

	negative([a]).
	negative([a,b]).
	negative([b,a]).
	negative([c]).
	negative([a,c,b]).
	negative([a,a,a,a,b,b,b]).

:- end_object.



:- object(aibiciTM,
	instantiates(tm)).

	:- initialization((
		write('*** Turing machine of slide 315 ***'), nl,
		::show,
		::diagnostics,
		aibiciL::test_mechanism(aibiciTM),
		::tracing([a,a,a,b,b,b,c,c,c])
	)).

	initial(q0).

	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/'X'/'R'/q2,	q1/'Y'/'Y'/'R'/q5,	q1/'B'/'B'/'R'/q6,
		q2/a/a/'R'/q2,		q2/'Y'/'Y'/'R'/q2,	q2/b/'Y'/'R'/q3,
		q3/b/b/'R'/q3,		q3/'Z'/'Z'/'R'/q3,	q3/c/'Z'/'L'/q4,
		q4/a/a/'L'/q4,		q4/b/b/'L'/q4,		q4/'Y'/'Y'/'L'/q4,
		q4/'Z'/'Z'/'L'/q4,	q4/'X'/'X'/'R'/q1,
		q5/'Y'/'Y'/'R'/q5,	q5/'Z'/'Z'/'R'/q5,	q5/'B'/'B'/'R'/q6
	]).

	finals([q6]).

:- end_object.
