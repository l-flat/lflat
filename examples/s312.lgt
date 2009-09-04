
% Slide 312: TM that duplicates word and has no final states

:- object(copyTM,
	instantiates(tm)).

	:- initialization((
		write('*** Turing machine of slide 312 ***'), nl,
		::show,
		::diagnostics,
		::tracing([a,a,a,b,b,b,a])
	)).

	initial(q0).

	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/'X'/'R'/q2,	q1/b/'Y'/'R'/q5,	q1/'B'/'B'/'L'/q7,
		q2/a/a/'R'/q2,		q2/b/b/'R'/q2,		q2/'B'/'B'/'R'/q3,
		q3/a/a/'R'/q3,		q3/b/b/'R'/q3,		q3/'B'/a/'L'/q4,
		q4/a/a/'L'/q4,		q4/b/b/'L'/q4,		q4/'B'/'B'/'L'/q4,
							q4/'X'/'X'/'R'/q1,	q4/'Y'/'Y'/'R'/q1,
		q5/a/a/'R'/q5,		q5/b/b/'R'/q5,		q5/'B'/'B'/'R'/q6,
		q6/a/a/'R'/q6,		q6/b/b/'R'/q6,		q6/'B'/b/'L'/q4,
		q7/'X'/a/'L'/q7,	q7/'Y'/b/'L'/q7
	]).

	finals([]).

:- end_object.
