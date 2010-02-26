:- object(evenTM,
	instantiates(tm)).

	initial(q0).

	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/a/'R'/q1,     q1/b/b/'R'/q2,    q1/'B'/'B'/'R'/q3,
		q2/a/a/'R'/q2,     q2/b/b/'R'/q1,    q2/'B'/'B'/'R'/q4
	]).

	finals([q3]).

:- end_object.
