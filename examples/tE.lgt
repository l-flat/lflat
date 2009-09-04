
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



:- object(evenL,
	instantiates(language)).

	:- initialization((
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(tm, evenTM),
		contests::diagnostics(evenTM),
		contests::check_deterministic(evenTM),    
		contests::test_mechanism(evenL, evenTM),  
		contests::finish_checking
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,a,a]).
	positive([b,b]).
	positive([a,b,b]).
	positive([b,b,a,a]).
	positive([b,a,a,a,b,b,a,b]).

	negative([b]).
	negative([b,b,b]).
	negative([b,a,a]).
	negative([a,a,b]).
	negative([b,a,a,b,b,a,a]).

:- end_object.
