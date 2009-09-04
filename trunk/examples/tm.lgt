
% LANG even1 is the set of all the words with a even number of 1s.

:- object(even1,
	instantiates(language)).

	:- initialization((
		write('*** LANG even1 is the set of all the words with a even number of 1s ***'), nl,
		::show,
		::diagnostics
	)).

	alphabet([0,1]).

	positive([]).
	positive([0,0]).
	positive([1,0,1]).
	positive([0,1,1,0,0,1,0,1]).

	negative([0,1,0]).
	negative([1]).

:- end_object.



% TM even1TM that accept words with even number of 1s.

:- object(even1TM,
	instantiates(tm)).

	:- initialization((
		write('*** TM that accept words with even number of 1s ***'), nl,
		::show,
		::diagnostics,
		even1::test_mechanism(even1TM),
		::tracing([0,1,0,1]),
		::tracing([1,0,0])
	)).

	initial(p).

	transitions([
		p/'B'/'B'/'R'/q,
		q/0/0/'R'/q,    q/1/1/'R'/r,    q/'B'/'B'/'R'/f,
		r/0/0/'R'/r,    r/1/1/'R'/q
	]).

	finals([f]).

:- end_object.
