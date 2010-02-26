:- object(evenFA,
    instantiates(fa)).

	initial(1).

	transitions([1/a/1, 1/b/2, 2/a/2, 2/b/1]).

	finals([1]).

:- end_object.
