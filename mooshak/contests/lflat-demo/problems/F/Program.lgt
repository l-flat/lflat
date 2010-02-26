:- object(paratembFA,
    instantiates(fa)).

	initial(1).

	transitions([
		1/c/1, 1/a/2, 1/b/3, 
		2/c/2, 2/a/1, 2/b/4,
		3/c/3, 3/b/3, 3/a/4,
		4/c/4, 4/b/4, 4/a/3
	]).

	finals([3]).

:- end_object.

